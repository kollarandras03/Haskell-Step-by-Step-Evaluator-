{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Model.AST.Type(Constraint(..), Type(..),Constraints,EType) where
import qualified Data.Set as Set
import Model.AST.Names(QName)
import qualified Language.Haskell.Exts as E
import Model.AST.Convertable
import Model.Error
import Text.Read

type EType = E.Type ()

data Type
  = TyVar String Constraints
  | TyCon QName 
  | TyTup [Type]
  | TyList Type
  | TyParen Type
  | TyFun Type Type deriving Eq


-- All the possible constraints that can be applied to a type
data Constraint 
  = Eq 
  | Ord 
  | Num 
  | Real 
  | Show 
  | Read 
  | Enum 
  | Bounded
  | Integral 
  | RealFrac
  | RealFloat
  | Semigroup
  | Monoid 
  | Fractional
  | Floating deriving (Eq, Show, Ord, Read)

type Constraints = Set.Set Constraint


instance Convertable EType Type where
  revert :: Type -> Either Error EType
  revert _ = Left $ NotSupported

  convert :: EType -> Either Error Type
  convert (E.TyStar _) = Left NotImplemented
  convert (E.TyUnboxedSum _ _) = Left NotImplemented
  convert (E.TyPromoted {}) = Left NotImplemented
  convert (E.TyInfix _ _ _ _) = Left NotImplemented
  convert (E.TyKind _ _ _) = Left NotImplemented
  convert (E.TyBang _ _ _ _) = Left NotImplemented
  convert (E.TyEquals _ _ _) = Left NotImplemented
  convert (E.TySplice _ _) = Left NotImplemented
  convert (E.TyWildCard _ _) = Left NotImplemented
  convert (E.TyQuasiQuote _ _ _) = Left NotImplemented
  convert (E.TyVar _ (E.Symbol _ _)) = Left NotImplemented
  convert (E.TyParArray _ _) = Left NotImplemented
  convert (E.TyApp _ _ _) = Left NotImplemented

  convert (E.TyVar _ (E.Ident _ str)) = Right $ TyVar str Set.empty
  convert (E.TyCon _ qn) = TyCon <$> (convert qn)
  convert (E.TyParen _ ty) = TyParen <$> (convert ty) 
  convert (E.TyFun _ t1 t2) =  TyFun <$> (convert t1) <*> (convert t2)
  convert (E.TyList _ t) = TyList <$> (convert t)
  convert (E.TyTuple _ _ ts) = TyTup <$> traverse convert ts
  convert (E.TyForall _ _ Nothing t) = convert t
  convert (E.TyForall _ _ (Just cont) t) = result where
    ty = convert t :: Either Error Type
    constraints = convertContext cont
    result = (\tt -> foldr (\(varName, constraint) ty' -> applyConstraints varName (Set.singleton constraint) ty') tt constraints) <$> ty
    

    convertContext :: E.Context () -> [(String, Constraint)]
    convertContext (E.CxSingle _ asst) = convertAsst asst
    convertContext (E.CxTuple _ assts) = concatMap convertAsst assts
    convertContext _ = []

    convertAsst :: E.Asst () -> [(String, Constraint)]
    convertAsst (E.TypeA _ (E.TyApp _ (E.TyCon _ (E.UnQual _ (E.Ident _ className))) (E.TyVar _ (E.Ident _ varName)))) =
      case readMaybe className of
          Just constraint -> [(varName, constraint)]
          Nothing -> []
    convertAsst (E.ParenA _ asst) = convertAsst asst
    convertAsst _ = []

applyConstraints :: String -> Constraints -> Type -> Type
applyConstraints varName constraints (TyVar name existingConstraints)
  | name == varName = TyVar name (Set.union existingConstraints constraints)
applyConstraints varName constraints (TyFun t1 t2) =
  TyFun (applyConstraints varName constraints t1) (applyConstraints varName constraints t2)
applyConstraints varName constraints (TyList ty) =
  TyList (applyConstraints varName constraints ty)
applyConstraints varName constraints (TyTup tys) =
  TyTup (map (applyConstraints varName constraints) tys)
applyConstraints _ _ ty = ty