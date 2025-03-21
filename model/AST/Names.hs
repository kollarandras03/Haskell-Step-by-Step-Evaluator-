{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Model.AST.Names where
import qualified Language.Haskell.Exts as E
import Model.AST.Convertable
import Model.Error

newtype ModuleName = ModuleName String deriving (Eq)

instance Convertable (E.ModuleName ()) ModuleName where
    convert :: E.ModuleName () -> Either Error ModuleName
    convert (E.ModuleName _ s) = Right $ ModuleName s

    revert :: ModuleName -> Either Error (E.ModuleName ())
    revert (ModuleName s) = Right $ E.ModuleName () s

--------------------------------------------

data QName = UnQual  Name
           | Qual    ModuleName Name
           | Special SpecialCon deriving (Eq)

instance Convertable (E.QName ()) QName where
    convert :: E.QName () -> Either Error QName
    convert (E.UnQual _ n) = UnQual <$> (convert n)
    convert (E.Qual _ m n) = Qual <$> (convert m) <*> (convert n)
    convert (E.Special _ s) = Special <$> (convert s)

    revert :: QName -> Either Error (E.QName ())
    revert (UnQual n) = fmap (E.UnQual ()) (revert n)
    revert (Qual m n) = (E.Qual ()) <$> (revert m) <*> (revert n)
    revert (Special s) = fmap (E.Special ()) (revert s)

--------------------------------------------

data SpecialCon = UnitCon
                | Cons
                | ListCon
                | TupleCon Int deriving (Eq)

instance Convertable (E.SpecialCon ()) SpecialCon where
    convert :: E.SpecialCon () -> Either Error SpecialCon
    convert (E.UnitCon _) = Right UnitCon
    convert (E.Cons _) = Right Cons
    convert (E.TupleCon _ _ i) = Right $ TupleCon i
    convert (E.ListCon _) = Right ListCon

    convert (E.UnboxedSingleCon _) = Left $ NotSupported
    convert (E.ExprHole _) = Left $ NotSupported
    convert (E.FunCon _) = Left $ NotSupported

    revert :: SpecialCon -> Either Error (E.SpecialCon ())
    revert UnitCon = Right $ E.UnitCon ()
    revert Cons = Right $ E.Cons ()
    revert ListCon = Right $ E.ListCon ()
    revert (TupleCon i) = Right $ E.TupleCon () E.Boxed i

--------------------------------------------

data Name = Ident  String
          | Symbol String deriving (Eq)

instance Convertable (E.Name ()) Name where
    convert :: E.Name () -> Either Error Name
    convert (E.Ident _ s) = Right $ Ident s
    convert (E.Symbol _ s) = Right $ Symbol s

    revert :: Name -> Either Error (E.Name ())
    revert (Ident s) = Right $ E.Ident () s
    revert (Symbol s) = Right $ E.Symbol () s

--------------------------------------------

data Op = VarOp Name 
        | ConOp Name deriving Eq

instance Convertable (E.Op ()) Op where
    convert :: E.Op () -> Either Error Op
    convert (E.VarOp _ n) = fmap VarOp (convert n)
    convert (E.ConOp _ n) = fmap ConOp (convert n)

    revert :: Op -> Either Error (E.Op ())
    revert (VarOp n) = fmap (E.VarOp ()) (revert n)
    revert (ConOp n) = fmap (E.ConOp ()) (revert n)

--------------------------------------------