module Model.AST.Build where
import Model.AST.Type
import Model.AST.Literal
import Model.AST.Convertable
import Model.AST.Core
import Model.AST.Names
import Model.AST.InternalTypes
import Model.AST.ASTInfo
import Model.AST.Show
import qualified Language.Haskell.Exts as E
import qualified Data.Set as Set

intT, integerT, stringT, charT, doubleT, floatT, boolT, unitT :: Type
intT     = TyCon $ UnQual $ Ident "Int"
integerT = TyCon $ UnQual $ Ident "Integer"
stringT  = TyCon $ UnQual $ Ident "String"
charT    = TyCon $ UnQual $ Ident "Char"
doubleT  = TyCon $ UnQual $ Ident "Double"
floatT   = TyCon $ UnQual $ Ident "Float"
boolT    = TyCon $ UnQual $ Ident "Bool"
unitT    = TyCon $ Special $ UnitCon
aT cns   = TyVar "a" (Set.fromList cns)

listT :: Type -> Type
listT t = TyList t

tupleT :: [Type] -> Type
tupleT ts = TyTup ts

-- Expressions

defaultScope t = ExpInfo NoChange t

trueC  = Con (defaultScope $ Just boolT) $ UnQual $ Ident "True"
falseC = Con (defaultScope $ Just boolT) $ UnQual $ Ident "False"