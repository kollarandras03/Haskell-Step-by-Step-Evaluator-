module Model.AST.PrettyPrint(PrettyPrint(..)) where
import Model.AST.Convertable
import Model.AST.Type
import Model.AST.Literal
import Model.AST.Names
import Model.AST.InternalTypes
import Model.AST.Pat
import Model.AST.Core 
import Model.AST.Build
import Model.AST.ASTInfo 
import Model.AST.Module
import Data.List(intercalate)
import qualified Language.Haskell.Exts as E

-- Pretty printing module, for regular reading

paren :: String -> String
paren s = "(" <> s <> ")"

backTick :: String -> String
backTick s = "`" <> s <> "`"

sepW :: String -> [String] -> String
sepW s ss = intercalate s ss

class PrettyPrint a where
    prettyPrint :: a -> String

instance PrettyPrint Literal where
    prettyPrint (Char c) = show c
    prettyPrint (String s) = show s
    prettyPrint (Frac f) = show f
    prettyPrint (Whole i) = show i
    prettyPrint (Double d) = show d
    prettyPrint (Float f) = show f
    prettyPrint (Int i) = show i
    prettyPrint (Integer i) = show i

instance PrettyPrint QName where
    prettyPrint (Qual m n) = prettyPrint m <> "." <> prettyPrint n
    prettyPrint (UnQual n) = prettyPrint n
    prettyPrint (Special s) = prettyPrint s

instance PrettyPrint SpecialCon where
    prettyPrint UnitCon = "()"
    prettyPrint Cons = "(:)"
    prettyPrint (TupleCon i) = "(" <> replicate (i-1) ',' <> ")"
    prettyPrint ListCon = "[]"

instance PrettyPrint Name where
    prettyPrint (Ident s) = s
    prettyPrint (Symbol s) = paren s

instance PrettyPrint ModuleName where
    prettyPrint (ModuleName s) = s

showInfixQN :: QName -> String
showInfixQN (Special Cons) = ":"
showInfixQN (Special sc) = backTick (prettyPrint sc)
showInfixQN (Qual mn n) = prettyPrint mn <> "." <> showInfixN n
showInfixQN (UnQual n) = showInfixN n

showInfixN :: Name -> String
showInfixN (Ident s) = backTick s
showInfixN (Symbol s) = s

instance PrettyPrint QOp where
    prettyPrint (QVarOp _ qn) = showInfixQN qn
    prettyPrint (QConOp _ qn) = showInfixQN qn 



instance PrettyPrint Stmt where
    prettyPrint (Qualifier _ e) = prettyPrint e

instance PrettyPrint [Stmt] where
    prettyPrint stmts = sepW ", " $ map prettyPrint stmts

pparen :: Pat -> Pat
pparen p = PParen p

asEmbedPat :: Pat -> Pat
asEmbedPat p@(PVar _ _) = p
asEmbedPat p@(PLit _ Pos _) = p
asEmbedPat p@(PParen _) = p
asEmbedPat p@(PLit _ Neg _) = pparen p
asEmbedPat p@(PApp {}) = pparen p
asEmbedPat p@(PInfix {}) = pparen p
asEmbedPat p@(PList {}) = p
asEmbedPat p@(PTuple {}) = p
asEmbedPat p@(PWildCard _) = p

instance PrettyPrint Pat where
    prettyPrint (PVar _ n) = prettyPrint n
    prettyPrint (PLit _ Pos l) = prettyPrint l
    prettyPrint (PLit _ Neg l) = "-" <> prettyPrint l
    prettyPrint (PInfix _ p1 qn p2) = prettyPrint p1 <> showInfixQN qn <> prettyPrint p2 
    prettyPrint (PWildCard _) = "_"
    prettyPrint (PApp _ n []) = prettyPrint n
    prettyPrint (PApp _ n ps) = prettyPrint n <> " " <> sepW " " (map (prettyPrint . asEmbedPat) ps)
    prettyPrint (PList _ ps) = "[" <> sepW ", " (map prettyPrint ps) <> "]"
    prettyPrint (PTuple _ ps) = "(" <> sepW ", " (map prettyPrint ps) <> ")"
    prettyPrint (PParen p) = paren (prettyPrint p)
    prettyPrint (PAsPat _ n p) = prettyPrint n <> "@" <> prettyPrint (asEmbedPat p)

eparen :: Exp -> Exp
eparen e = Paren undefined e 

asEmbedExp :: Exp -> Exp
asEmbedExp e = case e of
    NegApp {} -> eparen e
    InfixApp {} -> eparen e
    Lambda {} -> eparen e
    App {} -> eparen e
    Let {} -> eparen e
    _ -> e


instance PrettyPrint Exp where
    prettyPrint (Var _ n) = prettyPrint n
    prettyPrint (Con _ n) = prettyPrint n
    prettyPrint (Lit _ l) = prettyPrint l
    prettyPrint (InfixApp _ e1 op e2) = sepW " " [prettyPrint (asEmbedExp e1), prettyPrint op, prettyPrint (asEmbedExp e2)]
    prettyPrint (NegApp _ e) = "-" <> prettyPrint e
    prettyPrint (Paren _ e) = paren (prettyPrint e)
    prettyPrint (Tuple s es) = paren (sepW ", " (map prettyPrint es))
    prettyPrint (List s es) = "[" <> sepW ", " (map prettyPrint es) <> "]"
    prettyPrint (Lambda s ps e) = "\\" <> sepW " " (map prettyPrint ps) <> " -> " <> prettyPrint e
    prettyPrint (Let _ bs e) = "let " <> prettyPrint bs <> " in " <> prettyPrint e
    prettyPrint (App _ e1 e2) = sepW " " [prettyPrint e1, prettyPrint e2]
    prettyPrint (ExpTypeSig e t) = prettyPrint e <> " :: " <> prettyPrint t 

instance PrettyPrint Binds where
    prettyPrint (BDecls ds) = sepW ";" $ map prettyPrint ds

instance PrettyPrint Decl where
    prettyPrint (FunBind _ ms) = sepW ";" $ map prettyPrint ms
    prettyPrint (PatBind _ p rhs Nothing) = prettyPrint p <> prettyPrint rhs
    prettyPrint (PatBind _ p rhs (Just bs)) = sepW " " [prettyPrint p, prettyPrint rhs] <> " where {" <> prettyPrint bs <> "}"
    prettyPrint (TypeSig ns t) = sepW ", " (map prettyPrint ns) <> " :: " <> E.prettyPrint t

instance PrettyPrint GuardedRhs where
    prettyPrint (GuardedRhs _ stmts e) = "| " <> prettyPrint stmts <> " = " <> prettyPrint e

instance PrettyPrint Rhs where
    prettyPrint (UnGuardedRhs _ e) = " = " <> prettyPrint e
    prettyPrint (GuardedRhss _ grhss) = sepW " " $ map prettyPrint grhss

instance PrettyPrint Match where
    prettyPrint (Match _ n ps rhs Nothing) =  prettyPrint n <> " " <> sepW " " (map prettyPrint ps) <> prettyPrint rhs
    prettyPrint (Match _ n ps rhs (Just bs)) = prettyPrint n <> " " <> sepW " " (map prettyPrint ps) <> prettyPrint rhs <> " where {" <> prettyPrint bs <> "}"

instance PrettyPrint EType where
    prettyPrint = E.prettyPrint