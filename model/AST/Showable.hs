module Model.AST.Showable(asInfixN,asPrefixN) where
import Model.AST.Core
import Model.AST.InternalTypes
import Model.AST.Literal
import Model.AST.Names
import Model.AST.ASTInfo
import Model.AST.Pat
import Model.AST.Type
import Model.AST.Show
import Data.List(intercalate)
import qualified Data.Set as Set
import Control.Concurrent (threadDelay)

{-
-- This class is a prettyPrint, but also shows toplevel, or all infos
class Showable2 a where
    -- Nothing is shown, basically a prettyprint
    showNo :: PrettyPrint a => a -> String
    showNo = prettyPrint

    -- Shows only the toplevel scopes
    showTop :: PrettyPrint a => a -> String

    -- Shows the scopes of the in-scope nodes as well 
    showAll :: PrettyPrint a => a -> String
    {-#MINIMAL showTop, showAll #-}
-}






class Showable a where
    -- Printing the AST with the ASTInfo
    prettyPrintT :: a -> String
    prettyPrintT = prettyPrint
    -- Printing the AST without the ASTInfo
    prettyPrint :: a -> String
    {-# MINIMAL prettyPrint #-}



-----------------------------------------------------------------------------------

instance Showable ExpInfoSc where
    prettyPrint (ExpInfoS s _ Nothing) = show s
    prettyPrint (ExpInfoS s _ (Just sc)) = "{" <> show s <> prettyPrint sc <> "}"
    prettyPrint (ExpInfo s _) = show s
    prettyPrint (State st) = show st

instance Showable Scope where
    prettyPrint ds = intercalate "; " $ map prettyPrint ds
    prettyPrintT ds = intercalate "; " $ map prettyPrintT ds

asPrefixN :: Name -> String
asPrefixN (Ident s) = s
asPrefixN (Symbol s) = "(" <> s <> ")"

asInfixN :: Name -> String
asInfixN (Ident s) = "`" <> s <> "`"
asInfixN (Symbol s) = s

instance Showable StmtState where
    prettyPrint s = show s <> " "

------------------------------------------------------------

instance Showable QName where
    prettyPrint (Qual m n) = prettyPrint m <> "." <> prettyPrint n
    prettyPrint (UnQual n) = prettyPrint n
    prettyPrint (Special s) = prettyPrint s

instance Showable Literal where
    prettyPrint (Char c) = show c
    prettyPrint (String s) = show s
    prettyPrint (Frac f) = show f
    prettyPrint (Whole i) = show i
    prettyPrint (Double d) = show d
    prettyPrint (Float f) = show f
    prettyPrint (Int i) = show i
    prettyPrint (Integer i) = show i

instance Showable SpecialCon where
    prettyPrint UnitCon = "()"
    prettyPrint Cons = ":"
    prettyPrint (TupleCon i) = "(" <> replicate (i-1) ',' <> ")"
    prettyPrint ListCon = "[]"

instance Showable Name where
    prettyPrint (Ident s) = s
    prettyPrint (Symbol s) = s

instance Showable ModuleName where
    prettyPrint (ModuleName s) = s

instance Showable QOp where
    prettyPrint (QVarOp s qn) = prettyPr' qn <> prettyPrint s where
        prettyPr' (Qual mn (Ident s)) = "`" <> prettyPrint mn <> "." <> s <> "`" 
        prettyPr' (Qual mn (Symbol s)) = prettyPrint mn <> "." <> s
        prettyPr' (UnQual (Ident s)) = "`" <> s <> "`" 
        prettyPr' (UnQual (Symbol s)) = s 
        prettyPr' (Special Cons) = ":"
        prettyPr' s = prettyPrint s
        


    prettyPrint (QConOp s n) = "`" <> prettyPrint n <> "`" <> prettyPrint s

    prettyPrintT (QVarOp s n) = "`" <> prettyPrintT n <> "`" <> prettyPrintT s
    prettyPrintT (QConOp s n) = prettyPrintT n <> prettyPrintT s

instance Showable Stmt where
    prettyPrint (Qualifier s e) = prettyPrint s <> prettyPrint e
    prettyPrintT (Qualifier s e) = prettyPrintT e

instance Showable [Stmt] where
    prettyPrint stmts = intercalate ", " $ map prettyPrint stmts
    prettyPrintT stmts = intercalate ", " $ map prettyPrintT stmts

instance Showable Pat where
    prettyPrint (PVar _ n) = prettyPrint n
    prettyPrint (PLit _ _ l) = prettyPrint l
    prettyPrint (PInfix _ p1 n p2) 
        | Special _ <- n = prettyPrint p1 <> " " <> prettyPrint n <> " " <> prettyPrint p2
        | otherwise = prettyPrint p1 <> " `" <> prettyPrint n <> "` " <> prettyPrint p2
    
    prettyPrint (PApp _ n ps) 
        | Special _ <- n = "(" <> prettyPrint n <> ") " <> intercalate " " (map prettyPrint ps)
        | otherwise = prettyPrint n <> " " <> intercalate " " (map prettyPrint ps)
    
    prettyPrint (PTuple _ ps) = "(" <> intercalate ", " (map prettyPrint ps) <> ")"
    prettyPrint (PList _ ps) = "[" <> intercalate ", " (map prettyPrint ps) <> "]"
    prettyPrint (PParen p) = "(" <> prettyPrint p <> ")"
    prettyPrint (PWildCard _) = "_"
    prettyPrint (PAsPat (PatInfo _ t) n p) = prettyPrint n <> "@" <> prettyPrint p
    prettyPrint (PIrrPat p) = "~" <> prettyPrint p


instance Showable Exp where
    prettyPrint (Var s n) = prettyPrint n <> "" <> prettyPrint s
    prettyPrint (Con s n) = prettyPrint n <> "" <> prettyPrint s
    prettyPrint (Lit s l) = prettyPrint l <> "" <> prettyPrint s
    
    prettyPrint (InfixApp s e1 op e2) 
        | QVarOp {} <- op = prettyPrint e1 <> " " <> prettyPrint op <> " " <> prettyPrint e2
        | otherwise = prettyPrint e1 <> " " <> prettyPrint op <> " " <> prettyPrint e2
    prettyPrint (App s e1 e2)
        | Var _ (UnQual (Symbol _)) <- e1 = "(" <> prettyPrint e1 <> ") " <> prettyPrint e2 
        | Var _ (Qual _ (Symbol _)) <- e1 = "(" <> prettyPrint e1 <> ") " <> prettyPrint e2 
        | otherwise = prettyPrint e1 <> " " <> prettyPrint e2 <> ""
    
    prettyPrint (NegApp s e) = "-" <> prettyPrint e
    prettyPrint (Lambda s ps e) = "\\" <> intercalate " " (map prettyPrint ps) <> " -> " <> prettyPrint e
    prettyPrint (Let s bs e) = "let " <> prettyPrint bs <> " in " <> prettyPrint e 
    prettyPrint (Tuple s es) = "(" <> intercalate ", " (map prettyPrint es) <> ")"
    prettyPrint (List s es) = "[" <> intercalate ", " (map prettyPrint es) <> "]"
    prettyPrint (Paren s e) = "(" <> prettyPrint e <> ")"
 

instance Showable Binds where
    prettyPrint (BDecls ds) = intercalate "; " $ map prettyPrint ds
    prettyPrintT (BDecls ds) = intercalate "; " $ map prettyPrintT ds

instance Showable Decl where
    prettyPrint (FunBind _ ms) = intercalate "; " $ map prettyPrint ms
    prettyPrint (PatBind _ p rhs Nothing) = prettyPrint p <> prettyPrint rhs
    prettyPrint (PatBind _ p rhs (Just bs)) = prettyPrint p <> prettyPrint rhs <> " where {" <> prettyPrint bs <> "}"

    prettyPrintT (FunBind _ ms) = intercalate "; " $ map prettyPrintT ms
    prettyPrintT (PatBind _ p rhs Nothing) = prettyPrintT p <> prettyPrintT rhs
    prettyPrintT (PatBind _ p rhs (Just bs)) = prettyPrintT p <> prettyPrintT rhs <> " where {" <> prettyPrintT bs <> "}"


instance Showable GuardedRhs where
    prettyPrint (GuardedRhs s stmts e) = " | " <> prettyPrint stmts <> " = " <> prettyPrint e
    prettyPrintT (GuardedRhs s stmts e) = " | " <> prettyPrintT stmts <> " = " <> prettyPrintT e

instance Showable Rhs where
    prettyPrint (UnGuardedRhs _ e) = " = " <> prettyPrint e
    prettyPrint (GuardedRhss _ grhss) = intercalate " " $ map prettyPrint grhss

    prettyPrintT (UnGuardedRhs _ e) = " = " <> prettyPrintT e
    prettyPrintT (GuardedRhss _ grhss) = intercalate " " $ map prettyPrintT grhss

instance Showable Match where
    prettyPrint (Match _ n ps rhs Nothing) =  asPrefixN n <> " " <> intercalate " " (map prettyPrint ps) <> prettyPrint rhs
    prettyPrint (Match _ n ps rhs (Just bs)) = asPrefixN n <> " " <> intercalate " " (map prettyPrint ps) <> prettyPrint rhs <> " where {" <> prettyPrint bs <> "}"
    
    prettyPrintT (Match _ n ps rhs Nothing) = asPrefixN n <> " " <> intercalate " " (map prettyPrintT ps) <> prettyPrintT rhs
    prettyPrintT (Match _ n ps rhs (Just bs)) = asPrefixN n <> " " <> intercalate " " (map prettyPrintT ps) <> prettyPrintT rhs <> " where {" <> prettyPrintT bs <> "}"
