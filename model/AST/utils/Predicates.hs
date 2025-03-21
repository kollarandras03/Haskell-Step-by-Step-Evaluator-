module Model.AST.Utils.Predicates where
import Model.AST.AST

-------- DECL ---------------------------------------------------------
isFunBind (FunBind {}) = True
isFunBind _ = False

isPatBind (PatBind {}) = True
isPatBind _ = False

isTypeSig (TypeSig {}) = True
isTypeSig _ = False
-----------------------------------------------------------------------
unParen = noParen id 

noParen f (Paren _ e) = noParen f e
noParen f e = f e

isSpecialCon (Con _ (Special (Cons {}))) = True
isSpecialCon _ = False

isTupleCon (Con _ (Special (TupleCon {}))) = True
isTupleCon _ = False

isInfixApp (InfixApp {}) = True
isInfixApp _ = False

isApp (App {}) = True
isApp _ = False

isVar (Var {}) = True
isVar _ = False

isCon (Con {}) = True
isCon _ = False

isLit (Lit {}) = True
isLit _ = False

isNegApp (NegApp {}) = True
isNegApp _ = False

isTuple (Tuple {}) = True
isTuple _ = False

isList (List {}) = True
isList _ = False

isLambda (Lambda {}) = True
isLambda _ = False

