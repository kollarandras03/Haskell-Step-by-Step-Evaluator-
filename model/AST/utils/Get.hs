module Model.AST.Utils.Get where
import Model.AST.AST
import Model.AST.Utils.FlattenPat
import Data.Maybe


getI :: Exp -> ExpInfoSc
getI (Var i _) = i
getI (Con i _) = i
getI (Lit i _) = i
getI (InfixApp i _ _ _) = i
getI (App i _ _) = i
getI (NegApp i _) = i
getI (Lambda i _ _) = i
getI (Let i _ _) = i
getI (Tuple i _) = i
getI (List i _) = i
getI (Paren i e) = i -- paren statejével nem nagyon kéne foglalkozni
getI (ExpTypeSig e _) = getI e
getI (VarP i _ _) = error "VarP should not be examined"

setI :: Exp -> ExpInfoSc -> Exp
setI (Var _ n) i = Var i n
setI (Con _ n) i = Con i n
setI (Lit _ l) i = Lit i l
setI (InfixApp _ a op b) i = InfixApp i a op b
setI (App _ a b) i = App i a b
setI (NegApp _ e) i = NegApp i e
setI (Lambda _ p e) i = Lambda i p e
setI (Let _ bs e) i = Let i bs e
setI (Tuple _ es) i = Tuple i es
setI (List _ es) i = List i es
setI (Paren _ e) i = Paren i e
setI (ExpTypeSig e t) i = ExpTypeSig (setI e i) t
setI (VarP _ n e) i = error "VarP should not be examined"



class Get a where
    getStr :: a -> Maybe [String]
    getName :: a -> Maybe [Name]
    safeGet :: a -> String
    {-# MINIMAL getStr #-}

instance Get Name where
    getStr n = Just [safeGet n]
    getName x = Just [x] 
    safeGet (Ident x) = x
    safeGet (Symbol x) = x

instance Get QName where
    getStr (Qual (ModuleName mn) n) = Just [mn <> "." <> safeGet n] 
    getStr (UnQual n) = getStr n

    getName (UnQual n) = Just [n]
    getName (Qual _ n) = Just [n]

    safeGet (Qual (ModuleName mn) n) = mn <> "." <> safeGet n
    safeGet (UnQual n) = safeGet n

instance Get ModuleName where
    getStr (ModuleName n) = Just [n]
    safeGet (ModuleName n) = n

instance Get Pat where
    getStr (PWildCard _) = Nothing
    getStr (PLit _ _ _) = Nothing
    getStr p = Just $ flattenS p
    safeGet (PVar _ n) = safeGet n 

    getName p = Just $ flattenN p

instance Get Binds where
    getStr (BDecls ds) = Just $ concat $ mapMaybe getStr ds

instance Get QOp where
    getStr (QVarOp _ n) = getStr n
    getStr (QConOp _ n) = getStr n
    safeGet (QVarOp _ n) = safeGet n
    safeGet (QConOp _ n) = safeGet n

instance (Get s) => Get (Maybe s) where
    getStr (Just x) = getStr x
    getStr Nothing = Nothing

instance Get Decl where
    getStr (FunBind _ (Match _ n _ _ _:_)) = getStr n
    getStr (PatBind _ p _ _) = getStr p
    getStr (TypeSig _ _) = Nothing
    getStr _ = Nothing
    safeGet (FunBind _ (Match _ n _ _ _:_)) = safeGet n

    getName (FunBind _ (Match _ n _ _ _:_)) = Just [n]
    getName (PatBind _ p _ _) = getName p
    getName (TypeSig _ _) = Nothing


instance Get Match where
    getStr  (Match _ n _ _ _) = getStr n
    safeGet (Match _ n _ _ _) = safeGet n
