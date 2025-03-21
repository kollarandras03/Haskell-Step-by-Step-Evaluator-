module Model.Preprocess.Qualify where
import Model.Base
import qualified Data.Set as Set


type ShadowedNames = Set String
type LightLib = [LightModule]


class Qualify a where
    qualify :: (LightLib, ShadowedNames) -> a -> a

instance Qualify Module where
    qualify dep (Module mn i e ds) = Module mn i e (map (qualify dep) ds)

instance Qualify Decl where
    qualify _ d@(TypeSig {}) = d
    qualify dep (FunBind s ms) = FunBind s (map (qualify dep) ms)
    qualify dep (PatBind s p rhs bs) = PatBind s p newRhs newBs where
        (newRhs, newBs) = qualify dep (rhs,bs)

instance Qualify (Rhs, Maybe Binds) where
    qualify (lib, shadows) (rhs,bs) = (newRhs,newBs) where
        newShadows = shadows `Set.union` (Set.fromList $ concatMapM getStr $ (\(BDecls ds) -> ds) $ fromMaybe (BDecls []) bs)
        newRhs = qualify (lib, newShadows) rhs
        newBs = fmap (\(BDecls ds) -> BDecls $ map (qualify (lib, newShadows)) ds) bs

instance Qualify Match where
    qualify (lib,shadows) m = case m of 
        (Match s n ps rhs bs) -> 
            let (newRhs, newBs) = qualify (lib, newShadows) (rhs,bs)
                newShadows = shadows `Set.union` (Set.fromList $ concatMapM getStr ps)
            in  Match s n ps newRhs newBs

instance Qualify Rhs where
    qualify dep (UnGuardedRhs s e) = UnGuardedRhs s (qualify dep e)
    qualify dep (GuardedRhss s rhss) = GuardedRhss s (map (qualify dep) rhss) 

instance Qualify GuardedRhs where
    qualify dep (GuardedRhs s stmts e) = GuardedRhs s (map (qualify dep) stmts) (qualify dep e)

instance Qualify Stmt where
    qualify dep (Qualifier s e) = Qualifier s (qualify dep e) 

instance Qualify QOp where
    qualify d (QVarOp s n) = let Var s' n' = qualify d (Var s n) in QVarOp s' n'
    qualify d (QConOp s n) = let Con s' n' = qualify d (Con s n) in QConOp s' n'

getQName :: LightLib -> String -> Maybe ModuleName
getQName [] n = Nothing
getQName ((LightModule (mn, ds)):ms) n = if n `elem` names then Just mn else getQName ms n where
    names = concatMap getNames ds

    getNames :: Decl -> [String]
    getNames (FunBind _ (m:_)) = [safeGet m]
    getNames (PatBind _ p _ _) = flattenS p
    getNames _ = []

instance Qualify Exp where
    qualify d e@(Var _ (Qual {})) = e
    qualify (lib, shadows) e@(Var s (UnQual n))
        | (safeGet n) `Set.member` shadows = e -- local binding, stays unqual
        | otherwise = case getQName lib (safeGet n) of
            Just mn -> Var s (Qual mn n)
            Nothing -> error "Impossible happened at Qualify Exp"

    qualify (lib,shadows) (Let s (BDecls bs) e) = Let s newBs newExp where
        newExp = qualify (lib, newShadows) e
        newBs = BDecls $ Prelude.map (qualify (lib, newShadows)) bs
        newShadows = shadows `Set.union` (Set.fromList $ concatMapM getStr bs)
    
    qualify (lib,shadows) (Lambda s ps e) = Lambda s ps newExp where
        newExp = qualify (lib, newShadows) e
        newShadows = shadows `Set.union` (Set.fromList $ concatMapM getStr ps)

    -- Base cases and recursion
    qualify _ e@(Lit {}) = e
    qualify d e@(Con {}) = e
    qualify d (App s e1 e2) = App s (qualify d e1) (qualify d e2)
    qualify d (InfixApp s e1 op e2) = InfixApp s (qualify d e1) (qualify d op) (qualify d e2)
    qualify d (List s es)  = List s (map (qualify d) es)
    qualify d (Tuple s es) = Tuple s (map (qualify d) es)
    qualify d (Paren s e) = Paren s (qualify d e)
    qualify d (ExpTypeSig e t) = ExpTypeSig (qualify d e) t

    qualify _ e = errorWithoutStackTrace $ "qualify: Case not handled:" <> show e
