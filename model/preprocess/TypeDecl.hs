module Model.Preprocess.TypeDecl where
import Model.Base
import Language.Haskell.Interpreter
import qualified Language.Haskell.Exts as E
import qualified Debug.Trace as DEB

-- THIS IS A RESTRICTED NAME FOR FUNCTIONS/PATTERNS
burninName = "burn1"


tySplit :: String -> [EType]
tySplit = getParamTypes . parseEType

getParamTypes :: EType -> [EType]
getParamTypes (E.TyForall _ Nothing mbCxt t) = map (applyConstraint (getContext mbCxt)) (getParamTypes t)
getParamTypes (E.TyFun _ t1 t2) = t1 : getParamTypes t2
getParamTypes (E.TyParen _ t) = getParamTypes t
getParamTypes t@(E.TyVar _ _) = [t]
getParamTypes t@(E.TyCon _ _) = [t]
getParamTypes _ = error "Unsupported node in typing"  -- Ignore unsupported nodes

getContext :: Maybe (E.Context ()) -> [(E.Name (), E.Asst ())]
getContext Nothing = []
getContext (Just (E.CxSingle _ asst)) = [getAsst asst]
getContext (Just (E.CxTuple _ assts)) = map getAsst assts
getContext (Just (E.CxEmpty _)) = []

getAsst :: E.Asst () -> (E.Name (), E.Asst ())
getAsst (E.ParenA () a) = getAsst a
getAsst a@(E.TypeA () t) = (getAN t, a) where
    getAN (E.TyApp () t1 (E.TyParen _ t)) = getAN (E.TyApp () t1 t)
    getAN (E.TyApp () _ (E.TyVar _ n)) = n 

applyConstraint :: [(E.Name (), E.Asst ())] -> EType -> EType
applyConstraint _ t@(E.TyCon _ _) = t
applyConstraint ts t@(E.TyVar _ n) = case filter ((==n) . fst) ts of
    [] -> t
    [(_,asst)] -> E.TyForall () Nothing (Just (E.CxSingle () asst)) t
    assts -> E.TyForall () Nothing (Just (E.CxTuple () (map snd assts))) t
applyConstraint ts (E.TyParen _ t) = E.TyParen () (applyConstraint ts t)
applyConstraint ts (E.TyFun _ t1 t2) = E.TyFun () (applyConstraint ts t1) (applyConstraint ts t2)
applyConstraint _ _ = error "Unsupported node in typing"  -- Ignore unsupported nodes


parseEType = void . E.fromParseResult . E.parseType

type Context = String

context xs -- = Just $ " let {" <> intercalate ";" xs <> "} in "
    | DEB.trace ("Context: " <> show xs <> "\n") True 
        = Just $ " let {" <> intercalate ";" xs <> "} in "

fromBinds Nothing = []
fromBinds (Just (BDecls ds)) = ds

toBinds [] = Nothing
toBinds xs = Just (BDecls xs)

typeOfC :: Maybe Context -> String -> Interpreter String
typeOfC c s | DEB.trace ("TypeGet: " <> fromMaybe "" c <> s <> "\n") True = typeOf $ (fromMaybe "" c) <> s <> "\n"
--typeOfC c s = typeOf $ (fromMaybe "" c) <> s


typeIt :: [Decl] -> Decl -> Interpreter Decl
typeIt ds d | DEB.trace ("typeIt: " <> prettyPrint d <> "\n") True = do 
    dsWTypes <- prepareBlockContext ds
    typeDecl dsWTypes d 










prepareBlockContext :: [Decl] -> Interpreter (Maybe Context)
prepareBlockContext ds = do
    let fnames = map prettyPrint $ concat $ mapMaybe getName ds
    types <- mapM typeOf fnames
    let dTypes = map parseDeclUnsafe (zipWith (\f t -> f <> " :: " <> t) fnames types)
    let funsPats = filter (\d -> isFunBind d || isPatBind d) ds
    pure $ context $ map prettyPrint $ dTypes <> funsPats







pure' !x = pure x

typeDecl :: Maybe Context -> Decl -> Interpreter Decl
typeDecl _ d@(TypeSig {}) = pure d
typeDecl c this@(PatBind i@(DeclInfo s mPt) p rhs bs) = do
    -- If pat's type was not known, get it from interpreter
    (pt,pts) <- case mPt of
        Nothing -> do
            let p' = PAsPat (PatInfo NoChange Nothing) (Ident burninName) (PParen p)
            let this' = PatBind i p' rhs bs
            let c1 = c <> context [prettyPrint this'] 
            ts <- typeOfC c1 burninName
            pure (parseEType ts,ts)
        Just t -> pure (t, E.prettyPrint t)
    -- Type of pat is known, add itself and then the binds to the context
    let c2 = c <> context [proxy p pts]
    let c3 = c2 <> context (map prettyPrint (fromBinds bs))
    -- Get type of rhs and binds
    rhs' <- typeRhs c3 rhs
    bs' <- typeBs c3 bs
    -- PatBind is fully typed
    pure $ PatBind (DeclInfo s (Just pt)) p rhs' bs'

-- lehet hogy this-t bele kell tenni a továbbadott contextbe
typeDecl c this@(FunBind (DeclInfo s mFt) ms) = do
    -- If fun's type was not known, get it from interpreter
    (ft,fts) <- case mFt of
        Nothing -> do
            let c1 = c -- <> context [prettyPrint this]
            ft <- typeOfC c1 (getPrefixName (head ms))
            pure (parseEType ft, ft)
        Just t -> pure (t, E.prettyPrint t)
    ms' <- mapM (typeMatch (getParamTypes ft) c) ms
    -- FunBind is fully typed
    pure $ FunBind (DeclInfo s (Just ft)) ms'

getPrefixName (Match _ n _ _ _ ) = asPrefixN n


typeMatch :: [EType] -> Maybe Context -> Match -> Interpreter Match
typeMatch pTypes c (Match s n pats rhs bs) = do
    -- Add all params with their types to the context, and binds as well
    let c1 = c <> context (zipWith proxy pats (map E.prettyPrint pTypes))
    let c2 = c1 <> context (map prettyPrint (fromBinds bs))

    rhs' <- typeRhs c2 rhs
    bs' <- typeBs c2 bs

    -- Match is fully typed
    pure $ Match s n pats rhs' bs'

typeBs :: Maybe Context -> Maybe Binds -> Interpreter (Maybe Binds)
typeBs c Nothing = pure Nothing
typeBs c (Just (BDecls bs)) = Just <$> BDecls <$> (mapM (typeDecl c) bs)

typeRhs :: Maybe Context -> Rhs -> Interpreter Rhs
typeRhs c (UnGuardedRhs s e) = UnGuardedRhs s <$> typeExp c e
typeRhs c (GuardedRhss s rhss) = GuardedRhss s <$> (mapM (typeGrhs c) rhss)

typeGrhs :: Maybe Context -> GuardedRhs -> Interpreter GuardedRhs
typeGrhs c (GuardedRhs s stmts e) = GuardedRhs s <$> (mapM (typeStmt c) stmts) <*> (typeExp c e)

typeStmt :: Maybe Context -> Stmt -> Interpreter Stmt
typeStmt c (Qualifier s e) = Qualifier s <$> typeExp c e

typeExp :: Maybe Context -> Exp -> Interpreter Exp 
typeExp _ e@(Var {}) = pure e -- we could add the type now to var, but should we?
typeExp _ e@(Con {}) = pure e
typeExp _ e@(Lit {}) = pure e

typeExp c  (Paren s e) = Paren s <$> typeExp c e
typeExp c (InfixApp s e1 qop e2) = InfixApp s <$> (typeExp c e1) <*> (pure qop) <*> (typeExp c e2) -- we could add the type now to qop, but should we?
typeExp c (App s e1 e2) = App s <$> (typeExp c e1) <*> (typeExp c e2)
typeExp c (NegApp s e1) = NegApp s <$> (typeExp c e1)
typeExp c (Tuple s es) = Tuple s <$> mapM (typeExp c) es
typeExp c (List s es) = List s <$> mapM (typeExp c) es
typeExp c (ExpTypeSig e t) = ExpTypeSig <$> typeExp c e <*> (pure t)
typeExp c (Let s bs@(BDecls ds) e) = do
    let c1 = c <> context (map prettyPrint ds)    
    e' <- typeExp c1 e
    bs' <- typeBs c1 (Just bs)
    case bs' of
        Just bs'' -> pure $ Let s bs'' e'
        Nothing -> error "Let got empty bind in typeExp"

typeExp c lam@(Lambda s pats e) = do
    ltype <- typeOfC c ("(" <> prettyPrint lam <> ")")
    let pTypes = getParamTypes (parseEType ltype) :: [EType]
    let c1 = c <> context (zipWith proxy pats (map E.prettyPrint pTypes))
    e' <- typeExp c1 e 
    pure $ Lambda s pats e' 

-- Creates an alibi declaration from a pattern/name and a type
proxy :: Pat -> String -> String
proxy p ty = prettyPrint p <> " = undefined :: " <> ty

unSafeRevert x = case revert x of
    Left e -> error "unsafe revert typedecl fail"
    Right x -> x




