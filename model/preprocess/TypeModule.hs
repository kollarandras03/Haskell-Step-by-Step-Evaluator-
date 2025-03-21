module Model.Preprocess.TypeModule where
import Model.Preprocess.TypeDecl
import Model.Base
import Language.Haskell.Interpreter
import System.FilePath

getTypes :: [FilePath] -> [Module] -> IO [[Decl]]
getTypes paths modules = do
    result <- runInterpreter $ do
        -- Initial options
        let options = [NoImplicitPrelude, NoMonomorphismRestriction, ExplicitForAll, ExtendedDefaultRules]
        set [languageExtensions := options]
        -- Making every module available
        loadModules paths

        -- Getting the types 
        let (um:libns) = map takeBaseName paths
        let moduleNames = um : map ("Library."++) libns

        res <- mapM getModuleTypes (zip3 paths moduleNames modules)
        return res

    case result of
        Left (WontCompile (GhcError {errMsg}:_)) -> do
            putStrLn $ "Custom error at getTypes: " ++ errMsg
            return []
        Left err -> error $ show err 
        Right values -> return values


-- Egyetlen modul típusait kéri le
getModuleTypes :: (FilePath,String,Module) -> Interpreter [Decl]
getModuleTypes (p,s,m@(Module _ _ _ ds)) = do
    setTopLevelModules [s]
    ds' <- mapM (typeIt ds) (filter (\d -> isFunBind d || isPatBind d) ds)
    pure ds'
