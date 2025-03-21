module Model.Preprocess.CheckModuleValidity where
import Language.Haskell.Interpreter
import System.FilePath

-- Given a module, and all available modules
-- This function tries to compile the module using GHCi API
-- Returns any error if fails, Nothing if it compiles
type CompileError = String
-- NoMonomorphismRestriction
checkModule :: InterpreterT IO a -> (FilePath,[FilePath]) -> IO (Maybe [CompileError])
checkModule interpreterAction (topLevel, allModules) = do
    res <- runInterpreter $ do
        set [languageExtensions := [ExtendedDefaultRules, ExplicitForAll, NoImplicitPrelude]]
        loadModules (topLevel : allModules)
        setTopLevelModules [takeBaseName topLevel]
        interpreterAction
    case res of
        Left (WontCompile es) -> return (Just (map (\(GhcError errMsg) -> errMsg) es))
        Left (NotAllowed s) -> return (Just [s])
        Left (GhcException ex) -> return (Just [ex])
        Left (UnknownError err) -> return (Just [err])
        Right _ -> return Nothing

checkExp :: MonadInterpreter m => String -> m String
checkExp s = typeOf s
