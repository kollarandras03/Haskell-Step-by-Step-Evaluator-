module Model.Preprocess.FileParser(parseFile) where
import qualified Language.Haskell.Exts as E
import Model.Base

parseFile :: FilePath -> IO (ErrorOr Module)
parseFile filePath = do
    parsedFile <- E.parseFile filePath
    case parsedFile of
        E.ParseFailed _ s -> error ("Parse failed." <> s)
        E.ParseOk m@(E.Module {}) -> return (convert (void m))
