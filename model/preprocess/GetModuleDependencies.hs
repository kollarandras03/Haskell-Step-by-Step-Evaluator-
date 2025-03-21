module Model.Preprocess.GetModuleDependencies(LightModule(..), getSimpleDependencies) where
import Model.Base
import Prelude

findModule :: ModuleName -> [Module] -> Maybe Module
findModule mn [] = Nothing
findModule mn (m@(Module mn' _ _ ds):ms) = if mn == mn' then Just m else findModule mn ms

-- Imports all modules that are imported (recursively)
-- ignores import/export lists, and imports whole module anyways
handleImportSimple :: [ImportDecl] -> [Module] -> ([LightModule],[Module])
handleImportSimple [] _ = ([], [])
handleImportSimple (ImportModule mn:is) ms = case findModule mn ms of
    Just (Module mn' _ (Just imps) ds) -> let 
        lightmd = LightModule (mn', ds)
        restModules = filter (\(Module mn'' _ _ _) -> mn'' /= mn) ms
        (lightms,ms') = handleImportSimple imps restModules
        (lightms',ms'') = handleImportSimple is ms'
        in (lightmd:lightms ++ lightms', ms'')
    Just (Module mn' _ Nothing _) -> ([],ms)
    Nothing -> handleImportSimple is ms
handleImportSimple (_:_) _ = error "Not supported import type"


getSimpleDependencies :: [Module] -> Module -> [LightModule]
getSimpleDependencies ms (Module mn exps imps decls) = LightModule (mn,decls) : lightms where
    (lightms,_) = handleImportSimple (fromMaybe [] imps) ms












