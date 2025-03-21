module Model.AST.Module(LightModule(..),Module(..),ExportName(..),ImportDecl(..)) where
import Model.AST.Core
import Model.AST.Names 
import qualified Language.Haskell.Exts as E
import Model.Error
import Model.AST.Convertable

newtype LightModule = LightModule (ModuleName, [Decl])

-- Define your custom types
data ExportName 
    = ExportFun  QName   -- exporting a function
    | ExportType QName  -- exporting a type
    | ExportModule ModuleName deriving Eq

data ImportDecl 
    = ImportFun ModuleName Name          -- imports a whole module
    | ImportType ModuleName Name     -- imports a selected set of exports
    | ImportModule ModuleName deriving Eq

data Module 
    = Module ModuleName (Maybe [ExportName]) (Maybe [ImportDecl]) [Decl] deriving Eq

instance Convertable (E.Module ()) Module where
    -- Convert function
    convert :: E.Module () -> ErrorOr Module
    convert (E.Module _ Nothing _ _ _) = Left NoModuleName
    convert (E.Module _ _ (_:_) _ _) = Left $ ErrorWith "Language pragmas are not allowed!"
    convert (E.Module _ (Just mhead) [] imports decls) =
            Module
                <$> (getModuleName mhead)
                <*> (getExportList mhead)
                <*> (convertImports imports)
                <*> (traverse (\x -> convert x :: ErrorOr Decl) decls)
    convert _ = Left $ ErrorWith "Unknown module conversion error"
    revert = undefined



getModuleName :: E.ModuleHead () -> ErrorOr ModuleName
getModuleName (E.ModuleHead _ _ (Just _) _) = Left $ ErrorWith "Warning text is not allowed!"
getModuleName (E.ModuleHead _ (E.ModuleName _ s) _ _) = Right $ ModuleName s
getModuleName (E.ModuleHead _ _ _ _) = Left $ ErrorWith "Unknown modulehead error"

getExportList :: E.ModuleHead () -> ErrorOr (Maybe [ExportName])
getExportList (E.ModuleHead _ _ _ Nothing) = Right Nothing
getExportList (E.ModuleHead _ _ _ (Just (E.ExportSpecList _ exps))) = Just <$> traverse convertExport exps

convertExport :: E.ExportSpec () -> ErrorOr ExportName
convertExport (E.EModuleContents _ (E.ModuleName _ mn)) = Right $ ExportModule (ModuleName mn)
convertExport (E.EVar _ qn) = ExportFun <$> (convert qn)
convertExport (E.EThingWith _ (E.EWildcard _ _) qn []) = ExportType <$> (convert qn :: ErrorOr QName)
convertExport (E.EThingWith _ _ _ _) = Left $ ErrorWith "Selective export is not supported!"
convertExport (E.EAbs {}) = Left NotSupported -- module Foo(Bar) where

convertImports :: [E.ImportDecl ()] -> ErrorOr (Maybe [ImportDecl])
convertImports [] = Right Nothing
convertImports impds = Just <$> concat <$> traverse convertImport impds

convertImport :: (E.ImportDecl ()) -> ErrorOr [ImportDecl]
--convertImport (E.ImportDecl _ _ False False False Nothing Nothing _) = Left NotSupported
convertImport (E.ImportDecl _ mn _ _ _ _ _ Nothing) =  (:[]) <$> (ImportModule <$> (convert mn))
convertImport (E.ImportDecl _ _ _ _ _ _ _ (Just (E.ImportSpecList _ True _))) = Left NotSupported -- import Foo hiding(Bar)
convertImport (E.ImportDecl _ mn _ _ _ _ _ (Just (E.ImportSpecList _ False imps))) = traverse (convertImportSpec mn) imps

convertImportSpec :: E.ModuleName () -> E.ImportSpec () -> ErrorOr ImportDecl
convertImportSpec mn (E.IVar _ n) = ImportFun <$> (convert mn) <*> (convert n)
convertImportSpec mn (E.IThingAll _ _) = ImportModule <$> (convert mn)
convertImportSpec _ (E.IThingWith {}) = Left NotSupported -- import Foo(Bar(Baz)) data Bar = Baz | Qux
convertImportSpec _ (E.IAbs {}) = Right $ ImportType (ModuleName "None") (Ident "None") --Left NotSupported -- import Foo(Bar)

