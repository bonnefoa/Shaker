module Shaker.HsHelper
 where

import Shaker.Type
import Shaker.Io

import Data.List
import Data.Maybe

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

import Control.Arrow

fillModuleData :: ShakerInput -> IO ShakerInput
fillModuleData shIn = do
  lstHsModules <- shakerListenerInput >>> listenerInputFiles >>> parseHsFiles $ shIn
  return shIn { shakerModuleData = map constructModuleData lstHsModules }

parseHsFiles :: [FileListenInfo] -> IO [Module]
parseHsFiles fliListenInfos = do
  files <- recurseMultipleListFiles fliListenInfos
  fileContentList <- mapM readFile files
  return $ mapMaybe parseHs fileContentList
  where parseHs content = case parseModule content of
                               ParseOk val -> Just val
                               _ -> Nothing

constructModuleData :: Module -> ModuleData
constructModuleData hsModule = ModuleData {
  moduleDataModule = hsModule
  ,moduleDataProperties = hsModuleCollectProperties hsModule
  ,moduleDataAssertions = hsModuleCollectAssertions hsModule
  ,moduleDataTestCase = hsModuleCollectTest hsModule
 }

hsModuleHasMain :: Module -> Bool
hsModuleHasMain = getTupleIdentType >>> map fst >>> any (=="main")

hsModuleDataHasTest :: ModuleData -> Bool
hsModuleDataHasTest hsModuleData = any (not . null) [moduleDataProperties hsModuleData, moduleDataAssertions hsModuleData] 

hsModuleCollectProperties :: Module -> [String]
hsModuleCollectProperties = getTupleIdentType >>> map fst >>> filter (isPrefixOf "prop_")

abstractCollectFunctionWithUnqualifiedType :: (String -> Bool) -> Module -> [String]
abstractCollectFunctionWithUnqualifiedType fun = getTupleIdentUnqualifiedType
  >>> filterSnd fun
  >>> map fst

hsModuleCollectTest :: Module -> [String]
hsModuleCollectTest = abstractCollectFunctionWithUnqualifiedType (== "Test") 

hsModuleCollectAssertions :: Module -> [String]
hsModuleCollectAssertions = abstractCollectFunctionWithUnqualifiedType ( == "Assertion") 

filterSnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
filterSnd fun = filter (snd >>> fun)

mapSnd :: ( t1 -> t2 ) -> [ ( t, t1 ) ] -> [ ( t , t2 ) ] 
mapSnd fun = map ( second fun )

getTupleIdentUnqualifiedType :: Module -> [(String, String)]
getTupleIdentUnqualifiedType = getTupleIdentType 
  >>> mapSnd getSigUnQualType 
  >>> filterSnd isJust
  >>> mapSnd fromJust

getTupleIdentType :: Module -> [(String,Type)] 
getTupleIdentType = hsModuleDecl >>> mapMaybe getSigIdent 
  where getSigIdent (TypeSig _ names ty) = Just (ident (head names), ty)
        getSigIdent _ = Nothing

getSigTypes :: Module -> [String] 
getSigTypes = hsModuleDecl >>> mapMaybe getSigType
  where getSigType (TypeSig _ _ (TyCon (UnQual name) ) ) = Just (ident name)
        getSigType _ = Nothing

getSigUnQualType :: Type -> Maybe String
getSigUnQualType (TyCon (UnQual name) ) = Just (ident name)
getSigUnQualType _ = Nothing

ident :: Name -> String
ident (Ident v) = v
ident (Symbol v) = v

hsModuleDecl :: Module -> [Decl]
hsModuleDecl (Module _ _ _ _ _ _ decls) = decls

