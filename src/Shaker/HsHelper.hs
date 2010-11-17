module Shaker.HsHelper
 where

import Control.Arrow
import Data.List
import Data.Maybe
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Shaker.Io
import Shaker.Type

parseHsFiles :: [FileListenInfo] -> IO [HsModule]
parseHsFiles fliListenInfos = do
  files <- recurseMultipleListFiles fliListenInfos
  parseResults <- mapM parseFileToHsModule files
  return $ mapMaybe parseHs parseResults
  where parseHs parseResults = case parseResults of
                               ParseOk val -> Just val
                               _ -> Nothing
        parseFileToHsModule fp = do 
          fileContent <- readFile fp
          return $ parseModuleWithMode defaultParseMode { parseFilename = fp } fileContent

hsModuleCollectProperties :: HsModule -> [String]
hsModuleCollectProperties = getListFunction >>> filter (isPrefixOf "prop_")

abstractCollectFunctionWithUnqualifiedType :: (HsQualType -> Bool) -> HsModule -> [String]
abstractCollectFunctionWithUnqualifiedType fun = getTupleFunctionNameType 
  >>> filterSnd fun
  >>> map fst

hsModuleCollectTest :: HsModule -> [String]
hsModuleCollectTest = abstractCollectFunctionWithUnqualifiedType predicat
  where predicat (HsQualType _ (HsTyCon (UnQual hsName) ) ) = hsName == HsIdent "Test"
        predicat _ = False

hsModuleCollectAssertions :: HsModule -> [String]
hsModuleCollectAssertions = abstractCollectFunctionWithUnqualifiedType predicat
  where predicat (HsQualType _ (HsTyCon (UnQual hsName) ) ) = hsName == HsIdent "Assertion"
        predicat _ = False

filterSnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
filterSnd fun = filter (snd >>> fun)

mapSnd :: ( t1 -> t2 ) -> [ ( t, t1 ) ] -> [ ( t , t2 ) ] 
mapSnd fun = map ( second fun )

getListFunction :: HsModule -> [String]
getListFunction = getDecls >>> mapMaybe getPatBindName

getTupleFunctionNameType :: HsModule -> [(String, HsQualType)]
getTupleFunctionNameType = getDecls >>> mapMaybe getSignature

getSignature :: HsDecl -> Maybe (String, HsQualType)
getSignature (HsTypeSig _ hsNames hsQualType) = Just (head >>> getIdentFromHsName $ hsNames, hsQualType)
getSignature _ = Nothing

getPatBindName :: HsDecl -> Maybe String
getPatBindName (HsPatBind _ (HsPVar (HsIdent funName))_ _) = Just funName
getPatBindName _ = Nothing

getIdentFromHsName :: HsName -> String
getIdentFromHsName (HsIdent v) = v
getIdentFromHsName _ = ""

getDecls :: HsModule -> [HsDecl]
getDecls (HsModule _ _ _ _ decls) = decls

hsModuleFileName :: HsModule -> String
hsModuleFileName (HsModule loc _ _ _ _) = srcFilename loc

hsModuleName :: HsModule -> String
hsModuleName (HsModule _ (Module moduleName) _ _ _) = moduleName

