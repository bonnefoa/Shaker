module Shaker.Reflexivite
 where

import OccName (occNameString)
import Name (nameOccName)
import Var (varName)
import Data.List
import Data.Maybe
import GHC
import GHC.Paths
import Shaker.Type 
import Shaker.Action.Compile
import Shaker.SourceHelper
import Control.Monad.Reader

-- ^ Mapping between module name (to import) and test to execute
data ModuleMapping = ModuleMapping {
  cfModuleName :: String -- ^ Complete name of the module 
  ,cfHunitName :: [String] -- ^ Hunit test function names
  ,cfPropName :: [String] -- ^ QuickCheck test function names
 }
 deriving Show

-- | Collect all non-main modules with their test function associated
runReflexivite :: Shaker IO [ModuleMapping]
runReflexivite = do
  cpList <- asks compileInputs 
  let cpIn = mergeCompileInputsSources cpList
  cfFlList <- lift $ constructCompileFileList cpIn
  modMaps <- lift $ runGhc (Just libdir) $ do 
            _ <- ghcCompile $ runReader (removeFileWithTemplateHaskell cpIn >>= removeFileWithMain >>= setAllHsFilesAsTargets ) cfFlList
            modSummaries <- getModuleGraph
            mapM getModuleMapping modSummaries 
  return modMaps

-- | Collect module name and tests name for the given module
getModuleMapping :: (GhcMonad m) => ModSummary -> m ModuleMapping
getModuleMapping  modSum = do 
  mayModuleInfo <- getModuleInfo $  ms_mod modSum
  props <- return $ getQuickcheckFunction mayModuleInfo
  hunits <- return $ getHunitFunctions mayModuleInfo
  return $ ModuleMapping modName hunits props
  where modName = (moduleNameString . moduleName . ms_mod) modSum        
       
getQuickcheckFunction :: Maybe ModuleInfo -> [String]
getQuickcheckFunction = getFunctionWithPredicate ("prop_" `isPrefixOf`) 

getHunitFunctions :: Maybe ModuleInfo -> [String]
getHunitFunctions = getFunctionWithPredicate ("test" `isPrefixOf`) 

getFunctionWithPredicate :: (String -> Bool) -> Maybe ModuleInfo -> [String]
getFunctionWithPredicate _ Nothing = []
getFunctionWithPredicate predicat (Just modInfo) = filter predicat nameList
   where idList = catMaybes $ map tyThingToId $ modInfoTyThings modInfo
         nameList = map (occNameString . nameOccName . varName) idList 

tyThingToId :: TyThing -> Maybe Id
tyThingToId (AnId tyId) = Just tyId
tyThingToId _ = Nothing

