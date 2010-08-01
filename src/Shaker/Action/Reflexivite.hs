module Shaker.Action.Reflexivite
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
  ,cfTestFunctionName :: [String] -- ^ Hunit test function names
  ,cfPropName :: [String] -- ^ QuickCheck test function names
 }
 deriving Show

-- | Collect all non-main modules with their test function associated
runReflexivite :: Shaker IO [ModuleMapping]
runReflexivite = do
  cplInp <- getCompileInputForAllHsSources 
  targetFiles <- checkTargetFiles $ cfTargetFiles cplInp
  modMaps <- lift $ runGhc (Just libdir) $ do 
            _ <- ghcCompile cplInp targetFiles 
            modSummaries <- getModuleGraph
            mapM getModuleMapping modSummaries 
  return modMaps

-- | Collect module name and tests name for the given module
getModuleMapping :: (GhcMonad m) => ModSummary -> m ModuleMapping
getModuleMapping  modSum = do 
  mayModuleInfo <- getModuleInfo $  ms_mod modSum
  props <- return $ getQuickcheckFunction mayModuleInfo
  return $ ModuleMapping modName [] props
  where modName = (moduleNameString . moduleName . ms_mod) modSum        
       
getQuickcheckFunction :: Maybe ModuleInfo -> [String]
getQuickcheckFunction Nothing = []
getQuickcheckFunction (Just modInfo) = filter ("prop_" `isPrefixOf`) nameList
   where tyMap = modInfoTyThings modInfo 
         idList = catMaybes $ map tyThingToId tyMap
         varList = map varName idList
         occList = map nameOccName varList
         nameList = map occNameString occList

tyThingToId :: TyThing -> Maybe Id
tyThingToId (AnId tyId) = Just tyId
tyThingToId _ = Nothing

