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

data ModuleMapping = ModuleMapping String [String]
  deriving Show

runReflexivite :: Shaker IO [ModuleMapping]
runReflexivite = do
  cplInp <- getCompileInputForAllHsSources 
  targetFiles <- checkTargetFiles $ cfTargetFiles cplInp
  modMaps <- lift $ runGhc (Just libdir) $ do 
            _ <- ghcCompile cplInp targetFiles 
            getModuleMappingList
  return modMaps
  
{-
example :: IO [String]
example = defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc ["-itestsuite/tests","-isrc/","-package ghc"])
    _ <- setSessionDynFlags newFlags
    target <- guessTarget "Shaker.CliTest" Nothing
    setTargets [target]
    _ <- load LoadAllTargets
    modulesList <- getModuleGraph
    tyThingsList <- getTyThingsFromModuleSummary modulesList
    return $ getQuickcheckFunction tyThingsList
 -}

getModuleMappingList :: (GhcMonad m) => m [ModuleMapping] 
getModuleMappingList = do
  modSummaries <- getModuleGraph
  return $ map getModuleMapping modSummaries 

getModuleMapping :: ModSummary -> ModuleMapping
getModuleMapping  modSum = ModuleMapping modName [] 
 where modName = (moduleNameString . moduleName . ms_mod) modSum

getTyThingsFromModuleSummary :: (GhcMonad  m) => [ModSummary] -> m [TyThing]
getTyThingsFromModuleSummary modSummaries = do
        modulesInfo <- mapM getModuleInfo modules
        return $ concat $ map modInfoTyThings $ catMaybes modulesInfo
   where modules = map ms_mod modSummaries
         
getQuickcheckFunction :: [TyThing] -> [String]
getQuickcheckFunction tyMap = filter ("prop_" `isPrefixOf`) nameList
   where idList = catMaybes $ map tyThingToId tyMap
         varList = map varName idList
         occList = map nameOccName varList
         nameList = map occNameString occList

tyThingToId :: TyThing -> Maybe Id
tyThingToId (AnId tyId) = Just tyId
tyThingToId _ = Nothing
