module Shaker.Action.Quickcheck
 where

{-
import OccName (occNameString)
import Name (nameOccName)
import Var (varName)
import Data.List
import Data.Maybe
import           Data.Dynamic                           (fromDynamic)
import GHC
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader
import Unsafe.Coerce


runQuickcheck :: Plugin
runQuickcheck = do
        (CompileInput sourceDir targetInput procFlags strflags) <-  asks compileInputs
        (ListenerInput fli _) <- asks listenerInput 
        targetFiles <-  lift $ recurseMultipleListFiles fli
        lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
	               _ <- setSessionDynFlags $ procFlags $ setSourceAndTarget sourceDir targetInput newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       _ <- load LoadAllTargets
                       return()
runTest :: IO()
runTest = do 
  _ <- getTestFunc
  return()


getTestFunc :: IO()
getTestFunc = 
            defaultErrorHandler defaultDynFlags $ do
            runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc ["-itestsuite/tests","-isrc/","-package ghc"])
            _ <- setSessionDynFlags newFlags
            target <- guessTarget "Shaker.RunTest" Nothing
            setTargets [target]
            _ <- load LoadAllTargets
            m <- findModule (mkModuleName "Shaker.RunTest") Nothing
            setContext [] [m]
            dynCompileExpr("runAll")

            --compileExpr ("runAll")
            --(unsafeCoerce value) 

example :: IO[String]
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
--    return $ showPpr gra
    return $ getQuickcheckFunction tyThingsList
 
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

setSourceAndTarget :: [String] -> String ->DynFlags -> DynFlags
setSourceAndTarget sources target dflags = dflags{
    importPaths = sources
    ,objectDir = Just target
    ,hiDir = Just target
  }


-} 
