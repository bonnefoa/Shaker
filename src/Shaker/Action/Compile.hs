module Shaker.Action.Compile
  (runCompile ,runFullCompile ,ghcCompile)
 where

import Control.Monad.Reader
import DynFlags 
import GHC
import GHC.Paths
import Shaker.CabalInterface
import Shaker.GhcInterface
import Shaker.ModuleData
import Shaker.Type

-- | Run haskell compilation on given CompileInput list
runCompile :: Plugin
runCompile = applyPreprocessSources 
  >> asks shakerCompileInputs 
  >>= foldM runUntilFail Succeeded
  >> return ()

runUntilFail :: SuccessFlag -> CompileInput -> Shaker IO SuccessFlag
runUntilFail Succeeded cpIn = runSingleCompileInput cpIn
runUntilFail Failed _ = return Failed
 
-- | Run haskell compilation on all haskell files
runFullCompile :: Plugin
runFullCompile = applyPreprocessSources 
  >> convertModuleDataToFullCompileInput 
  >>= foldM runUntilFail Succeeded 
  >> return()

runSingleCompileInput :: CompileInput -> Shaker IO SuccessFlag
runSingleCompileInput cplInp = do
        lift $ putStrLn ""
        lift $ putStrLn $ concat ["--", "Compiling target : "++ show (compileInputTargetFiles cplInp) ,"--"]
        lift $ putStrLn $ concat ["--", "Arguments :"++ show (compileInputCommandLineFlags cplInp) ,"--"]
        lift $ defaultErrorHandler defaultDynFlags $ 
                    runGhc (Just libdir) $ ghcCompile cplInp 

