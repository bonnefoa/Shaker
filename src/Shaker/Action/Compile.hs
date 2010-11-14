module Shaker.Action.Compile(
    runCompile
    ,runFullCompile
    ,ghcCompile
  )
 where

import Shaker.Type
import Shaker.ModuleData
import Shaker.GhcInterface

import GHC
import DynFlags 
import GHC.Paths
import Control.Monad.Reader

-- | Run haskell compilation on given CompileInput list
runCompile :: Plugin
runCompile = asks shakerCompileInputs >>= lift . foldM runUntilFail Succeeded >> return ()

runUntilFail :: SuccessFlag -> CompileInput -> IO SuccessFlag
runUntilFail Succeeded cpIn = runSingleCompileInput cpIn
runUntilFail Failed _ = return Failed
 
-- | Run haskell compilation on all haskell files
runFullCompile :: Plugin
runFullCompile = convertModuleDataToFullCompileInput >>= lift . foldM runUntilFail Succeeded >> return()

runSingleCompileInput :: CompileInput -> IO SuccessFlag
runSingleCompileInput cplInp = do
        putStrLn ""
        putStrLn $ concat ["--", "Compiling target : "++ show (compileInputTargetFiles cplInp) ,"--"]
        defaultErrorHandler defaultDynFlags $ 
                    runGhc (Just libdir) $ ghcCompile cplInp 

