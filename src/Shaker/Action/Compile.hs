module Shaker.Action.Compile(
    runCompile
    ,runFullCompile
    ,ghcCompile
  )
 where

import Shaker.SourceHelper
import GHC
import DynFlags 
import GHC.Paths
import Shaker.Type
import Control.Monad.Reader

-- | Run haskell compilation on given CompileInput list
runCompile :: Plugin
runCompile = asks compileInputs >>= lift . foldM runUntilFail Succeeded >> return ()

runUntilFail :: SuccessFlag -> CompileInput -> IO SuccessFlag
runUntilFail Succeeded cpIn = runSingleCompileInput cpIn
runUntilFail Failed _ = return Failed
 
-- | Run haskell compilation on all haskell files
runFullCompile :: Plugin
runFullCompile = getFullCompileCompileInput >>= lift . foldM runUntilFail Succeeded >> return()

runSingleCompileInput :: CompileInput -> IO SuccessFlag
runSingleCompileInput cplInp = do
        putStrLn ""
        putStrLn $ concat ["--", "Compiling target : "++ show (cfTargetFiles cplInp) ,"--"]
        defaultErrorHandler defaultDynFlags $ 
                    runGhc (Just libdir) $ ghcCompile cplInp 

