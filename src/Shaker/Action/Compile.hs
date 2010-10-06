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
runCompile = asks compileInputs >>= mapM (lift . runSingleCompileInput )  >> return ()
 
-- | Run haskell compilation on all haskell files
runFullCompile :: Plugin
runFullCompile = getFullCompileCompileInput >>= lift . runSingleCompileInput 

runSingleCompileInput :: CompileInput -> IO()
runSingleCompileInput cplInp = do
        putStrLn $ concat ["   --------- ", cfDescription cplInp," ---------"]
        putStrLn $ concat ["   --------- ", "Compiling target : "++ show (cfTargetFiles cplInp) ," ---------"]
        _ <- defaultErrorHandler defaultDynFlags $ 
                    runGhc (Just libdir) $ ghcCompile cplInp 
        return ()

