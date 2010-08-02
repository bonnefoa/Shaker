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
import Control.Monad.Trans 
import Control.Monad.Reader

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile = asks compileInputs >>=  mapM runSingleCompileInput >> return ()

runSingleCompileInput :: CompileInput -> Shaker IO()
runSingleCompileInput cplInp = do
        lift $ putStrLn $ concat ["   --------- ", cfDescription cplInp," ---------"]
        targetFiles <- lift $ fillTargetIfEmpty cplInp >>= removeFileWithMain
        lift $ putStrLn $ concat ["   --------- ", "Compiling target : "++ show targetFiles," ---------"]
        _ <- lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ ghcCompile cplInp 
        return ()

 
runFullCompile :: Plugin
runFullCompile = getCompileInputForAllHsSources >>= \a -> 
  runSingleCompileInput a >> 
  return () 

