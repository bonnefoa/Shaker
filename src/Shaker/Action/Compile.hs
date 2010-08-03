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

-- |Run haskell compilation on given file input 
runCompile :: Plugin
runCompile = asks compileInputs >>= mapM runSingleCompileInput >> return ()
 
runFullCompile :: Plugin
runFullCompile = do
  cpIn <- mergeCompileInputsSources 
  cfFlList <- lift $ constructCompileFileList cpIn
  runSingleCompileInput $ ( removeFileWithMain cfFlList . setAllHsFilesAsTargets cfFlList ) cpIn

runSingleCompileInput :: CompileInput -> Shaker IO()
runSingleCompileInput cplInp = do
        lift $ putStrLn $ concat ["   --------- ", cfDescription cplInp," ---------"]
        lift $ putStrLn $ concat ["   --------- ", "Compiling target : "++ show (cfTargetFiles cplInp) ," ---------"]
        _ <- lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ ghcCompile cplInp 
        return ()

