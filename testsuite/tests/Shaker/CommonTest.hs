module Shaker.CommonTest
 where 

import Control.Exception
import Control.Monad.Reader
import Data.Monoid
import DynFlags 
import GHC.Paths
import GHC (runGhc)
import Shaker.CabalInfo
import Shaker.GhcInterface
import Shaker.ModuleData
import Shaker.Type
import System.Directory
import Test.HUnit

runTestOnDirectory :: FilePath -> Assertion -> Assertion
runTestOnDirectory fp fun = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory fp
  finally fun (setCurrentDirectory oldDir)

testCompileInput ::IO CompileInput 
testCompileInput = fmap (mconcat . shakerCompileInputs ) defaultCabalInput 

mergedCompileInput :: IO CompileInput
mergedCompileInput = testShakerInput >>= runReaderT getNonMainCompileInput 

testShakerInput :: IO ShakerInput
testShakerInput = defaultCabalInput

compileProject :: IO CompileInput
compileProject = do
  shIn <- testShakerInput 
  cpIn <- runReaderT getNonMainCompileInput shIn
  _ <- runGhc (Just libdir) $ 
      ghcCompile cpIn
  return cpIn

exposePackageId :: PackageFlag -> String
exposePackageId (ExposePackageId v) = v
exposePackageId _ = ""

