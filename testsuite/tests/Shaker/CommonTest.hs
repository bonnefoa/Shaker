module Shaker.CommonTest
 where 

import Data.Monoid
import System.Directory
import Test.HUnit
import Control.Exception

import Shaker.ModuleData
import Shaker.Type
import Shaker.GhcInterface
import Shaker.Cabal.CabalInfo

import Control.Monad.Reader
import DynFlags 

import GHC (runGhc)
import GHC.Paths

runTestOnDirectory :: FilePath -> Assertion -> Assertion
runTestOnDirectory fp fun = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory fp
  finally fun (setCurrentDirectory oldDir)

testCompileInput ::IO CompileInput 
testCompileInput = fmap (mconcat . shakerCompileInputs ) defaultCabalInput 

mergedCompileInput :: IO CompileInput
mergedCompileInput = testShakerInput >>= runReaderT getMergedCompileInput 

testShakerInput :: IO ShakerInput
testShakerInput = defaultCabalInput

compileProject :: IO CompileInput
compileProject = do
  shIn <- testShakerInput 
  cpIn <- runReaderT getMergedCompileInput shIn
  _ <- runGhc (Just libdir) $ 
      ghcCompile cpIn
  return cpIn

exposePackageId :: PackageFlag -> String
exposePackageId (ExposePackageId v) = v
exposePackageId _ = ""

