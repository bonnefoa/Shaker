module Shaker.CommonTest
 where 

import Data.Monoid
import System.Directory
import Test.HUnit
import Control.Exception
import Shaker.Type
import Shaker.SourceHelper
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

testShakerInput :: IO ShakerInput
testShakerInput = defaultCabalInput

compileProject :: IO(CompileInput, [CompileFile])
compileProject = do
  cpIn <- testCompileInput 
  shIn <- testShakerInput 
  cfFlList <- runReaderT constructCompileFileList shIn
  _ <- runGhc (Just libdir) $ 
      ghcCompile $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
  return (cpIn, cfFlList)

exposePackageId :: PackageFlag -> String
exposePackageId (ExposePackageId v) = v
exposePackageId _ = ""

