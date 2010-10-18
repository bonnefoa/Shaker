module Shaker.CommonTest
 where 

import System.Directory
import Test.HUnit
import Control.Exception
import Shaker.Type
import Shaker.SourceHelper
import Shaker.GhcInterface
import Shaker.Cabal.CabalInfo

import Control.Monad.Reader(runReader)

import GHC
import GHC.Paths

runTestOnDirectory :: FilePath -> Assertion -> Assertion
runTestOnDirectory fp fun = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory fp
  finally fun (setCurrentDirectory oldDir)

testCompileInput ::IO CompileInput 
testCompileInput = fmap (mergeCompileInputsSources . compileInputs) defaultCabalInput 

testShakerInput :: IO ShakerInput
testShakerInput = defaultCabalInput

compileProject :: IO(CompileInput, [CompileFile])
compileProject = do
  cpIn <- testCompileInput 
  cfFlList <- constructCompileFileList cpIn
  _ <- runGhc (Just libdir) $ 
      ghcCompile $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
  return (cpIn, cfFlList)


