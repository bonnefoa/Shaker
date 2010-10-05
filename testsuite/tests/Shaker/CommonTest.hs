module Shaker.CommonTest
 where 

import System.Directory
import Test.HUnit
import Control.Exception
import Shaker.Type
import Shaker.SourceHelper
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
testCompileInput = defaultCabalInput >>= return . mergeCompileInputsSources . compileInputs

initializeEmptyCompileInput :: CompileInput 
initializeEmptyCompileInput = CompileInput {
  cfSourceDirs = []
  ,cfDescription = ""
  ,cfCompileTarget = ""
  ,cfDynFlags = id
  ,cfCommandLineFlags =[]
  ,cfTargetFiles = []
}

testShakerInput :: IO ShakerInput
testShakerInput = defaultCabalInput

compileProject :: IO(CompileInput, [CompileFile])
compileProject = do
  cpIn <- testCompileInput 
  cfFlList <- constructCompileFileList cpIn
  _ <- runGhc (Just libdir) $ 
      ghcCompile $ runReader (fillCompileInputWithStandardTarget cpIn) cfFlList
  return (cpIn, cfFlList)


