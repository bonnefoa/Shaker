module Shaker.CommonTest
 where 

import System.Directory
import Test.HUnit
import Control.Exception
import Shaker.Type
import Shaker.Config

runTestOnDirectory :: FilePath -> Assertion -> Assertion
runTestOnDirectory fp fun = do
  oldDir <- getCurrentDirectory 
  setCurrentDirectory fp
  finally fun (setCurrentDirectory oldDir)

initializeEmptyCompileInput :: CompileInput 
initializeEmptyCompileInput = CompileInput {
  cfSourceDirs = []
  ,cfDescription = ""
  ,cfCompileTarget = ""
  ,cfDynFlags = id
  ,cfCommandLineFlags =[]
  ,cfTargetFiles = []
}

tShakerInput :: ShakerInput
tShakerInput = defaultInput {
  compileInputs = [defaultCompileInput {
       cfCommandLineFlags = ["-package ghc"]
     }]
}


