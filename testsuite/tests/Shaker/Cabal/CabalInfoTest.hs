module Shaker.Cabal.CabalInfoTest
 where 

import Control.Monad.Reader
import System.Directory
import Shaker.Action.Compile
import Shaker.Action.Standard
import Shaker.Type
import Shaker.CommonTest
import Test.HUnit
import Shaker.Cabal.CabalInfo
import DynFlags( DynFlags, packageFlags, importPaths ,PackageFlag (ExposePackage) , defaultDynFlags
        )

testParseCabalConfig :: Assertion
testParseCabalConfig =  runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do  
  shIn <- defaultCabalInput
  let cplInps@(cplLib:cplExe:[]) = shakerCompileInputs  shIn
  length cplInps == 2 @? "Should have two compile input, one executable and one library, got "++ show ( length cplInps)
  all (`elem` cfSourceDirs cplLib) ["dist/build/autogen","src"] @? "source dir should have src and dist/build/autogen, got " ++ show (cfSourceDirs cplLib)
  cfCommandLineFlags cplLib == ["-hide-all-packages", "-Wall"] @? "command line flags should be -Wall, got " ++ show ( cfCommandLineFlags cplLib)
  cfTargetFiles cplLib == ["CabalTest"]  @? "targetFiles should be CabalTest, got "++ show ( cfTargetFiles cplLib)
  cfTargetFiles cplExe == ["src/Main.hs"]  @? "targetFiles should be src/Main.hs, got "++ show ( cfTargetFiles cplExe)
  let dFlags = cfDynFlags cplExe defaultDynFlags
  all (`elem` importPaths dFlags) ["dist/build/autogen","src"] @? "importPaths should be contains src and dist/build/autogen, got "++ show (importPaths dFlags)
  ExposePackage "ghc" `elem` packageFlags dFlags @? "Expected : ExposePackage ghc. No show instance so figure it yourself... (/me being lazy)" 
  let (ListenerInput (_:srcLib:[]) _) = listenerInput shIn
  dir srcLib == "src" @? "Expected : src, got " ++ show srcLib

testConditionalFlag :: Assertion
testConditionalFlag = runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do  
  shIn <- testShakerInput
  let (_:cplExe:[]) = shakerCompileInputs  shIn
  let packageList = packageFlags $ cfDynFlags cplExe defaultDynFlags
  all (`elem` packageList) [ExposePackage "mtl",ExposePackage "bytestring-mmap"] @? "mtl and bytestring-mmap should be exposed package, got "++ show (map showExposed packageList)
  ExposePackage "shaker" `notElem` packageList @? "shaker should not be present as exposed package"

showExposed :: PackageFlag -> String
showExposed (ExposePackage str) = str
showExposed _ = ""

testInvalidMainShouldBeExcluded :: Assertion
testInvalidMainShouldBeExcluded =  runTestOnDirectory "testsuite/tests/resources/invalidMain" $ do
 shIn <- defaultCabalInput
 let (cplExe:[]) = shakerCompileInputs  shIn
 cfTargetFiles cplExe == ["src/Main.hs"] @? "since tests/Main.hs is invalid, should have only src/Main.hs, got " ++ show (cfTargetFiles cplExe)

testCompileWithLocalSource :: Assertion
testCompileWithLocalSource =  runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do
 shIn <- defaultCabalInput
 runReaderT runCompile shIn
 ex <- doesFileExist "dist/shakerTarget/Main.o" 
 runReaderT runClean shIn
 ex2 <- doesFileExist "dist/shakerTarget/Main.o" 
 ex && not ex2 @? "file main should exist and be cleaned"


testProjectCabalContentWithLocalSource :: Assertion
testProjectCabalContentWithLocalSource = 
    runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do
    shIn <- defaultCabalInput
    let cplInps@(cplInp:_) = shakerCompileInputs  shIn
    length cplInps == 1 @? "Should have one compile input, got "++ show (length cplInps)
    let targs = cfTargetFiles cplInp
    targs == ["./noHsSource.hs"] @? "Expected [\"./noHsSource.hs\"] got " ++ show cplInp

