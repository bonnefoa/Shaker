module Shaker.Cabal.CabalInfoTest
 where 

import Control.Monad.Reader
import System.Directory
import Shaker.Action.Compile
import Shaker.Action.Clean
import Shaker.Type
import Shaker.CommonTest
import Test.HUnit
import Shaker.Cabal.CabalInfo
import DynFlags( DynFlags, packageFlags, importPaths ,PackageFlag (ExposePackage) , defaultDynFlags
        )

testParseCabalConfig :: Test
testParseCabalConfig = TestCase $ runTestOnDirectory "testsuite/tests/resources/cabalTest" $ do  
  shIn <- defaultCabalInput
  let cplInps@(cplLib:cplExe:[]) = compileInputs shIn
  length cplInps == 2 @? "Should have two compile input, one executable and one library, got "++ show ( length cplInps)
  cfSourceDirs cplLib == ["src"] @? "source dir should be src, got " ++ show (cfSourceDirs cplLib)
  cfCommandLineFlags cplLib == ["-Wall"] @? "command line flags should be -Wall, got " ++ show ( cfCommandLineFlags cplLib)
  cfTargetFiles cplLib == ["CabalTest"]  @? "targetFiles should be CabalTest, got "++ show ( cfTargetFiles cplLib)
  cfTargetFiles cplExe == ["src/Main.hs"]  @? "targetFiles should be src/Main.hs, got "++ show ( cfTargetFiles cplExe)
  let dFlags = cfDynFlags cplExe defaultDynFlags
  importPaths dFlags == ["src"] @? "importPaths should be src, got "++ show (importPaths dFlags)
  packageFlags dFlags == [ExposePackage "ghc"] @? "Expected : ExposePackage ghc. No show instance so figure it yourself... (/me being lazy)" 
  let (ListenerInput (flLib:[]) _) = listenerInput shIn
  dir flLib == "src" @? "Expected : src, got " ++ show  flLib

testInvalidMainShouldBeExcluded :: Test
testInvalidMainShouldBeExcluded = TestCase $ runTestOnDirectory "testsuite/tests/resources/invalidMain" $ do
 shIn <- defaultCabalInput
 let (cplExe:[]) = compileInputs shIn
 cfTargetFiles cplExe == ["src/Main.hs"] @? "since tests/Main.hs is invalid, should have only src/Main.hs, got " ++ show (cfTargetFiles cplExe)

testCompileWithLocalSource :: Test
testCompileWithLocalSource = TestCase $ runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do
 shIn <- defaultCabalInput
 runReaderT runCompile shIn
 ex <- doesFileExist "target/Main.o" 
 runReaderT runClean shIn
 ex2 <- doesFileExist "target/Main.o" 
 ex && not ex2 @? "file main should exist and be cleaned"


testProjectCabalContentWithLocalSource :: Test
testProjectCabalContentWithLocalSource = TestCase $
    runTestOnDirectory "testsuite/tests/resources/noSourceConfig" $ do
    shIn <- defaultCabalInput
    let cplInps@(cplInp:_) = compileInputs shIn
    length cplInps == 1 @? "Should have one compile input, got "++ show (length cplInps)
    let targs = cfTargetFiles cplInp
    targs == ["./noHsSource.hs"] @? "Expected [\"./noHsSource.hs\"] got " ++ show cplInp

