{-# LANGUAGE TemplateHaskell #-}
module RunTestTH
 where

import Test.HUnit
import Control.Monad.Trans
import Shaker.TestTH
import Test.QuickCheck
import Shaker.Cabal.CabalInfoTest
import Shaker.Action.CompileTest
import Shaker.CliTest
import Shaker.ListenerTest
import Shaker.ParserTest
import Shaker.RegexTest
import Shaker.IoTest
import Shaker.ReflexiviteTest
import Shaker.SourceHelperTest

runAll :: IO()
runAll = do 
  mapM_ liftIO propLists
  _ <- testLists
  return () 

propLists :: [IO()]
propLists = $(thListProperties)

testLists :: IO Counts
testLists = runTestTT  $ TestList $(thListHunit)


