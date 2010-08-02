{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}
module RunTestTH
 where

import Test.QuickCheck
import Test.HUnit
import Control.Monad.Trans
import Shaker.TestTH

runAll :: IO()
runAll = do 
  mapM_ liftIO propLists
  mapM_ liftIO testLists

propLists :: [IO()]
propLists = []
--propLists = $(listProperties)
  --  putStrLn "prop_completeWord ">> quickCheck prop_completeWord,
  

testLists :: [IO Counts]
testLists = [
  -- putStrLn "testRunCompileProject" >> runTestTT testRunCompileProject,
  ]

