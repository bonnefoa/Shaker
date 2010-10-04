{-# LANGUAGE TemplateHaskell #-}
module Main
 where

import Shaker.SourceHelper
import Shaker.TestTH
import Shaker.Cabal.CabalInfoTest
import Shaker.Action.CompileTest
import Shaker.CliTest
import Shaker.ListenerTest
import Shaker.ParserTest
import Shaker.RegexTest
import Shaker.IoTest
import Shaker.ReflexiviteTest
import Shaker.SourceHelperTest
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

main :: IO()
main = defaultMain $(thListTestFramework)

