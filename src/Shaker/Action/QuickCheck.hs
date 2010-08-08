module Shaker.Action.QuickCheck
 where

import Shaker.Type
import Shaker.Reflexivite

runQuickCheck :: Plugin
runQuickCheck = do 
  modMap <- runReflexivite 
  runFunction $ RunnableFunction ["Control.Monad","Test.QuickCheck","Prelude" ] $ "mapM (\\a -> putStrLn a >> quickCheck a) " ++ show (map cfPropName modMap)

