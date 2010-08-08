module Shaker.Action.QuickCheck
 where

import Shaker.Type
import Shaker.Reflexivite

runQuickcheck :: Plugin
runQuickcheck = do 
  modMap <- runReflexivite 
  runFunction $ RunnableFunction ["Control.Monad","Test.QuickCheck","Prelude" ] $ "mapM (\\a -> putStrLn a >> quickCheck a) " ++ show (map cfPropName modMap)

