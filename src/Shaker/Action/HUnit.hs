module Shaker.Action.HUnit
 where

import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Trans
import Control.Monad.Reader
import Language.Haskell.TH

runHUnit :: Plugin
runHUnit = do 
  modMap <- runReflexivite 
  let filteredModMap = filter (not . null . cfHunitName) modMap
  let modules = ["Test.HUnit","Prelude" ] ++ map cfModuleName filteredModMap 
  expression <- asks listHunit
  resolvedExp <- lift $ runQ expression
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction modules ("runTestTT  $ TestList $ " ++ function) 

