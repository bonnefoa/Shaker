module Shaker.Action.QuickCheck
 where

import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Trans
import Control.Monad.Reader
import Language.Haskell.TH

runQuickCheck :: Plugin
runQuickCheck = do 
  modMap <- runReflexivite 
  let filteredModMap = filter (\a ->  not . null $ cfPropName a) modMap
  let modules = ["Test.QuickCheck","Prelude" ] ++ (map cfModuleName filteredModMap )
  expression <- asks listProperties 
  resolvedExp <- lift $ runQ expression
  let function =  filter (/= '\n') $ pprint resolvedExp
  lift $ putStrLn function
  runFunction $ RunnableFunction modules ("sequence_ " ++ function ++ " >> return () ") 

