module Main
 where

import Shaker.Conductor
import Shaker.Config
import Shaker.Cabal.CabalInput
import Control.Monad.Reader

main :: IO()
main = do
  inputState <- defaultInputState
  cab <- defaultCabalInput 
  runReaderT (initThread inputState) cab

    
