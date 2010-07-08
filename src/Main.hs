module Main
 where

import Shaker.Conductor
import Shaker.Config
import Shaker.Cabal

main :: IO()
main = do
  inputState <- defaultInputState
  cab <- defaultCabalInput 
  initThread inputState cab


    
