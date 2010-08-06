module Main
 where

import Shaker.Conductor
import Shaker.Config
import Shaker.Parser
import Shaker.Cabal.CabalInfo
import Control.Monad.Reader
import System( getArgs )


main :: IO()
main = do
  args <- getArgs
  inputState <- defaultInputState
  cab <- defaultCabalInput 
  if (null args) 
    then runReaderT (initThread inputState) cab
    else runReaderT ( executeCommand . parseCommand cab $ concat args)  cab 
 
