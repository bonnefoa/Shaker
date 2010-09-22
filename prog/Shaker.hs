module Main
 where

import Shaker.Type
import Shaker.Conductor
import Shaker.Config
import Shaker.Parser
import Shaker.Cabal.CabalInfo
import Control.Monad.Reader
import System( getArgs )

main :: IO()
main = do
  args <- getArgs
  cab <- defaultCabalInput 
  if null args 
    then runReaderT initThread cab
    else 
         runReaderT ( runArgumentAction $ concat args) cab >> return ()
  
runArgumentAction :: String -> Shaker IO ()
runArgumentAction args = do 
  shIn <- ask 
  let either_command = parseCommand args shIn
  either errAction okAction either_command 
  return () 
  where 
        errAction = lift . putStrLn . show 
        okAction :: Command -> Shaker IO()
        okAction command = executeCommand (Just command) >> return ()

