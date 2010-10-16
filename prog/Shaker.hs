module Shaker
 where

import Shaker.Type
import Shaker.Conductor
import Shaker.Parser
import Shaker.Cabal.CabalInfo
import Control.Monad.Reader
import System.Environment

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
  lift $ either errAction (okAction shIn) either_command 
  return () 
  where 
        errAction = putStrLn . show 
        okAction :: ShakerInput -> Command -> IO()
        okAction shIn command = do
           putStrLn ("Executing " ++ show command) 
           withArgs [] (runReaderT (executeCommand (Just command)) shIn )
           return ()

