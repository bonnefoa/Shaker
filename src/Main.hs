module Main
 where

import Shaker.Conductor
import Shaker.Type
import Shaker.Config
import Shaker.Cabal
import DynFlags

main :: IO()
main = do 
  inputState <- defaultInputState 
  initThread inputState $  defaultInput { compileInput = defaultCompileInput{cfDynFlags = (myCompileFlags .defaultCompileFlags)} }

myCompileFlags :: (DynFlags->DynFlags)
myCompileFlags fl = fl {
          packageFlags = [ExposePackage "ghc"]
  }


mainCabal :: IO()
mainCabal = do
  inputState <- defaultInputState
  cab <- defaultCabalInput 
  initThread inputState cab


    
