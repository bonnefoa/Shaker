module Main
 where

import Shaker.Conductor
import Shaker.Type
import Shaker.Config
import Control.Concurrent
import Control.Concurrent.MVar
import DynFlags

main = do
  inputMv <- newEmptyMVar 
  tokenMv <- newEmptyMVar  
  let inputState = InputState { input = inputMv, token =  tokenMv } 
      shIn = defaultInput { compileInput = defaultCompileInput{cfDynFlags = (myCompileFlags .defaultCompileFlags)
      }}
     in initThread inputState shIn

myCompileFlags :: (DynFlags->DynFlags)
myCompileFlags fl = fl {
          packageFlags = [ExposePackage "ghc"]
  }
