module Shaker.Listener
 where

import Shaker.Io
import Control.Concurrent

listenerLoop fileListenInfo fileInfo m = 
  listModifiedAndCreatedFiles fileListenInfo fileInfo >>= \modifiedFiles
  putMVar m modifiedFiles >>
  threadDelay 1000 >>
  listenerLoop fileListenInfo 



