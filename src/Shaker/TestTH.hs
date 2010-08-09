module Shaker.TestTH
 where

import Shaker.Reflexivite
import Language.Haskell.TH
import Shaker.Cabal.CabalInfo

thListProperties :: ExpQ 
thListProperties = do
  shIn <-runIO defaultCabalInput
  listProperties shIn

thListHunit :: ExpQ 
thListHunit = do 
  shIn <-runIO defaultCabalInput
  listHunit shIn

