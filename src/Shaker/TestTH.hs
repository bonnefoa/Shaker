-- | Allow to dynamically construct a list of 
-- quickcheck properties and Hunit test with template haskell
module Shaker.TestTH
 where

import Shaker.Reflexivite
import Language.Haskell.TH
import Shaker.Cabal.CabalInfo

-- | Template for the list of quickcheck properties.
-- Currently generate a list of type [IO()] containing
--
-- /[putStrLn "prop_1" >> quickCheck prop_1, ...]/
thListProperties :: ExpQ 
thListProperties = do
  shIn <-runIO defaultCabalInput
  listAllProperties shIn

-- | Template for the list of hunit tests.
-- Currently generate a list of type [Test] containing
--
-- /[test1, test2]/
thListHunit :: ExpQ 
thListHunit = do 
  shIn <-runIO defaultCabalInput
  listHunit shIn

