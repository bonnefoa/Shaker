-- | Allow to dynamically construct a list of 
-- quickcheck properties and Hunit test with template haskell
module Shaker.TestTH
 where

import Shaker.Reflexivite
import Language.Haskell.TH
import Shaker.CabalInfo

-- | Template for the test group.
-- Currently generate a list of type [Test] with a test group per module
--
thListTestFramework :: ExpQ 
thListTestFramework = do
  shIn <- runIO defaultCabalInput 
  listAllTestFrameworkGroupList shIn

