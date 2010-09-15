-- | Register available actions and how they will be called
module Shaker.PluginConfig
 where

import qualified Data.Map as M (fromList)
import Shaker.Type
import Shaker.Action.Test
import Shaker.Action.Compile
import Shaker.Action.Standard

-- | The default plugin map contains mapping for compile, help and exit action 
defaultPluginMap :: PluginMap
defaultPluginMap = M.fromList $ map (\(a,b) -> (a, runStartAction >> b >> runEndAction)) list
  where list = [
                (Compile,runCompile ),
                (FullCompile,runFullCompile ),
                (Help,runHelp),
                (InvalidAction,runInvalidAction),
--                (Execute,runExecute),
                (QuickCheck,runQuickCheck),
                (IntelligentQuickCheck,runIntelligentQuickCheck),
                (IntelligentHUnit ,runIntelligentHUnit),
                (QuickHUnit, runQuickHUnit),
                (IntelligentQuickHUnit, runIntelligentQuickHUnit ),
                (HUnit,runHUnit),
                (Clean,runClean),
                (Quit,runExit)
              ]

defaultCommandMap :: CommandMap 
defaultCommandMap = M.fromList list
  where list = [
            ("compile",Compile),
            ("fullcompile",FullCompile),
            ("help", Help),
--            ("Execute", Execute),
            ("quickcheck",QuickCheck),
            ("iquickcheck",IntelligentQuickCheck),
            ("ihUnit",IntelligentHUnit),
            ("hunit",HUnit),
            ("quickhunit", QuickHUnit),
            ("iquickhunit", IntelligentQuickHUnit),
            ("clean",Clean),
            ("quit",Quit)
          ]

