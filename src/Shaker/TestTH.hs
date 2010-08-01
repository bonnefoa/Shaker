{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

module Shaker.TestTH
 where

import Language.Haskell.TH
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Text.Regex.Posix
import Maybe
import Language.Haskell.Exts.Extension
import Language.Haskell.Extract 


importModules :: ExpQ
importModules = undefined

listProperties :: ExpQ 
listProperties = undefined

listHunit :: ExpQ 
listHunit = undefined


