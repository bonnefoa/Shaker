{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

module Shaker.TestTH
 where

import Shaker.Reflexivite
import Language.Haskell.TH
import Control.Monad.Reader
import Shaker.Cabal.CabalInfo

importModules :: ExpQ
importModules = undefined

listProperties :: ExpQ 
listProperties = do
  modMaps <- runIO $ defaultCabalInput >>= runReaderT runReflexivite
  return $ ListE $ getQuickCheckProperty modMaps

getQuickCheckProperty :: [ModuleMapping] -> [Exp]
getQuickCheckProperty = concat . (map getQuickCheckProperty')

getQuickCheckProperty' :: ModuleMapping -> [Exp]
getQuickCheckProperty' modMap = map (\pName -> 
  AppE (VarE (mkName "quickCheck") ) (VarE (mkName pName) ) ) $ cfPropName modMap

listHunit :: ExpQ 
listHunit = undefined

