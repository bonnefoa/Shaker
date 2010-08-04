module Shaker.TestTH
 where

import Shaker.Reflexivite
import Language.Haskell.TH
import Control.Monad.Reader
import Shaker.Cabal.CabalInfo

listProperties :: ExpQ 
listProperties = do
  modMaps <- runIO $ defaultCabalInput >>= runReaderT runReflexivite
  return $ ListE $ getQuickCheckProperty modMaps

getQuickCheckProperty :: [ModuleMapping] -> [Exp]
getQuickCheckProperty = concatMap getQuickCheckProperty'

getQuickCheckProperty' :: ModuleMapping -> [Exp]
getQuickCheckProperty' modMap = map (AppE (VarE (mkName "quickCheck") ) . VarE . mkName) $ cfPropName modMap

listHunit :: ExpQ 
listHunit = do 
  modMaps <- runIO $ defaultCabalInput >>= runReaderT runReflexivite
  return $ ListE $ getHunit modMaps

getHunit :: [ModuleMapping] -> [Exp]
getHunit = concatMap getHunit'

getHunit' :: ModuleMapping -> [Exp]
getHunit' modMap = map (VarE . mkName) $ cfHunitName modMap

