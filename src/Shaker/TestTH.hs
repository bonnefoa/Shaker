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
getQuickCheckProperty' modMap = map getSingleQuickCheck $ cfPropName modMap

getSingleQuickCheck :: String -> Exp
getSingleQuickCheck propName = DoE [NoBindS printName, NoBindS quickCall] 
  where quickCall = (AppE (VarE $ mkName "quickCheck" ) . VarE . mkName) propName
        printName = AppE (VarE $ mkName "putStrLn") (LitE (StringL propName)) 

listHunit :: ExpQ 
listHunit = do 
  modMaps <- runIO $ defaultCabalInput >>= runReaderT runReflexivite
  return $ ListE $ getHunit modMaps

getHunit :: [ModuleMapping] -> [Exp]
getHunit = concatMap getHunit'

getHunit' :: ModuleMapping -> [Exp]
getHunit' modMap = map (VarE . mkName) $ cfHunitName modMap

