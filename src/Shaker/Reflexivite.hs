module Shaker.Reflexivite (
  RunnableFunction(..)
  -- * Collect module information functions
  ,runFunction
  ,searchInstalledPackageId
  )
 where

import Control.Arrow
import Control.Exception as C
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Distribution.InstalledPackageInfo
import Distribution.Simple.PackageIndex
import DynFlags
import GHC
import GHC.Paths
import Shaker.Action.Compile
import Shaker.GhcInterface
import Shaker.Type 
import Unsafe.Coerce

data RunnableFunction = RunnableFunction {
  runnableFunctionModule :: [String]
  ,runnableLibrairies :: [String]
  ,runnableFunctionFunction :: String -- The function name. Should have IO() as signature
}
 deriving Show

-- | Compile, load and run the given function
runFunction :: CompileInput -> RunnableFunction -> Shaker IO()
runFunction cpIn (RunnableFunction importModuleList listLibs fun) = do
  listInstalledPkgId <- fmap catMaybes (mapM searchInstalledPackageId listLibs)
  dynFun <- lift $ runGhc (Just libdir) $ do
         dflags <- getSessionDynFlags
         _ <- setSessionDynFlags (addShakerLibraryAsImport listInstalledPkgId (dopt_set dflags Opt_HideAllPackages))
         _ <- ghcCompile cpIn 
         configureContext importModuleList
         value <- compileExpr fun
         do let value' = unsafeCoerce value :: a
            return value'
  _ <- lift $ handleActionInterrupt dynFun
  return ()
  where 
        addNothing a = (a, Nothing)
        configureContext [] = getModuleGraph >>= \mGraphs ->  setContext [] $ map (ms_mod >>> addNothing) mGraphs
        configureContext imports = mapM (\a -> findModule (mkModuleName a)  Nothing ) imports >>= \mGraphs -> setContext [] $ map addNothing mGraphs

addShakerLibraryAsImport :: [String] -> DynFlags -> DynFlags
addShakerLibraryAsImport listInstalledPkgId dflags = dflags {
    packageFlags = nub $ map ExposePackageId listInstalledPkgId ++ oldPackageFlags
  }
  where oldPackageFlags = packageFlags dflags

searchInstalledPackageId :: String -> Shaker IO (Maybe String)
searchInstalledPackageId pkgName = do 
  pkgIndex <- asks shakerPackageIndex
  let srchRes = searchByName pkgIndex pkgName 
  return $ processSearchResult srchRes
  where processSearchResult None = Nothing
        processSearchResult (Unambiguous a) = Just $ installedPackageId >>> installedPackageIdString $ last a
        processSearchResult (Ambiguous (a:_)) = Just $ installedPackageId >>> installedPackageIdString $ last a
        processSearchResult _ = Nothing

handleActionInterrupt :: IO() -> IO()
handleActionInterrupt =  C.handle catchAll
  where catchAll :: C.SomeException -> IO ()
        catchAll e = putStrLn ("Shaker caught " ++ show e ) >>  return () 

