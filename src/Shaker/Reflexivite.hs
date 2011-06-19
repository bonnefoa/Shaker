module Shaker.Reflexivite (
  RunnableFunction(..)
  -- * Collect module information functions
  ,runFunction
  )
 where

import Control.Exception as C
import Control.Monad.Reader
import Data.Maybe
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
         _ <- setSessionDynFlags (addLibraryToDynFlags listInstalledPkgId (dopt_set dflags Opt_HideAllPackages))
         _ <- ghcCompile cpIn
         configureContext importModuleList
         value <- compileExpr fun
         do let value' = unsafeCoerce value :: a
            return value'
  _ <- lift $ handleActionInterrupt dynFun
  return ()
  where
        genTuple :: ModSummary -> (Module, Maybe (ImportDecl RdrName))
        genTuple modSummary = (ms_mod modSummary, Nothing)
        configureContext [] = do
          modGraph <- getModuleGraph
          setContext [] (map genTuple modGraph)
        configureContext imports = do
          mods <- mapM (\a -> findModule (mkModuleName a) Nothing) imports
          setContext [] $ map (\m -> (m, Nothing) ) mods

handleActionInterrupt :: IO() -> IO()
handleActionInterrupt =  C.handle catchAll
  where catchAll :: C.SomeException -> IO ()
        catchAll e = putStrLn ("Shaker caught " ++ show e ) >>  return ()

