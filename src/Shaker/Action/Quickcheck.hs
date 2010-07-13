module Shaker.Action.Quickcheck
 where

import GHC
import DynFlags 
import GHC.Paths
import Shaker.Io
import Shaker.Type
import Control.Monad.Trans 
import Control.Monad.Reader
{-
runQuickcheck :: Shaker IO String
runQuickcheck = do
        (CompileInput sourceDir targetInput procFlags strflags) <-  asks compileInput 
        (ListenerInput fli _) <- asks listenerInput 
        targetFiles <-  lift $ recurseMultipleListFiles fli
        lift $ defaultErrorHandler defaultDynFlags $ 
                       runGhc (Just libdir) $ do
                       dflags <- getSessionDynFlags
                       (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc strflags)
	               _ <- setSessionDynFlags $ procFlags $ setSourceAndTarget sourceDir targetInput newFlags
                       target <- mapM (`guessTarget` Nothing) targetFiles
                       setTargets target
        	       _ <- load LoadAllTargets
                       modSum <- getModSummary $ mkModuleName "B"
                       p <- parseModule modSum
                       t <- typecheckModule p
                       d <- desugarModule t
                       l <- loadModule d
                       n <- getNamesInScope
                       c <- return $ coreModule d
                       g <- getModuleGraph
                       mapM showModule g     
                       print $ showSDoc ( ppr (parsedSource d,"/n-----/n",  typecheckedSource d)        res )
-}

example = defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    (newFlags,_,_) <- parseDynamicFlags dflags (map noLoc ["-itestsuite/tests","-isrc/","-package ghc"])
    setSessionDynFlags newFlags
    target <- guessTarget "Shaker.CliTest" Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "Shaker.CliTest"
    p <- parseModule modSum
 --   liftIO $ putStrLn $ show p 
    t <- typecheckModule p
--    liftIO $ putStrLn $ show t 
    d <- desugarModule t
    l <- loadModule d
    n <- getNamesInScope
    c <- return $ coreModule d
    g <- getModuleGraph
    mapM showModule g     
    return $ (parsedSource d,"/n-----/n",  typecheckedSource d)

setSourceAndTarget :: [String] -> String ->DynFlags -> DynFlags
setSourceAndTarget sources target dflags = dflags{
    importPaths = sources
    ,objectDir = Just target
    ,hiDir = Just target
  }

