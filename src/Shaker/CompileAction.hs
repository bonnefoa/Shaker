module Shaker.CompileAction
 where

import GHC
import Outputable
import DynFlags ( defaultDynFlags )

libdir = "D:/dev/Haskell/2010.1.0.0/lib"

runCompile targetFile = defaultErrorHandler defaultDynFlags $ do
	runGhc (Just libdir) $ do
	dflags <- getSessionDynFlags
	setSessionDynFlags dflags {importPaths = ["src/","testsuite/tests/"], verbosity = 2, objectDir = Just "target" }
	target <- guessTarget targetFile Nothing
	setTargets [target]
	load LoadAllTargets
{-
	modSum <- getModSummary $ mkModuleName "B"
	p <- parseModule modSum
	t <- typecheckModule p
	d <- desugarModule t
	l <- loadModule d
	n <- getNamesInScope
	c <- return $ coreModule d
	g <- getModuleGraph
	mapM showModule g     
-}
--	return $ (parsedSource d,"/n-/n",  typecheckedSource d)

