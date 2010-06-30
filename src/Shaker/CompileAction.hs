module Shaker.CompileAction
 where

import GHC
import Outputable
import DynFlags ( defaultDynFlags )
import GHC.Paths

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

