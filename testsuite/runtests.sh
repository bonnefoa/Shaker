#/bin/sh
cd ..
runhaskell -packageghc -isrc/ -itestsuite/tests/ testsuite/tests/Main.hs

