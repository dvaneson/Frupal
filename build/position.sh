#!/bin/bash
ghc -O2 ../code/position.hs ../code/service.hs
mv ../code/position ../position/main
