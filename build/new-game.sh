#!/bin/bash
ghc -O2 ../code/new-game.hs ../code/service.hs ../code/json.hs
mv ../code/new-game ../new-game/main
