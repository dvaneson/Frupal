#!/bin/bash
ghc -O2 ../code/coordinator.hs ../code/service.hs ../code/json.hs
mv ../code/coordinator ../coordinator/main
