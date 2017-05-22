#!/bin/bash
ghc -O2 ../code/coordinator.hs ../code/service.hs 
mv ../code/coordinator ../coordinator/main
