#!/bin/bash
ghc -O2 ../code/backend.hs
mv ../code/backend ../../public_html/backend.cgi
