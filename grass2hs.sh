#!/usr/bin/bash

cat prelude.hs <(cat $1 | ./grass2hs) > a.hs
ghc a.hs -o a.out
