#!/usr/bin/bash

cat prelude.hs <(cat $1 | ./grass2hs) > a.hs
/usr/bin/time -v ghc -v2 a.hs -o a.out
