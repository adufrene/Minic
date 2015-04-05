#!/bin/bash

# tests the front end of the mini complier by parsing the json, re-encoding it, then checking that
# we get the same JSON back. This script creates temporary files in dist/testParse. Assumes that
# the parser writes two lines of useless junk, which is ignored.

# usage: scripts/testParse.sh


cabal configure

tests=(1.json 2.json lval.json ret.json) # files to test
testDir=json # directory of test input files
outDir=dist/testParse # where to save output

mkdir $outDir

for t in ${tests[@]}; do  # runs the parser, ignores first two lines of random junk, then compares
   echo $testDir/$t...
   cabal run Parser $testDir/$t testParse > $outDir/$t
   tail -n +3 $outDir/$t > $outDir/tail_$t
   node scripts/compareJSON.js $testDir/$t $outDir/tail_$t
done
