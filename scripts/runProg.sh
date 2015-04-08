#!/bin/bash

set -e

exe=$0
file=$1

if [[ $exe != scripts/* ]]
then
    echo "Please run from above scripts directory"
    exit 1
fi

if [[ -z $file ]]
then
    echo "Please specify the test filename without an extension"
    exit 1
fi

cd parser
java Mini ../mini/$file.mini > ../json/$file.json
cd ..
cabal run -- json/$file.json ${@:2}
