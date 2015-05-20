#!/bin/bash

#set -e
set -u

MINI_EXE=mCompile
TMP_DIR=$(mktemp -d -p .)
BENCHMARK_DIR="$(realpath $(dirname ${BASH_SOURCE})/../benchmarks)"

cd $TMP_DIR

cleanup() {
    cd ..
    rm -rf $TMP_DIR
}

trap cleanup EXIT

runTest() {
    filename=$(basename "$1")
    filename="${filename%.*}"

    miniFile=$1
    input=$2
    output=$3

    $MINI_EXE $miniFile
    gcc "$filename".s -o $filename
    $filename < $input | diff - $output &> /dev/null
}

EXIT_STATUS=0

for bench in $(ls $BENCHMARK_DIR)
do
    printf "$bench... "
    prefix="$BENCHMARK_DIR/$bench"
    runTest $prefix/$bench.mini $prefix/input $prefix/output
    if [[ $? -eq 0 ]]
    then
        echo -e "\033[92m\xe2\x9c\x93\033[0m"
    else
        EXIT_STATUS=1
        echo -e "\033[91m\xe2\x9c\x97\033[0m"
    fi
done

exit $EXIT_STATUS
