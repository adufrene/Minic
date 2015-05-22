#!/bin/bash

#set -e
set -u

MINI_EXE=minic
BENCHMARK_DIR="$(realpath $(dirname ${BASH_SOURCE})/../benchmarks)"
TIMEOUT=2m
REMOTE_LOGIN=''

cleanup() {
    cd ..
    rm -rf $TMP_DIR
}

while getopts r: opt
do
    case $opt in
        r)
            REMOTE_LOGIN=$OPTARG
            ;;
        \?)
#            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done

testSSH() {
    if [[ -z $REMOTE_LOGIN ]]
    then
        return 0
    else
        ssh $REMOTE_LOGIN 'true'
    fi
}

if [[ -n $REMOTE_LOGIN ]]
then
    echo "Testing ssh..."
    ssh $REMOTE_LOGIN 'true'
    if [[ $? -eq 0 ]]
    then
        echo "Success!"
    else
        echo "Remote login failed"
        exit 1
    fi
fi

TMP_DIR=$(mktemp -d -p .)
cd $TMP_DIR
trap cleanup EXIT

runTest() {
    filename=$(basename "$1")
    filename="${filename%.*}"

    miniFile=$1
    input=$2
    output=$3
    tempFile="$filename".tmp

    timeout $TIMEOUT $MINI_EXE $miniFile --noOpt

    if [[ $? -ne 0 ]]
    then
        printf "timeout "
        return 1
    fi

    if [[ -n $REMOTE_LOGIN ]]
    then
        remote_input=$(basename "$input")

        scp -q "$filename.s" "$REMOTE_LOGIN:"
        scp -q "$input" "$REMOTE_LOGIN:"
        ssh $REMOTE_LOGIN "gcc $filename.s -o $filename && ./$filename < $remote_input | cat &> $tempFile"
        scp -q "$REMOTE_LOGIN:$tempFile" .
        ssh $REMOTE_LOGIN "rm $filename $filename.s $remote_input $tempFile"
    else
        gcc "$filename.s" -o $filename && $filename < $input | cat &> $tempFile
    fi

    diff $tempFile $output &> /dev/null
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
