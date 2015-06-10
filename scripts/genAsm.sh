#!/bin/bash

# set -x

BENCHMARK_DIR="$(realpath $(dirname ${BASH_SOURCE})/../benchmarks)"
MINI_EXE=minic

MINI_FLAG_OPTIONS=("--noOpt" "-noCP --noLVN" "--noUCR --noLVN" "--noUCR --noCP" "--noLVN"  "--noCP" "--noUCR" "")
MINI_FLAG_DESCS=("unopt"  "UCR" "CP" "LVN" "UCR_CP" "UCR_LVN" "CP_LVN" "opt")

ASM_DIR="dist/asm"

mkdir -p $ASM_DIR

for bench in $(ls $BENCHMARK_DIR)
do
   printf "$bench... "

   prefix="$BENCHMARK_DIR/$bench"
   miniFile=$prefix/$bench.mini

   for ((i=0; i<${#MINI_FLAG_OPTIONS[@]}; i++))
   do
      MINI_FLAGS=${MINI_FLAG_OPTIONS[i]}
      OUTPUT_FILE="$bench.s"
      ASM_DST=$ASM_DIR/$bench"_"${MINI_FLAG_DESCS[i]}.s

      $MINI_EXE $miniFile $MINI_FLAGS
      mv $OUTPUT_FILE $ASM_DST
   done


done
