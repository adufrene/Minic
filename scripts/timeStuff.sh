#!/bin/bash

# set -x

BENCHMARK_DIR=benchmarks #"$(realpath $(dirname ${BASH_SOURCE})/../benchmarks)"
MINI_EXE=minic

MINI_FLAG_OPTIONS=("--noOpt" "-noCP --noLVN" "--noUCR --noLVN" "--noUCR --noCP" "--noLVN"  "--noCP" "--noUCR" "")
MINI_FLAG_DESCS=("unopt"  "UCR" "CP" "LVN" "UCR_CP" "UCR_LVN" "CP_LVN" "opt")

ASM_DIR="dist/asm"

mkdir -p $ASM_DIR

benchmarks=$(ls $BENCHMARK_DIR)

echo "benchmarks: $benchmarks"

header=""
for desc in ${MINI_FLAG_DESCS[@]}
do
   header=$header$desc,
done

echo $header > temp2.txt

for bench in $benchmarks
do
   if [[ $bench = "creativeBenchMarkName" ]]
   then
      continue
   fi

   echo -ne "$bench"

   prefix="$BENCHMARK_DIR/$bench"
   results="$bench.csv"

   echo $header > $results 

   for ((testNo=0; testNo<5; testNo++))
   do
      echo -ne "\n\ttest $testNo: "

      for ((i=0; i<${#MINI_FLAG_OPTIONS[@]}; i++))
      do
         MINI_FLAGS=${MINI_FLAG_OPTIONS[i]}
         OUTPUT_FILE="$bench.s"
         ASM_DST=$ASM_DIR/$bench"_"${MINI_FLAG_DESCS[i]}.s

         echo -ne "${MINI_FLAG_DESCS[i]}... "

         asmFile="dist/asm/"$bench"_"${MINI_FLAG_DESCS[i]}.s

         input="$prefix"/input
         output=$prefix/output

         gcc $asmFile

         TIMEFORMAT='%3R'
         realTime=`(time ./a.out < $input > temp.txt) 2>&1`
         # x=`(\time -f "%e" ./a.out < $input > temp.txt) 2>&1`
         diff temp.txt $output &> /dev/null

         if [[ $? -ne 0 ]]
         then
           echo -e "\033[91m\xe2\x9c\x97\033[0m"
           exit 1
         fi

         echo -n $realTime, >> $results
      done

      echo -e "" >> $results
   done
   echo ""
done
