#!/bin/bash
runs=$(ls -l data/full_runs | wc -l)
cd data/full_runs

for i in $(seq $runs); do
#
        awk -f col_row.awk ${i}_parsed_fitness.csv
done
slim ../../model.slim
# These awk commands replace all spaces with commas
for file in *.csv; do
        awk -F"," '{ gsub(/ /, ","); }1' "${file}" > "parsed_${file}"
done