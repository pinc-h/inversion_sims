#!/bin/bash
i=$1
cd data
mkdir -p ${i}
cd ${i}
slim ../../model.slim
#if statement on if the inversion is lost, then run again
# These awk commands replace all spaces with commas
for file in *.csv; do
	awk -F"," '{ gsub(/ /, ","); }1' "${file}" > "parsed_${file}"
done
for file in *.*; do
	mv ${file} ${i}_${file}
done