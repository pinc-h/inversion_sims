#!/bin/bash
i=$(ls -l data/ | wc -l)
cd data
mkdir -p ${i}
cd ${i}
slim ../../model.slim
# These awk commands replace all spaces with commas
for file in *.csv; do
	awk -F"," '{ gsub(/ /, ","); }1' "${file}" > "parsed_${file}"
done
for file in *.*; do
	mv ${file} ${i}_${file}
done
