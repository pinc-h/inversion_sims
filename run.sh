#!/bin/bash
# Usage: seq (insert_number_of_runs_here) | /usr/local/bin/parallel -j 2 bash run_model.sh
i=$1
cd data
mkdir ${i}
cd ${i}
/usr/local/bin/slim ../../model.slim
for file in *.*; do
	mv ${file} ${i}_${file}
done
