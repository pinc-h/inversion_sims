# oct 23 2024 note: amend this script so that:
# mkdir data_$date and also runs the burn_in once before looping


#!/bin/bash
# Usage: seq (insert_number_of_runs_here) | /usr/local/bin/parallel -j 2 bash run.sh
i=$1
cd data
mkdir ${i}
cd ${i}
/usr/local/bin/slim ../../main.slim
for file in *.*; do
	mv ${file} ${i}_${file}
done
