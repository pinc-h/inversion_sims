#!/bin/bash
i=$1
j=$1

while j < 100; do
	seq 1 | parallel -j 1 bash run_model.sh
	if [ -f ${i}/"${i}_lost_fitness.csv" ]; then
        else
		j = j + 1
        fi 
	i = i + 1
done
bash sort_output.sh $i
