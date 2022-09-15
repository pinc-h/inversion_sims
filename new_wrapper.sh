#!/bin/bash
i=1
j=1

while [ ${j} -le 100 ]
do
	seq 1 | parallel -j 1 bash run_model.sh
	if [ -f ${i}/"${i}_lost_fitness.csv" ]; then
		echo "${i} runs"
		echo "${j} runs lost"

        else
		((j++))
        fi 
	((i++))
done
bash sort_output.sh ${i}
