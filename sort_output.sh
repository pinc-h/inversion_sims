#!/bin/bash
runs=$1
mkdir data/full_runs
mkdir data/lost_runs
cd data

for i in $(seq $runs); do
#
	if [ -f ${i}/"${i}_lost_fitness.csv" ]; then
		cd lost_runs
		mv ../${i} ${i}
		cd ..
	else
		cd full_runs
		mv ../${i} ${i}
		cd ..
	fi
done
DIR=$(date +%Y%m%d%H%M)
mkdir "${DIR}"
mv full_runs ${DIR}/full_runs
mv lost_runs ${DIR}/lost_runs
tar --use-compress-program="pigz -k -p2" -cf ${DIR}.tar.gz ${DIR}
# -p is the number of processors to use
