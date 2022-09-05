mkdir data/full_runs
mkdir data/lost_runs
cd data
for i in {1..100}; do
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
ls | time parallel -j 2 tar -zcvf ${DIR}.tar.gz ${DIR}
rm -r ${DIR}


