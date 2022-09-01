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
cd ..
