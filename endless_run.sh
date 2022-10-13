#!/bin/bash
run_model () {
	i=$(ls -l data/ | wc -l)
	cd data
	mkdir -p ${i}
	cd ${i}
	slim ../../model.slim
}

fix_commas () {
	# These awk commands replace all spaces with commas
	for file in *.csv; do
		awk -F"," '{ gsub(/ /, ","); }1' "${file}" > "parsed_${file}"
	done
	for file in *.*; do
		mv ${file} ${i}_${file}
	done
}

lost_check() {
	if [ -f "lost_fitness.csv" ]; then
		echo "Lost check: True"
		return 0
	else
		echo "Lost check: False"
		return 1
	fi
}

run_model
lost_check
fix_commas


while [ lost_check ]; do
	cd ../..
	run_model
	lost_check
	fix_commas
done
