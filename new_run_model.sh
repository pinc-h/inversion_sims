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

lost=true

while [ lost ]; do
	run_model
	if [ -f "lost_fitness.csv" ]; then
		lost=true
	else
		lost=false
	fi
	fix_commas
	cd ../..
done
