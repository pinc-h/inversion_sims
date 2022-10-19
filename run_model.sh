#!/bin/bash
i=$1
cd data
mkdir ${i}
cd ${i}
/usr/local/bin/slim ../../model.slim
for file in *.*; do
	mv ${file} ${i}_${file}
done
