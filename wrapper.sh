#!/bin/bash
read -p "Enter number of runs: " i
seq $i | /usr/local/bin/parallel -j 2 bash run_model.sh
echo "Complete"
