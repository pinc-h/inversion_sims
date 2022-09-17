#!/bin/bash
read -p "Enter number of runs: " runs
seq $runs | parallel -j 2 bash run_model.sh
bash sort_output.sh $runs
echo "Complete"