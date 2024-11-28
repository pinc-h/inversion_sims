#!/bin/bash
read -p "Enter number of runs: " i
seq $i | /usr/local/bin/parallel -j 2 bash 1_run_muteffect_truestart.sh
echo "Complete"