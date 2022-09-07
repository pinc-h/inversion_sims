seq 100 | parallel -j 2 bash run_model.sh
bash sort_output.sh
say "Beep"
