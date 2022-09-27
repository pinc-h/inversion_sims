library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion-model")

all_the_data <- tibble()
files <- length(list.files("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs"))
cat(files)
full_runs <- list.files("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs")
cat(full_runs)
for (i in 1:files) {
  run <- (full_runs[i])
  setwd(file.path("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs/",run))
  run_data  <- read_delim(paste0(run,".csv"),col_names=F) %>%
    rename(gen=X1,pop=X2)
  run_data %>%
    mutate(sim_run=c(run))
  all_the_data <- rbind(all_the_data, run_data)
}

#' Plot just genotype==2 and see if theres a simulation that skews the global average