library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion_model")
all_data <- tibble()
files <- length(list.files("/Users/alexpinch/Documents/GitHub/inversion_model/nov15_data/lost_runs"))
full_runs <- list.files("/Users/alexpinch/Documents/GitHub/inversion_model/nov15_data/lost_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/Documents/GitHub/inversion_model/nov15_data/lost_runs/",run))
  run_data <- read.csv(file = paste(run,"_seed.csv",sep=""),skip=1,header=F) %>%
    rename(seed=V1,gen=V2)
  all_data <- rbind(all_data, run_data)
}