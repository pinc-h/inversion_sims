library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion-model")
files <- length(list.files("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs"))
cat(files)
full_runs <- list.files("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs")
cat(full_runs)
for (i in 1:files) {
  run <- (full_runs[i])
  cat(run)
  setwd(file.path("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs/",run))
  data <- read_delim(paste0(run,"_fitness.csv"),col_names=F) %>%
    rename(gen=X1,pop=X2)
  data <- data %>%
    pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "fitness")
  genotypes <- read_delim(paste0(run,"_genotypes.csv"),col_names=F) %>%
    rename(gen=X1,pop=X2)
  genotypes <- genotypes %>%
    pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")
  all_data <- data %>%
    inner_join(genotypes)
  write.csv(all_data,paste0(run,".csv"), row.names = FALSE)
}

