library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion_model")
files <- length(list.files("/Users/alexpinch/Documents/GitHub/inversion_model/oct20_data/full_runs"))
full_runs <- list.files("/Users/alexpinch/Documents/GitHub/inversion_model/oct20_data/full_runs")

# For joining SLiM output fitness and genotype data into a single file
for (i in 1:files) {
  run <- (full_runs[i])
  cat(run)
  setwd(file.path("/Users/alexpinch/Documents/GitHub/inversion_model/oct20_data/full_runs/",run))
  data <- read_delim(paste0(run,"_fitness.csv"),col_names=F) %>%
    rename(gen=X1,pop=X2)
  data <- data %>%
    pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "fitness")
  genotypes <- read_delim(paste0(run,"_genotypes.csv"),col_names=F) %>%
    rename(gen=X1,pop=X2)
  genotypes <- genotypes %>%
    pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")
  joined_data <- data %>%
    inner_join(genotypes)
  write.csv(joined_data,paste0(run,".csv"), row.names = FALSE)
}

# For creating a single, massive file of all run data
setwd("/Users/alexpinch/Documents/GitHub/inversion_model/oct20_data/processed")
write.csv(joined_data,"all_data.csv", row.names = FALSE)

