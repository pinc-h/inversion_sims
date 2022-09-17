library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion-model")
data <- read_delim("./data/full_runs/69/69_fitness.csv",col_names=F) %>% # Skips the column names (first column) and says there are no column names for this data set
  rename(gen=X1,pop=X2)
data <- data %>% # %>% is a pipe function
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "fitness") # This converts this 'wide data' to tidy data
genotypes <- read_delim("./data/full_runs/69/69_genotypes.csv",col_names=F) %>%
  rename(gen=X1,pop=X2)
genotypes <- genotypes %>%
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")
all_data <- data %>%
  inner_join(genotypes)
write.csv(all_data,"./data/full_runs/69/69.csv", row.names = FALSE)
