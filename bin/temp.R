library(tidyverse)

# NO LONGER OUTPUTTING BURN IN FIT
# burn_in <- read_delim(paste0("/Users/alexpinch/Desktop/burn_in_fitness.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

del_muts_fit <- read_delim(paste0("/Users/alexpinch/Desktop/del_muts_fitness_4221439735696159844.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

del_muts_genotypes <- read_delim(paste0("/Users/alexpinch/Desktop/del_muts_genotypes_4221439735696159844.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")

no_del_muts_fit <- read_delim(paste0("/Users/alexpinch/Desktop/no_del_muts_fitness_8227572936049252286.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

no_del_muts_genotypes <- read_delim(paste0("/Users/alexpinch/Desktop/no_del_muts_genotypes_8227572936049252286.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")

del_muts <- del_muts_fit %>%
  inner_join(del_muts_genotypes)

no_del_muts <- no_del_muts_fit %>%
  inner_join(no_del_muts_genotypes)
