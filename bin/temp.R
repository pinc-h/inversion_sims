library(tidyverse)

burn_in <- read_delim(paste0("/Users/alexpinch/Desktop/burn_in_fitness.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

del_muts <- read_delim(paste0("/Users/alexpinch/Desktop/del_muts_fitness.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

no_del_muts <- read_delim(paste0("/Users/alexpinch/Desktop/no_del_muts_fitness.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

