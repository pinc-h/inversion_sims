library(tidyverse)
data <- read_delim("/Users/alexpinch/Desktop/august29/data/full_runs/10/10_individual_fitness.csv", skip = 1,col_names=F) %>% # Skips the column names (first column) and says there are no column names for this data set
  rename(gen=X1,pop=X2)
data <- data %>% # %>% is a pipe function
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "fitness") # This converts this 'wide data' to tidy data
genotypes <- read_delim("//Users/alexpinch/Desktop/august29/data/full_runs/10/10_individual_genotypes.csv",
                        skip = 1,col_names=F) %>%
  rename(gen=X1,pop=X2)
genotypes <- genotypes %>%
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")
all_data <- data %>%
  inner_join(genotypes)
#Fitness is reduced for inversion samples due to deleterious mutation build up
all_data %>%
  filter(gen == 60000) %>%
  #Normalize the fitness based on the inversion genotype
  ggplot(.,aes(x=gen,y=fitness,color=inv_genotype,group=as.factor(inv_genotype))) +
  geom_boxplot() +
  facet_wrap(~pop)