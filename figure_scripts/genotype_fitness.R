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

#Fitness is reduced for inversion samples due to deleterious mutation build up
all_data %>%
  filter(pop == "pop1") %>%
  #Normalize the fitness based on the inversion genotype
  mutate(fitness = case_when(inv_genotype >= 1 ~ fitness - 0.1, # Case when is if statement in mutate, mutate is find and replace data
                             TRUE ~ fitness)) %>%
  ggplot(.,aes(x=gen,y=fitness,color=inv_genotype,group=as.factor(inv_genotype))) +
  geom_smooth(method="loess") # As factor converts 0, 1 ,2 genotype as discrete not continuous

#Allele frequency change over time
all_data %>%
  group_by(gen,pop,inv_genotype) %>% # Lets you apply grouping functions, calculate average fitness within a subpopulation
  summarize(n=n()) %>% # New column named n, and using a command name n to count
  mutate(freq = n / sum(n)) %>% # Summarize compresses it down, 1 line per group, while mutate keeps the same number of lines and adds a column to it
  ggplot(.,aes(x=gen,y=freq,color=as_factor(inv_genotype))) +
  geom_line() +
  facet_wrap(~pop) #Make a different plot for every pop

all_data %>%
  filter(gen == “60000”) %>%
  #Normalize the fitness based on the inversion genotype
  ggplot(.,aes(x=gen,y=fitness,color=inv_genotype,group=as.factor(inv_genotype))) +
  geom_boxplot() +
  facet_wrap(~pop)
