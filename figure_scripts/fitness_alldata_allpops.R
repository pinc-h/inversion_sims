library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion-model")

# Calculating averages and what not from the single large all_data.csv
# Save smaller CSVs with the averages to /processed
all_data <- read.csv(file = 'data/processed/all_data.csv')
head(all_data)

# Unaveraged fitness plots

# A single population, fitness per genotype over time, fitness corrected for fitMod
all_data %>%
  filter(pop == "pop1") %>%
  #Normalize the fitness based on the inversion genotype
  mutate(fitness = case_when(inv_genotype >= 1 ~ fitness - 0.1, # Case when is if statement in mutate, mutate is find and replace data
                             TRUE ~ fitness)) %>%
  ggplot(.,aes(x=gen,y=fitness,color=inv_genotype,group=as.factor(inv_genotype))) +
  geom_smooth(method="loess") # As factor converts 0, 1 ,2 genotype as discrete not continuous

# All populations, fitness over time, fitness corrected for 
all_data %>%
  #Normalize the fitness based on the inversion genotype
  mutate(fitness = case_when(inv_genotype == 2 ~ fitness - 0.1, # Case when is if statement in mutate, mutate is find and replace data
                             TRUE ~ fitness)) %>%
  ggplot(.,aes(x=gen,y=fitness,color=pop,group=as.factor(pop))) +
  geom_smooth(method="loess") # As factor converts 0, 1 ,2 genotype as discrete not continuous

# Get all the individuals in a single population: count all the rows when sample == X+(less than or equal to 500)


mean(all_data$fitness)
