library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion-model")

# Calculating averages and what not from the single large all_data.csv
# Save smaller CSVs with the averages to /processed
all_data <- read.csv(file = 'data/processed/all_data.csv')
head(all_data)

# ========================
# Unaveraged fitness plots
# ========================

# Fitness vs. Genotype for Pop 1
all_data %>%
  filter(pop == "pop1") %>%
  #Normalize the fitness based on the inversion genotype
  mutate(fitness = case_when(inv_genotype >= 1 ~ fitness - 0.1, # Case when is if statement in mutate, mutate is find and replace data
                             TRUE ~ fitness)) %>%
  ggplot(.,aes(x=gen,y=fitness,color=inv_genotype,group=as.factor(inv_genotype))) +
  geom_smooth(method="loess") # As factor converts 0, 1 ,2 genotype as discrete not continuous

# Fitness vs. Genotype for all populations
all_data %>%
  mutate(fitness = case_when(inv_genotype >= 1 ~ fitness - 0.1, # Case when is if statement in mutate, mutate is find and replace data
                             TRUE ~ fitness)) %>%
  ggplot(.,aes(x=gen,y=fitness,color=pop,group=as.factor(pop))) +
  geom_smooth(method="loess") # As factor converts 0, 1 ,2 genotype as discrete not continuous

# Fitness vs. Genotype for the upper region 
all_data %>%
  filter(pop %in%  c("pop1", "pop5", "pop9", inv_genotype >= 1)) %>%
  mutate(fitness = case_when(inv_genotype >= 1 ~ fitness - 0.1, # Case when is if statement in mutate, mutate is find and replace data
                             TRUE ~ fitness)) %>%
  ggplot(.,aes(x=gen,y=fitness,color=pop)) +
  geom_smooth(method="loess") # As factor converts 0, 1 ,2 genotype as discrete not continuous

# ========================
# Average fitness plots
# ========================

# Average Fitness vs. Genotype for each region
all_data %>% 
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  group_by(gen, pop, inv_genotype)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)

# How to get mean fitness over time?