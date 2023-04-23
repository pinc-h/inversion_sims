# Alex Pinch, last edited April 18th 2023
# This script makes the plots used in my poster for EVO-WIBO 2023

# Required packages
library(tidyverse)
setwd("/Users/alexpinch/GitHub/inversion_model")

# Load locally adaptive data
la_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs"))
full_runs <- list.files("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs",run))
  run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  run_data <- run_data %>% mutate(sim_run=run)
  la_data <- rbind(la_data, run_data)
}

# This removes the strict fitness changes coded in the SLiM model, this way we can assess fitness changes to look for build-up of deleterious mutations
la_data <- la_data %>% mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                             inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                             inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                             inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                             TRUE ~ fitness))

# Load overdominant data
od_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/inversion_model/data_041823_od/full_runs"))
full_runs <- list.files("/Users/alexpinch/GitHub/inversion_model/data_041823_od/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/inversion_model/data_041823_od/full_runs",run))
  od_run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  od_run_data <- run_data %>% mutate(sim_run=run)
  od_data <- rbind(od_data, od_run_data)
}

# This removes the global fitness increase for all inversion-carrying heterozygotes
od_data <- od_data %>% mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.05,
                                                        inv_genotype == 1 ~ fitness - 0.1,
                                                        TRUE ~ fitness))

# |-------------------|
# | Plotting Figure 1 |
# |-------------------|
# This figure plots fitness over time for each inversion genotype in the locally adaptive model.
# Format average fitness (compare this to overdominance model)
la_data %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) %>% ## Can change to 0.5 for the median
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2) +  # alpha = transparency
  labs(x = "Generations", y = "Fitness") + 
  scale_color_discrete(name = "Inversion Genotype") + # Generates a second legend with a readable title
  guides(fill = "none") # Removes the original unreadable legend

# |-------------------|
# | Plotting Figure 2 |
# |-------------------|
# This figure plots average fitness of each run at 100,000 generations for each genotype in the overdominant model.
# Calculate average fitness at the last generation
la_data %>%
  filter(!is.na(inv_genotype), gen==1e5) %>% # Change to gen=51000 to compare to first generation
  group_by(sim_run, pop, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(sim_run,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=inv_genotype,y=mean_sim_fit,group=inv_genotype,color=inv_genotype)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  facet_wrap(~pop) + 
  labs(x = "Genotype", y = "Mean fitness") + 
  scale_color_continuous(name = "Inversion Genotype")

# |-------------------|
# | Plotting Figure 3 |
# |-------------------|
# This figure plots fitness over time for each inversion genotype in the overdominant model.
# Format average fitness
od_data %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) %>% ## Can change to 0.5 for the median
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2) +  # alpha = transparency
  labs(x = "Generations", y = "Fitness") + 
  scale_color_discrete(name = "Inversion Genotype") + # Generates a second legend with a readable title
  guides(fill = "none") # Removes the original unreadable legend

# |-------------------|
# | Plotting Figure 4 |
# |-------------------|
# This figure plots average fitness of each run at 100,000 generations for each genotype in the overdominant model.
# Calculate average fitness at the last generation
od_data %>%
  filter(!is.na(inv_genotype), gen==1e5) %>% # Change to gen=51000 to compare to first generation
  group_by(sim_run, pop, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(sim_run,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=inv_genotype,y=mean_sim_fit,group=inv_genotype,color=inv_genotype)) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  facet_wrap(~pop) + 
  labs(x = "Genotype", y = "Mean fitness") + 
  scale_color_continuous(name = "Inversion Genotype")

# |--------------------------------|
# | Plotting supplementary figures |
# |--------------------------------|
# Supplementary figure: fitness over time for each population 
la_data %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype, pop) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype, pop) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop) + 
  labs(x = "Generations", y = "Mean fitness per population per replicate") + 
  scale_color_continuous(name = "Inversion Genotype")

# Supplementary figure: Inversion frequency over time
la_data %>%
  group_by(gen,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=gen,y=freq,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method = "loess") +
  facet_wrap(~pop) + 
  labs(x = "Generations", y = "Inversion Frequency") + 
  scale_color_continuous(name = "Inversion Genotype")

# Supplementary figure: Inversion genotype frequency at the last generation
la_data %>%
  filter(gen==1e5) %>%
  group_by(sim_run,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=inv_genotype,y=freq,group=inv_genotype,color=inv_genotype)) +
  geom_boxplot(method = "loess") +
  facet_wrap(~pop) + 
  labs(x = "Genotype", y = "Inversion Frequency") + 
  scale_color_continuous(name = "Inversion Genotype")

# Supplementary figure: Inversion genotype frequency at the first generation
la_data %>%
  filter(gen==51000) %>%
  group_by(sim_run,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=inv_genotype,y=freq,group=inv_genotype,color=inv_genotype)) +
  geom_boxplot(method = "loess") +
  facet_wrap(~pop) + 
  labs(x = "Genotype", y = "Inversion Frequency") + 
  scale_color_continuous(name = "Inversion Genotype")

