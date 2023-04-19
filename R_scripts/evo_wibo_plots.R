# Alex Pinch, last edited April 17th 2023
# This script makes the plots used in my poster for EVO-WIBO 2023

# Required packages
library(tidyverse)

# Load data
setwd("/Users/alexpinch/GitHub/inversion_model")
all_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs"))
full_runs <- list.files("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/inversion_model/data_040623/full_runs",run))
  run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  run_data <- run_data %>% mutate(sim_run=run)
  all_data <- rbind(all_data, run_data)
}

# This removes the strict fitness changes coded in the SLiM model, this way we can assess fitness changes to look for build-up of deleterious mutations
all_data <- all_data %>% mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                             inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                             inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                             inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                             TRUE ~ fitness))

# |-------------------|
# | Plotting Figure 1 |
# |-------------------|
# This figure plots fitness over time for each inversion genotype.
# Format average fitness (compare this to overdominance model)
all_data %>%
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
# This figure plots average fitness of each run at 100,000 generations for each genotype.
# Calculate average fitness at the last generation



# |--------------------------------|
# | Plotting supplementary figures |
# |--------------------------------|
# Supplementary figure: fitness over time for each population 
# Format average fitness with population info
all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype, pop) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype, pop) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)

# Supplementary figure: Inversion frequency over time
all_data %>%
  group_by(gen,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=gen,y=freq,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method = "loess") +
  facet_wrap(~pop)

# Supplementary figure: Inversion frequency over time
all_data %>%
  filter(gen==1e5) %>%
  group_by(sim_run,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=inv_genotype,y=freq,group=inv_genotype,color=inv_genotype)) +
  geom_boxplot(method = "loess") +
  facet_wrap(~pop)
# are coding it in truly a symmetrical way? is a 10% increase the actual inverse of a 10% decrease
# run again with no del muts, if see exactly symmetrical, then this difference is either due to deleterial load build up OR too short of a burn in

