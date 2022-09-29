library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion-model")

all_data <- tibble()
files <- length(list.files("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs"))
full_runs <- list.files("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/Documents/GitHub/inversion-model/data/full_runs/",run))
  run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  run_data <- run_data %>% mutate(sim_run=run)
  all_data <- rbind(all_data, run_data)
}

# ========================
# Fitness per simulation run
# ========================

#' Plot just genotype==2 and see if theres a simulation that skews the global average

all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=sim_run,color=sim_run)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)


all_data %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=sim_run,color=sim_run)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)
