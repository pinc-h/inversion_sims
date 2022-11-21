library(tidyverse)
setwd("/Users/alexpinch/Documents/GitHub/inversion_model")

all_data <- tibble()
files <- length(list.files("/Users/alexpinch/Documents/GitHub/inversion_model/data_het_adv_102022/full_runs"))
full_runs <- list.files("/Users/alexpinch/Documents/GitHub/inversion_model/data_het_adv_102022/full_runs")
for (i in 1:files) {
  run <- (full_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/Documents/GitHub/inversion_model/data_het_adv_102022/full_runs/",run))
  run_data <- read.csv(file = paste(run,".csv",sep=""),skip=1,header=F) %>%
    rename(gen=V1,pop=V2,sample=V3,fitness=V4,inv_genotype=V5)
  run_data <- run_data %>% mutate(sim_run=run)
  all_data <- rbind(all_data, run_data)
}

# ========================
# Fitness per simulation run
# ========================

all_data %>%
  filter(inv_genotype == 1) %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.1,
                                   inv_genotype == 1 ~ fitness - 0.05,
                                   TRUE ~ fitness)) %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=sim_run,color=sim_run)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)

all_data %>%
  filter(sim_run == "775") %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.1,
                                   inv_genotype == 1 ~ fitness - 0.5,
                                   TRUE ~ fitness)) %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=inv_genotype,color=inv_genotype)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)


summarized_data <- all_data %>%
  mutate(fixed_fitness = ase_when(inv_genotype == 2 ~ fitness - 0.1,
                                  inv_genotype == 1 ~ fitness - 0.5,
                                  TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) ## 0.5 = median

summarized_data %>%
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2)
## alpha = transparency

## Box plots for the last generation data
all_data %>%
  mutate(fixed_fitness = ase_when(inv_genotype == 2 ~ fitness - 0.1,
                                  inv_genotype == 1 ~ fitness - 0.5,
                                  TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype), sim_run==775) %>%
  group_by(gen, inv_genotype) %>%
  filter(gen == 5e4) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=as.factor(inv_genotype),y=mean_fit)) +
  geom_boxplot()

all_data %>% 
  mutate(fixed_fitness = ase_when(inv_genotype == 2 ~ fitness - 0.1,
                                  inv_genotype == 1 ~ fitness - 0.5,
                                  TRUE ~ fitness)) %>%
  filter(inv_genotype==2, sim_run==914) %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness)) +
  geom_smooth(method="loess") + 
  facet_wrap(~pop)

# Trying to find low fitness individuals
all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.1,
                                   inv_genotype == 1 ~ fitness - 0.05,
                                   TRUE ~ fitness)) %>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  filter(mean_fitness<=0.05) %>%
  group_by(gen, pop, sim_run)%>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=sim_run,color=sim_run)) +
  geom_smooth(method="loess") +
  facet_wrap(~pop)

fit0_homozygotes <- all_data %>%   
  filter(inv_genotype == 2) %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.1,
                                   inv_genotype == 1 ~ fitness - 0.05,
                                   TRUE ~ fitness)) %>%
  filter(fixed_fitness<=0) %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T))


# Greg's method of finding low fitness individuals
low_fitness_counts <- all_data %>%
  filter(inv_genotype == 2) %>%
  mutate(low_fitness = case_when(fitness < 0.25 ~ "low",
                                 T ~ "high") ) %>%
  group_by(sim_run, gen, low_fitness) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count)) 

# Plotting homozygote individuals of the weird run (goes to 0 very quickly)
all_data %>%
  filter(sim_run == 483, inv_genotype==2) %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 ~ fitness - 0.1,
                                   inv_genotype == 1 ~ fitness - 0.05,
                                   TRUE ~ fitness)) %>%
  group_by(gen, sample, pop) %>%
  ggplot(.,aes(x=gen, y=fixed_fitness,group=sample,color=pop)) + 
  geom_smooth(method="loess")


  
  
