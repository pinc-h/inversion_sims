# Alex Pinch, last edited April 16th 2023
# This script is full of rough work in formatting and plotting raw output from the model

library(tidyverse)
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

# Plotting all runs mean fitness over generations
# (x axis = generations, y axis = mean fitness, grouped and coloured by run)
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

# Plotting a single run's fitness over generations
# (x axis = generations, y axis = mean fitness, grouped and coloured by genotype)
all_data %>%
  filter(sim_run == "1044") %>%
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

# Plotting homozygote and heterozygote fitness over generations for all runs
# (x axis generations, y axis mean fitness, grouped and coloured by run, 1 plot for homozygotes, 1 plot for heterozygotyes)
all_data %>%
  filter(inv_genotype == 2) %>%
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
  filter(inv_genotype == 1) %>%
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

# Creating fixed mean fitness with 90% CI
summarized_data <- all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) ## 0.5 = median

# Plotting mean fitness (90% CI) over generations, averaged all populations
summarized_data %>%
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2) # alpha = transparency

# Box plots of genotype vs. fitness of the last generation
all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype)) %>%
  filter(gen == 51000) %>%
  group_by(pop, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=as.factor(inv_genotype),y=mean_fit)) +
  geom_boxplot() +
  facet_wrap(~pop)

# Plotting homozygotes of a particular run
all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(inv_genotype==2, sim_run==914) %>%
  group_by(gen, pop, sim_run)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness)) +
  geom_smooth(method="loess") + 
  facet_wrap(~pop)

#Allele frequency change over time
genotype <- as_factor(inv_genotype)
all_data %>%
  group_by(gen,pop,inv_genotype) %>%
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(.,aes(x=gen,y=freq,color=inv_genotype)) +
  geom_line() +
  facet_wrap(~pop)


# April changes, Greg's code
all_data %>%
  filter(gen >= 100000) %>%
  group_by(sim_run, pop, inv_genotype) %>%
  summarize(count=n()) %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(sim_run,pop) %>%
  pivot_wider(names_from = inv_genotype,values_from=count)

# April 15th 2023, making plots for EVO-WIBO
# (want to make AVERAGE fitness plots for SPECIFIC genotypes)
summarized_data <- all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(!is.na(inv_genotype)) %>%
  group_by(gen, sim_run, inv_genotype) %>%
  summarize(mean_fit = mean(fixed_fitness,na.rm=T)) %>%
  group_by(gen,inv_genotype) %>%
  mutate(qnt_90 = quantile(mean_fit, 0.9),
         qnt_10 = quantile(mean_fit, 0.1),
         mean_sim_fit = quantile(mean_fit, 0.9)) ## 0.5 = median

# Plotting mean fitness (90% CI) over generations
summarized_data %>%
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2) # alpha = transparency
  
avg_pop_data <- all_data %>%
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
         mean_sim_fit = quantile(mean_fit, 0.9)) ## 0.5 = median  

avg_pop_data %>%
  ggplot(.) +
  geom_line(aes(x=gen,y=mean_sim_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_ribbon(aes(x=gen,ymin=qnt_10,ymax=qnt_90,group=inv_genotype,fill=as.factor(inv_genotype)),alpha=0.2) + # alpha = transparency
  facet_wrap(~pop)

all_data %>%
  mutate(fixed_fitness = case_when(inv_genotype == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.1,
                                   inv_genotype == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.1,
                                   inv_genotype == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                   inv_genotype == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                   TRUE ~ fitness)) %>%
  filter(inv_genotype == 2) %>%
  group_by(gen, pop)%>%
  summarize(mean_fitness= mean(fixed_fitness,na.rm=T)) %>%
  ggplot(.,aes(x=gen,y=mean_fitness,group=pop,color=pop)) +
  geom_smooth(method="loess")
  

avg_pop_data %>%
  filter(inv_genotype ==2) %>%
  ggplot(.,aes(x=gen,y=mean_sim_fit,group=pop,color=pop)) +
  geom_smooth(method="loess")
  
  


