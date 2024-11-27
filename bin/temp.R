library(tidyverse)

# NO LONGER OUTPUTTING BURN IN FIT
# burn_in <- read_delim(paste0("/Users/alexpinch/Desktop/burn_in_fitness.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

del_muts_fit <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/del_muts_fitness_4221439735696159844.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

del_muts_genotypes <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/del_muts_genotypes_4221439735696159844.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")

no_del_muts_fit <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/no_del_muts_fitness_8227572936049252286.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

no_del_muts_genotypes <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/no_del_muts_genotypes_8227572936049252286.csv"),col_names=F) %>%
  rename(gen=X1,pop=X2) %>%
  pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "inv_genotype")

del_muts <- del_muts_fit %>%
  inner_join(del_muts_genotypes)

no_del_muts <- no_del_muts_fit %>%
  inner_join(no_del_muts_genotypes)

no_del_muts %>%
  filter(!is.na(inv_genotype), gen==199000, fitness > 0.06) %>%
  group_by(pop, inv_genotype) %>%
  summarize(mean_fit = mean(fitness,na.rm=T)) %>%
  ggplot(.,aes(x=as.factor(inv_genotype),y=mean_fit,group=inv_genotype,color=as.factor(inv_genotype))) +
  geom_boxplot() + geom_jitter(width = 0.2) + facet_wrap(~pop) +
  labs(x = "Genotype", y = "Fitness", title = "no del muts, gen 199,000") +
  scale_color_discrete(name = "Inversion Genotype") + 
  theme(text = element_text(size = 20))

frequency <- del_muts %>%
  filter(!is.na(inv_genotype), gen==101000) %>%
  group_by(pop, inv_genotype) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(frequency = count / 500)

frequency %>%
  ggplot(.,aes(x=as.factor(inv_genotype),y=frequency,group=inv_genotype,fill=as.factor(inv_genotype))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Genotype", y = "Frequency", title = "no del muts") +
  scale_color_discrete(name = "Inversion Genotype") + 
  theme(text = element_text(size = 20))
  
