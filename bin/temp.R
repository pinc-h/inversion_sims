library(tidyverse)

# NO LONGER OUTPUTTING BURN IN FIT
# burn_in <- read_delim(paste0("/Users/alexpinch/Desktop/burn_in_fitness.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")

# del_muts_fit <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/del_muts_fitness_4867507197415516269.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")
# 
# del_muts_genotypes <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/del_muts_genotypes_4867507197415516269.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "genotypes")
# 
# del_muts <- del_muts_fit %>%
#   inner_join(del_muts_genotypes)


lib_dir <- "/Users/alexpinch/GitHub/private/inversion_model/lib/250121"

# Read all "del_muts_fitness_*" files
del_muts_fit <- list.files(path = lib_dir, pattern = "del_muts_fitness_.*\\.csv", full.names = TRUE) %>%
  map_df(~ {
    file_name <- basename(.x)  # Get the file name without the path
    seed <- gsub(".*del_muts_fitness_(\\d+).*", "\\1", file_name)  # Extract the seed number
    read_delim(.x, col_names = FALSE) %>%
      rename(gen = X1, pop = X2) %>%
      pivot_longer(cols = -c(gen, pop), names_to = "sample", values_to = "fitness") %>%
      mutate(seed = seed) 
  })

# Read all "del_muts_genotype_*" files
del_muts_genotype <- list.files(path = lib_dir, pattern = "genotypes_.*\\.csv", full.names = TRUE) %>%
  map_df(~ {
    file_name <- basename(.x)  # Get the file name without the path
    seed <- gsub(".*genotypes_(\\d+).*", "\\1", file_name)  # Extract the seed number
    read_delim(.x, col_names = FALSE) %>%
      rename(gen = X1, pop = X2) %>%
      pivot_longer(cols = -c(gen, pop), names_to = "sample", values_to = "genotypes") %>%
      mutate(seed = seed) 
  })

# This appends the population to the sample ID. This helps me keep them straight as now each individual sample is unique, no getting pops mixed up
del_muts_fit <- del_muts_fit %>%
  mutate(sample = paste0(gsub("\\D", "", pop), "_", sample))
del_muts_genotype <- del_muts_genotype %>%
  mutate(sample = paste0(gsub("\\D", "", pop), "_", sample))

# Merge the datasets
del_muts <- del_muts_fit %>%
  inner_join(del_muts_genotype, by = c("gen", "pop", "sample", "seed"))


# no_del_muts_fit <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/no_del_muts_fitness_8227572936049252286.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample", values_to = "fitness")
# 
# no_del_muts_genotypes <- read_delim(paste0("/Users/alexpinch/GitHub/private/inversion_model/lib/no_del_muts_genotypes_8227572936049252286.csv"),col_names=F) %>%
#   rename(gen=X1,pop=X2) %>%
#   pivot_longer(c(-gen,-pop), names_to = "sample",values_to = "genotypes")
# 
# no_del_muts <- no_del_muts_fit %>%
#   inner_join(no_del_muts_genotypes)

frequency <- del_muts %>%
  filter(!is.na(genotypes), gen==199000) %>%
  group_by(pop, genotypes) %>%
  summarise(count = n(), .groups = "drop")

frequency %>%
  ggplot(.,aes(x=as.factor(genotypes),y=count,group=genotypes,fill=as.factor(genotypes))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Genotype", y = "Total count", title = "Individual counts at 199,000 generations") +
  scale_fill_discrete(name = "Inversion Genotype", labels = c("Non-inverted", "Heterozygous", "Homozygous")) + 
  theme(text = element_text(size = 20)) +
  ylim(0,25000)

# Fitness
del_muts %>%
  filter(!is.na(genotypes), gen==199000) %>%
  group_by(pop, genotypes) %>%
  summarize(mean_fit = mean(fitness,na.rm=T)) %>%
  ggplot(.,aes(x=as.factor(genotypes),y=mean_fit,group=genotypes,color=as.factor(genotypes))) +
  geom_boxplot() + geom_jitter(width = 0.2) + facet_wrap(~pop) +
  labs(x = "Genotype", y = "Fitness", title = "Fitness, gen. 101,000") +
  scale_color_discrete(name = "Inversion Genotype") + 
  theme(text = element_text(size = 20)) #+
  #ylim(0.8,1.1)

# "Deleterial load"
# This removes the strict fitness changes coded in the SLiM model, this way we can assess fitness changes to look for build-up of deleterious mutations
del_muts <- del_muts %>% mutate(fixed_fitness = case_when(genotypes == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                                        genotypes == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                                        genotypes == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.025,
                                                        genotypes == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.025,
                                                        TRUE ~ fitness))
del_muts <- del_muts %>% mutate(del_load = case_when(fixed_fitness > 0.5 ~ 1 - fixed_fitness,
                                                   TRUE ~ fitness))

# Adjusting for fitness benefit in Overdominant model
del_muts <- del_muts %>% mutate(fixed_fitness = case_when(genotypes == 2 ~ fitness - 0.025,
                                                          genotypes == 1 ~ fitness - 0.05,
                                                          TRUE ~ fitness))

del_muts <- del_muts %>% mutate(del_load = case_when(fixed_fitness > 0.5 ~ 1 - fixed_fitness,
                                                     TRUE ~ fitness))


del_muts %>%
  filter(!is.na(genotypes), gen==103000) %>% # Change gen=1e5 to compare to last generation
  group_by(seed, pop, del_load, genotypes) %>%
  summarize(mean_load = mean(del_load,na.rm=T)) %>%
  group_by(seed, genotypes) %>%
  mutate(qnt_90 = quantile(mean_load, 0.9),
         qnt_10 = quantile(mean_load, 0.1),
         mean_seed_load = quantile(mean_load, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=as.factor(genotypes),y=mean_seed_load,group=genotypes,color=as.factor(genotypes))) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = "Genotype", y = "Deleterial load", title="Deleterial load accumulation at 103,000 generations") +
  scale_fill_discrete(name = "Inversion Genotype", labels = c("Non-inverted", "Heterozygous", "Homozygous")) + 
  ylim(0, 0.45) +
  theme(text = element_text(size = 20)) 
  
# Looking at some odd rows in the od dataset
weird_rows <- subset(del_muts, del_load > 0.2 & gen == 103000)
print(weird_rows)




# -------------
# -------------
# -------------
# -------------
# -------------
# -------------
# -------------
# -------------
# -------------




# tiny_inversion data loading
lib_dir <- "/Users/alexpinch/GitHub/private/inversion_model/lib/250206"
tiny_fit <- list.files(path = lib_dir, pattern = "tiny_inversion_fitness_.*\\.csv", full.names = TRUE) %>%
  map_df(~ {
    file_name <- basename(.x)  # Get the file name without the path
    seed <- gsub(".*tiny_inversion_fitness_(\\d+).*", "\\1", file_name)  # Extract the seed number
    read_delim(.x, col_names = FALSE) %>%
      rename(gen = X1, pop = X2) %>%
      pivot_longer(cols = -c(gen, pop), names_to = "sample", values_to = "fitness") %>%
      mutate(seed = seed) 
  })
tiny_genotype <- list.files(path = lib_dir, pattern = "tiny_inversion_genotypes_.*\\.csv", full.names = TRUE) %>%
  map_df(~ {
    file_name <- basename(.x)  # Get the file name without the path
    seed <- gsub(".*tiny_inversion_genotypes_(\\d+).*", "\\1", file_name)  # Extract the seed number
    read_delim(.x, col_names = FALSE) %>%
      rename(gen = X1, pop = X2) %>%
      pivot_longer(cols = -c(gen, pop), names_to = "sample", values_to = "genotypes") %>%
      mutate(seed = seed) 
  })
tiny_fit <- tiny_fit %>%
  mutate(sample = paste0(gsub("\\D", "", pop), "_", sample))
tiny_genotype <- tiny_genotype %>%
  mutate(sample = paste0(gsub("\\D", "", pop), "_", sample))

tiny <- tiny_fit %>%
  inner_join(tiny_genotype, by = c("gen", "pop", "sample", "seed"))

tiny_frequency <- tiny %>%
  filter(!is.na(genotypes), gen==150000) %>%
  group_by(pop, gen, genotypes) %>%
  summarise(count = n(), .groups = "drop")

tiny_frequency %>%
  ggplot(.,aes(x=as.factor(genotypes),y=count,group=genotypes,fill=as.factor(genotypes))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Genotype", y = "Total count", title = "Individual counts at 150,000 generations") +
  scale_fill_discrete(name = "Inversion Genotype", labels = c("Non-inverted", "Heterozygous", "Homozygous")) + 
  theme(text = element_text(size = 20)) +
  ylim(0,25000)

tiny <- tiny %>% mutate(fixed_fitness = case_when(genotypes == 2 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.05,
                                                          genotypes == 2 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.05,
                                                          genotypes == 1 & pop %in% c("pop1","pop2","pop4") ~ fitness - 0.025,
                                                          genotypes == 1 & pop %in% c("pop6","pop8","pop9") ~ fitness + 0.025,
                                                          TRUE ~ fitness))
tiny <- tiny %>% mutate(del_load = case_when(fixed_fitness > 0.5 ~ 1 - fixed_fitness,
                                                     TRUE ~ fitness))
tiny %>%
  filter(!is.na(genotypes), gen==103000) %>% # Change gen=1e5 to compare to last generation
  group_by(seed, pop, del_load, genotypes) %>%
  summarize(mean_load = mean(del_load,na.rm=T)) %>%
  group_by(seed, genotypes) %>%
  mutate(qnt_90 = quantile(mean_load, 0.9),
         qnt_10 = quantile(mean_load, 0.1),
         mean_seed_load = quantile(mean_load, 0.9)) %>%  ## 0.5 = median
  ggplot(.,aes(x=as.factor(genotypes),y=mean_seed_load,group=genotypes,color=as.factor(genotypes))) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = "Genotype", y = "Deleterial load", title="Deleterial load accumulation at 103,000 generations") +
  scale_fill_discrete(name = "Inversion Genotype", labels = c("Non-inverted", "Heterozygous", "Homozygous")) + 
  ylim(0, 0.45) +
  theme(text = element_text(size = 20)) 

# For saving plots all at once (gives them odd crop, commented them out)
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="/Users/alexpinch/GitHub/private/inversion_model/lib/plots/tiny_inv")


no_recomb_fit <- list.files(path = lib_dir, pattern = "no_recomb_del_muts_fitness_.*\\.csv", full.names = TRUE) %>%
  map_df(~ {
    file_name <- basename(.x)  # Get the file name without the path
    seed <- gsub(".*no_recomb_del_muts_fitness_(\\d+).*", "\\1", file_name)  # Extract the seed number
    read_delim(.x, col_names = FALSE) %>%
      rename(gen = X1, pop = X2) %>%
      pivot_longer(cols = -c(gen, pop), names_to = "sample", values_to = "fitness") %>%
      mutate(seed = seed) 
  })
no_recomb_genotype <- list.files(path = lib_dir, pattern = "no_recomb_genotypes_.*\\.csv", full.names = TRUE) %>%
  map_df(~ {
    file_name <- basename(.x)  # Get the file name without the path
    seed <- gsub(".*no_recomb_genotypes_(\\d+).*", "\\1", file_name)  # Extract the seed number
    read_delim(.x, col_names = FALSE) %>%
      rename(gen = X1, pop = X2) %>%
      pivot_longer(cols = -c(gen, pop), names_to = "sample", values_to = "genotypes") %>%
      mutate(seed = seed) 
  })
no_recomb_fit <- no_recomb_fit %>%
  mutate(sample = paste0(gsub("\\D", "", pop), "_", sample))
no_recomb_genotype <- no_recomb_genotype %>%
  mutate(sample = paste0(gsub("\\D", "", pop), "_", sample))


no_recomb <- no_recomb_fit %>%
  inner_join(no_recomb_genotype, by = c("gen", "pop", "sample", "seed"))

no_recomb_frequency <- no_recomb %>%
  filter(!is.na(genotypes), gen==199000) %>%
  group_by(pop, genotypes) %>%
  summarise(count = n(), .groups = "drop")

no_recomb_frequency %>%
  ggplot(.,aes(x=as.factor(genotypes),y=count,group=genotypes,fill=as.factor(genotypes))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Genotype", y = "Total count", title = "Individual counts at 199,000 generations") +
  scale_fill_discrete(name = "Inversion Genotype", labels = c("Non-inverted", "Heterozygous", "Homozygous")) + 
  theme(text = element_text(size = 20))
  #ylim(0,25000)


