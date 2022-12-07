library(tidyverse)
setwd("/Users/alexpinch/GitHub/inversion_model")
all_data <- tibble()
files <- length(list.files("/Users/alexpinch/GitHub/inversion_model/data_het_adv_111522/lost_runs"))
lost_runs <- list.files("/Users/alexpinch/GitHub/inversion_model/data_het_adv_111522/lost_runs")
for (i in 1:files) {
  run <- (lost_runs[i])
  typeof(run)
  setwd(file.path("/Users/alexpinch/GitHub/inversion_model/data_het_adv_111522/lost_runs/",run))
  run_data <- read.csv(file = paste(run,"_seed.csv",sep=""),skip=0,header=F) %>%
    rename(seed=V1,gen=V2)
  run_data <- run_data %>% mutate(sim_run=run)
  all_data <- rbind(all_data, run_data)
}

ggplot(all_data, aes(x=gen))+geom_freqpoly(size=1, alpha=I(.6), colour="blue")
