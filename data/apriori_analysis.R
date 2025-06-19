# file to process data and make it usable for analysis

# packages
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)

# loading in the data
prio_sim <- fread("data/apriori_sim_parallel.csv")

prio_sim <- prio_sim %>% 
  mutate(
    conclusive = case_when(
    (p_h1 >= eta - 0.1 & p_hc >= eta - 0.1) ~ 1,
    TRUE ~ 0
    )
  )

prio_sim %>% filter(beta_1 == 1, n > 100) %>% count(conclusive, hypothesis)