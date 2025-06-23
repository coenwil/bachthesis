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
      # both are > eta
      (p_h1 >= eta & p_hc >= eta) ~ 1,
      
      # sometimes one is within 0.1 due to precision error. still allow that
      (p_h1 >= eta & p_hc >= eta - 0.1) ~ 1,
      (p_hc >= eta & p_h1 >= eta - 0.1) ~ 1,
      
      TRUE ~ 0
    ),
    trial_aligned = case_when(
      (hypothesis %in% c("non-inferiority", "superiority") & (beta_1 == 1)) ~ 1,
      (hypothesis == "equivalence" & (beta_1 == 0)) ~ 1,
      TRUE ~ 0
    )
  )

# looking into proportion of iterations failed due to no variance
sum_novar <- sum(prio_sim$prop_fail)
prop_novar <- sum(prio_sim$prop_fail) / nrow(prio_sim)
max_novar <- max(prio_sim$prop_fail)

# there is one set (sim_id = 30) that had n_min = 1000
# but an n_max of 87. so manually add it in the results section
# explains why here it would appear that 10 out 11 trial aligned sets were
# conclusive. In reality it's 11/12.
prio_sim %>% 
  filter(n_max < 101, sim_id != 6) %>% 
  count(conclusive, hypothesis)

# the one failed row
prio_sim %>%
  filter(n_max > 100, conclusive == 0, trial_aligned == 1)

# mean sample size for the smaller range
prio_sim %>% 
  filter(conclusive == 1, n_max < 101, sim_id != 30) %>% 
  summarise(conc_correct_n = mean(n))

# mean sample size for the bigger range
prio_sim %>% 
  filter((conclusive == 1 & n_max > 100) | sim_id == 30) %>% 
  summarise(conc_correct_n = mean(n),
            max_correct_n = max(n))

prio_sim %>% 
  filter(conclusive == 1, bf_thresh == 10) %>% 
  summarise(conc_correct_n = mean(n))

prio_sim %>% 
  filter(conclusive == 1, hypothesis == "equivalence") %>% 
  summarise(conc_correct_n = mean(n))

