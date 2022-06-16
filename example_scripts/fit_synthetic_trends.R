
rm(list =ls())
library(tidyverse)
library(icd)
source("functions/fit_trend_funcs.R")
source("functions/misc_funcs.R")

######################
#### Prepare Data ####
######################

## Load Count Data --------------------------------------------------------------

# Load counts of total number of patients with visit each day
visit_counts <- read_csv("data/synthetic_visit_counts_tb.csv")

# Load daily visit counts (number of patients with each diagnosis code each day)
daily_dx_counts <- read_csv("data/synthetic_dx_counts_tb.csv")

# compute normalized values and prep daily dx counts for model fitting
daily_dx_counts <- daily_dx_counts %>%
  left_join(visit_counts, by = "days_since_index") %>%
  mutate(frac = n/n_patient_visits) %>%                            # compute the fraction of patient visits with the diagnosis
  group_by(dx) %>%
  mutate(norm_n = (frac-min(frac))/(max(frac)-min(frac))) %>%      # normalize fraction of visits between [0,1]
  mutate(period = -days_since_index) %>%                           # create period for fitting (positive value of days since index)
  ungroup()

# Initialize dataset for model parameters and get icd_descriptions
model_params <- daily_dx_counts %>%
  group_by(dx) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(index =1:n()) %>%
  mutate(description = map_chr(dx,~explain_code_safe(icd::as.icd9(.)))) %>%
  select(index,dx,description,n)

#########################################
##### Fit Models for Each ICD-9 Code ####
#########################################

### Fit linear models to daily data --------------------------------------------
out <- list()

for (i in 1:nrow(model_params)){

  print(i)

  visits_count <- daily_dx_counts %>%
    filter(dx == model_params$dx[i])

  out[[i]] <- extract_parameters(data = visits_count,var_name = "norm_n", method = "lm", week_effect = F) %>%
    mutate(index = i)
}

lm_dx9_normalized <- model_params %>%
  inner_join(bind_rows(out)) %>%
  select(-n)


### Fit models to weekly counts ------------------------------------------------

daily_dx_counts_7day <- daily_dx_counts %>%
  mutate(period=7*(period-1)%/%7+1) %>%
  group_by(dx,period) %>%
  summarise(n=sum(n),
            n_patient_visits=sum(n_patient_visits)) %>%
  ungroup() %>%
  mutate(frac = n/n_patient_visits) %>%
  group_by(dx) %>%
  mutate(norm_n = (frac-min(frac))/(max(frac)-min(frac))) %>%
  ungroup()

out <- list()

for (i in 1:500){

  print(i)

  visits_count <- daily_dx_counts_7day %>%
    filter(dx == model_params$dx[i])

  out[[i]] <- extract_parameters(data = visits_count,var_name = "norm_n", method = "lm", week_effect = F) %>%
    mutate(index = i)
}


lm_dx9_normalized_7day <- model_params %>%
  inner_join(bind_rows(out)) %>%
  select(-n)


### Save final output ----------------------------------------------------------

lm_dx9_normalized %>%
  select(index,code=dx,description,cp:b2) %>%
  write_csv("data/sythetic_fits_tb_1day.csv")

lm_dx9_normalized_7day %>%
  select(index,code=dx,description,cp:b2) %>%
  write_csv("data/sythetic_fits_tb_7day.csv")



