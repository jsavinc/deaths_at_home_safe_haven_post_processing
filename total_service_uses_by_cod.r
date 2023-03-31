## Computing high-level overviews of service use - person-days in hospital and
## A&E, total number of hospital spells, totals of A&E admissions, OOH GP
## contacts, NHS24 calls, ambulance uses - all by cohort & PoD & cause of death!

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R", local = TRUE)  # load helper functions

# load data ---------------------------------------------------------------

## cohort by pod and cod
cohort_by_pod_cod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_by_pod_cod_underlying (1).csv") %>%
  pivot_annual_to_long() %>%
  rename(val_cohort_year = cohort_year) %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death, cat_cod_nrs) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  )

# TODO: import the different service usage by CoD metrics and combine into a single plot somehow, facet by service & CoD?
hospital_los_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_underlying_mean_ae_admissions.csv") %>%
  pivot_annual_to_long(new_col = "mean_ci") %>%
  rename(val_cohort_year = cohort_year) %>%
  parse_m_ci(col_m_ci = "mean_ci")
  