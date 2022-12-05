#### Wrangling data for service use by cause of death


# Packages ----------------------------------------------------------------

library(tidyverse)

source("./FUNCTIONS.R", local = TRUE)  # load helper functions


# Setup -------------------------------------------------------------------

## create directory for outputs if not yet present
outputs_dir <- "X:/R2090/2021-0312 Deaths at home/outputs/service_use_by_cod/"
if(!dir.exists(outputs_dir)) dir.create(outputs_dir)


# Load data ---------------------------------------------------------------

cod_raw <- 
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_by_pod_cod_underlying.csv")


# Wrangling ---------------------------------------------------------------

## convert to long format and calculate annual proportion of each CoD
cod_all_pod <-
  cod_raw %>%
  pivot_annual_to_long %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  group_by(cohort_year, cat_cod_nrs) %>%
  summarise(
    n = sum(n, na.rm = TRUE),  # C19 is NA before pandemic
    .groups = "drop"
  ) %>%
  group_by(cohort_year) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup %>%
  mutate(n = if_else(n==0, NA_real_, n))

## convert to wide format for pretty tables
cod_all_pod_wide <-
  cod_all_pod %>%
  mutate(
    n_prop = calculate_n_prop(n, prop)
  ) %>%
  select(-c(n,prop)) %>%
  pivot_wider(names_from = cohort_year, values_from = n_prop) 

cod_all_pod_wide %>%
  write_csv(file = file.path(outputs_dir, "deaths_by_cod_underlying.csv"))

## Add 'all PoD' line to original table
cod_pod <-
  bind_rows(
    cod_raw,
    cod_all_pod_wide %>% mutate(cat_place_of_death = "All")
    ) %>%
  mutate(cat_place_of_death = factor(cat_place_of_death, levels = c("All", order_place_of_death))) %>%
  arrange(cat_place_of_death, cat_cod_nrs)

## write to csv
cod_pod  %>%
  write_csv(file = file.path(outputs_dir, "deaths_by_cod_pod_incl_all_pod.csv"))
