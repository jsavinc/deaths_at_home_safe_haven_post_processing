# deaths at home, descriptive paper 1 scripts



# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

source("./FUNCTIONS.R")  # load common functions



# load & wrangle data -----------------------------------------------------


## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(n=sum(n), .groups = "drop")

## note, these aren't broken down by place of death and are of limited use
demographics_of_cohort <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/cohort_demographics.csv")

age_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_age_cohort_pod.csv")
## some age groups merged
age_group_by_pod <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/age_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop)

## this is aggregated by cause of death but there's also an 'all causes' category
sex_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_underlying_sex.csv") %>%
  filter(cat_cod_nrs == "All causes") %>%
  select(-cat_cod_nrs) %>%  # won't need this for now
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop)

marstat_by_pod <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/marstat_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop)

ethnicity_by_pod <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/ethnicity_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop)

## rounded to nearest 10
simd_by_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/place_of_death_by_simd_and_cohort.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  ## simd is reported as proportion of each simd quintile in each PoD, e.g. all
  ## PoDs in quintile 1 add up to 100%; so it needs rebased to proportion of
  ## quintiles within each PoD
  group_by(val_cohort_year, cat_place_of_death) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup

ur_by_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/place_of_death_by_ur_and_cohort.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(cat_ur8 = factor(cat_ur8, levels = unique(cat_ur8))) %>%
  ## ur is reported as proportion of each UR level by PoD; for this paper I think I need proportion of URs across each PoD
  group_by(val_cohort_year, cat_place_of_death) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup

num_cod_by_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_count.csv") %>%
  pivot_annual_to_long(new_col = "mean_ci") %>%
  parse_m_ci(col_m_ci = mean_ci) %>%
  left_join(
    cohort_by_pod %>% rename(denominator = n),
    by = c("cat_place_of_death", "val_cohort_year")
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = m, ci_lo_var = ci_lo, n_var = denominator, new_sd_var = sd)

num_cod_categorised_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/cod_count_categorised_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop)

## this includes both number of comorbidities & elixhauser index
comorb_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_comorb_index.csv") %>%
  pivot_annual_to_long(new_col = "mean_ci") %>%
  parse_m_ci(col_m_ci = mean_ci) %>%
  left_join(
    cohort_by_pod %>% rename(denominator = n),
    by = c("cat_place_of_death", "val_cohort_year")
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = m, ci_lo_var = ci_lo, n_var = denominator, new_sd_var = sd)

## number of comorbidities, categorised
num_comorb_categorised_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/comorb_count_categorised_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop)

## recalculated from the breakdown by sex
pall_care_needs_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/palliative_care_needs_exploration/palliative_estimates_murtaugh_upper_mid_by_cohort_pod_sex.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(denominator = round(n / prop)) %>%  # infer denominator from proportions
  group_by(cat_place_of_death, val_cohort_year) %>%
  summarise(
    n = sum(n), 
    denominator = sum(denominator), 
    .groups="drop"
    ) %>%
  mutate(prop = n / denominator)

# TODO: import the breakdowns of pall care needs by other variables if needed

pal_care_needs_by_pod_simd <- NULL



# preliminaries, missingness ----------------------------------------------

## approx. 0.2% missing per year
missing_proportion_marital_status <-
  marstat_by_pod %>%
  group_by(val_cohort_year, cat_marital_status_condensed) %>%
  summarise(n=sum(n), .groups = "drop") %>% 
  group_by(val_cohort_year) %>%
  mutate(prop_missing = scales::label_percent(accuracy = 0.01)(n/sum(n))) %>%
  ungroup %>%
  filter(cat_marital_status_condensed == "Missing")

missing_proportion_simd <-
  simd_by_pod %>%
  group_by(val_cohort_year, val_simd_quintile) %>%
  summarise(n=sum(n), .groups = "drop") %>% 
  group_by(val_cohort_year) %>%
  mutate(prop_missing = scales::label_percent(accuracy = 0.01)(n/sum(n))) %>%
  ungroup %>%
  filter(is.na(val_simd_quintile))

missing_proportion_simd <-
  ur_by_pod %>%
  group_by(val_cohort_year, cat_ur8) %>%
  summarise(n=sum(n), .groups = "drop") %>% 
  group_by(val_cohort_year) %>%
  mutate(prop_missing = scales::label_percent(accuracy = 0.01)(n/sum(n))) %>%
  ungroup %>%
  filter(is.na(cat_ur8))

# compose tables ----------------------------------------------------------

table1 <-
  bind_rows(
    age_by_pod %>% 
      mutate(measure = "Age, M (SD)", value = glue::glue("{round(mean,2)} ({round(sd,2)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    sex_by_pod %>%
      mutate(measure = "Female, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    marstat_by_pod %>%
      filter(cat_marital_status_condensed != "Missing") %>%
      mutate(measure = "Marital status, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_marital_status_condensed, value),
    ethnicity_by_pod %>%
      mutate(measure = "Ethnicity, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_ethnic_group_collapsed, value),
    simd_by_pod %>%
      filter(!is.na(val_simd_quintile)) %>%
      mutate(measure = "Deprivation, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = val_simd_quintile, value) %>%
      mutate(levels = if_else(levels==1, "1 (most deprived)", as.character(levels))),
    ur_by_pod %>%
      filter(!is.na(cat_ur8)) %>%
      mutate(measure = "Urban-rural indicator, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_ur8, value),
  ) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
    ) %>%
  arrange(measure, cat_place_of_death) %>%
  mutate(levels = replace_na(levels, " "))

table1 %>% print_all

table2 <-
  bind_rows(
    num_cod_by_pod %>%
      mutate(measure = "Recorded causes of death, M (SD)", value = glue::glue("{round(m,2)} ({round(sd,2)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    comorb_by_pod %>%
      filter(measure == "Elixhauser comorbidity index") %>%
      mutate(measure = "Comorbidity index, M (SD)", value = glue::glue("{round(m,2)} ({round(sd,2)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    comorb_by_pod %>%
      filter(measure == "Number of comorbidities incl. in the Elixhauser index") %>%
      mutate(measure = "Recorded comorbidities, M (SD)", value = glue::glue("{round(m,2)} ({round(sd,2)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    pall_care_needs_by_pod %>%
      mutate(measure = "Palliative care needs, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
  ) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
    cat_place_of_death = factor(cat_place_of_death, levels = c(order_place_of_death,"All"))
  ) %>%
  arrange(measure, cat_place_of_death)

table2 %>% print_all

supplementary_table_1 <-
  bind_rows(
    num_cod_categorised_by_pod %>%
      mutate(measure = "Recorded causes of death, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_cod_count, value),
    num_comorb_categorised_by_pod %>%
      mutate(measure = "Recorded comorbidities, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_comorb_count, value),
  ) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
    cat_place_of_death = factor(cat_place_of_death, levels = c(order_place_of_death,"All"))
  ) %>%
  arrange(measure, cat_place_of_death)

if(!dir.exists("X:/R2090/2021-0312 Deaths at home/outputs/descriptive_paper_1")) dir.create("X:/R2090/2021-0312 Deaths at home/outputs/descriptive_paper_1")

write.xlsx(
  x = list(
    "table1" = table1,
    "table2" = table2,
    "suppl_table1" = supplementary_table_1
  ),
  file = "X:/R2090/2021-0312 Deaths at home/outputs/descriptive_paper_1/descriptive_paper_1_tables.xlsx"
)
