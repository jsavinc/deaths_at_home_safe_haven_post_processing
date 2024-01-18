# deaths at home, descriptive paper 1 scripts



# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)

source("./FUNCTIONS.R")  # load common functions



# load & wrangle data -----------------------------------------------------

## load urban-rural table for the different level folds
urban_rural_reference <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/urban_rural_8_6_3_2_fold_table.csv") %>%
  mutate(across(everything(), ~factor(.x, levels = unique(.x))))

## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(n=sum(n), .groups = "drop")

pod <- cohort_by_pod %>%
  group_by(val_cohort_year, cat_place_of_death=="All") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup %>%
  select(val_cohort_year, cat_place_of_death, n, prop)

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

ur8_by_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/place_of_death_by_ur_and_cohort.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(cat_ur8 = factor(cat_ur8, levels = unique(cat_ur8))) %>%
  ## ur is reported as proportion of each UR level by PoD; for this paper I think I need proportion of URs across each PoD
  group_by(val_cohort_year, cat_place_of_death) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup

ur2_by_pod <-
  ur8_by_pod %>%
  left_join(urban_rural_reference, by = "cat_ur8") %>%
  group_by(val_cohort_year, cat_place_of_death, cat_ur2) %>%
  summarise(
    n = sum(n),
    prop = sum(prop),
    .groups = "drop"
  )

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

missingness_per_year <-
  map2_dfr(
    .x = list(marstat_by_pod, ur8_by_pod, simd_by_pod),
    .y = c("Marital status", "Urban-rural location", "Deprivation (SIMD)"),
    .f = function(data_tbl, data_name) {
      data_tbl %>%
        pivot_longer(cols = any_of(
          c(
            "cat_marital_status_condensed",
            "val_simd_quintile",
            "cat_ur8"
          )
        ),
        names_to = "variable",
        values_to = "value") %>%
        count(val_cohort_year, value, wt = n) %>%
        group_by(val_cohort_year) %>%
        mutate(prop_missing = scales::label_percent(accuracy = 0.01)(n/sum(n))) %>%
        ungroup %>%
        filter(is.na(value) | value == "Missing") %>%
        mutate(value = "Missing", variable = data_name) %>%
        relocate(variable)
    }
  )

# compose tables ----------------------------------------------------------

table1 <-
  bind_rows(
    pod %>%
      mutate(measure = "Place of death, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
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
    ur2_by_pod %>%
      filter(!is.na(cat_ur2)) %>%
      mutate(measure = "Urban-rural indicator, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_ur2, value),
  ) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
    cat_place_of_death = factor(cat_place_of_death, levels = c(order_place_of_death, "All"))
    ) %>%
  arrange(measure, cat_place_of_death) %>%
  mutate(levels = replace_na(levels, " ")) %>%
  ## leaves repeat entries in table blank for nicer formatting
  group_by(measure) %>%
  mutate(
    cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
    ) %>%
  ungroup

table1 %>% print_all

table1_average_prepandemic <-
  bind_rows(
    pod %>%
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      summarise(
        measure = "Place of death, N (%)",
        total = mean(n/prop),
        n = mean(n),
        prop = n/total,
        value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})"),
        .groups = "drop"
        ) %>%
      select(measure, cat_cohort, cat_place_of_death, value),
    age_by_pod %>% 
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      summarise(
        measure = "Age, M (SD)",
        mean = weighted.mean(mean, w = n_deaths),
        sd = sd_pooled(sd, n_deaths),
        value = glue::glue("{round(mean,2)} ({round(sd,2)})"),
        .groups = "drop"
      ) %>%
      select(measure, cat_cohort, cat_place_of_death, value),
    sex_by_pod %>%
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      summarise(
        measure = "Female, N (%)",
        total = mean(n/prop),
        n = mean(n),
        prop = n/total,
        value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})"),
        .groups = "drop"
      ) %>%
      select(measure, cat_cohort, cat_place_of_death, value),
    marstat_by_pod %>%
      filter(cat_marital_status_condensed != "Missing") %>%
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death, cat_marital_status_condensed) %>%
      summarise(
        n = mean(n),
        .groups = "drop"
      ) %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      mutate(prop = n/sum(n)) %>%
      ungroup %>%
      mutate(measure = "Marital status, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, cat_cohort, cat_place_of_death, levels = cat_marital_status_condensed, value),
    ethnicity_by_pod %>%
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death, cat_ethnic_group_collapsed) %>%
      summarise(
        n = mean(n),
        .groups = "drop"
      ) %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      mutate(prop = n/sum(n)) %>%
      ungroup %>%
      mutate(measure = "Ethnicity, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, cat_cohort, cat_place_of_death, levels = cat_ethnic_group_collapsed, value),
    simd_by_pod %>%
      filter(!is.na(val_simd_quintile)) %>%
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death, val_simd_quintile) %>%
      summarise(
        n = mean(n),
        .groups = "drop"
      ) %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      mutate(prop = n/sum(n)) %>%
      ungroup %>%
      mutate(measure = "Deprivation, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, cat_cohort, cat_place_of_death, levels = val_simd_quintile, value) %>%
      mutate(levels = if_else(levels==1, "1 (most deprived)", as.character(levels))),
    ur2_by_pod %>%
      filter(!is.na(cat_ur2)) %>%
      create_pandemic_variable() %>%
      group_by(cat_cohort, cat_place_of_death, cat_ur2) %>%
      summarise(
        n = mean(n),
        .groups = "drop"
      ) %>%
      group_by(cat_cohort, cat_place_of_death) %>%
      mutate(prop = n/sum(n)) %>%
      ungroup %>%
      mutate(measure = "Urban-rural indicator, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, cat_cohort, cat_place_of_death, levels = cat_ur2, value),
  ) %>%
  pivot_wider(names_from = cat_cohort, values_from = value)
  
# TODO: add averages for 2015-2019 aggregated
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
  arrange(measure, cat_place_of_death) %>%
  ## leaves repeat entries in table blank for nicer formatting
  group_by(measure) %>%
  mutate(
    cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
  ) %>%
  ungroup

table2 %>% print_all

## suppl table 1 contains the full 8-fold Urban-rural split of the cohort
supplementary_table1 <-
  bind_rows(
    age_group_by_pod %>%
      mutate(measure = "Age group, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, levels = cat_age_5, value),
    ur8_by_pod %>%
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
  ## leaves repeat entries in table blank for nicer formatting
  group_by(measure) %>%
  mutate(
    cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
  ) %>%
  ungroup

## suppl table 2 contains the CoD count & comorb count frequencies, showing that
## the change is largely in the 0/1 categories

# TODO: this is an alternative categorisation of 5+ comorbidities, maybe clearer to show differences here
num_comorb_categorised_by_pod %>%
  mutate(
    cat_comorb_count = factor(cat_comorb_count, levels = c(as.character(0:10),"11+")),
    cat_comorb_count_5 = fct_collapse(cat_comorb_count, "5+" = c(as.character(5:10),"11+"))
    ) %>%
  group_by(cat_place_of_death, val_cohort_year, cat_comorb_count_5) %>%
  summarise(
    n = sum(n), .groups = "drop"
  ) %>%
  group_by(cat_place_of_death, val_cohort_year) %>%
  mutate(prop = n/sum(n)) %>%
  mutate(measure = "Recorded comorbidities, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
  select(measure, val_cohort_year, cat_place_of_death, levels = cat_comorb_count_5, value) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value)

supplementary_table2 <-
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
  arrange(measure, cat_place_of_death) %>%
  ## leaves repeat entries in table blank for nicer formatting
  group_by(measure) %>%
  mutate(
    cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
  ) %>%
  ungroup

if(!dir.exists("X:/R2090/2021-0312 Deaths at home/outputs/descriptive_paper_1")) dir.create("X:/R2090/2021-0312 Deaths at home/outputs/descriptive_paper_1")

write.xlsx(
  x = list(
    "table1" = table1,
    "table1, prepandemic avg" = table1_average_prepandemic,
    "table2" = table2,
    "suppl_table1" = supplementary_table1,
    "suppl_table2" = supplementary_table2,
    "missingness_per_year" = missingness_per_year
  ),
  file = "X:/R2090/2021-0312 Deaths at home/outputs/descriptive_paper_1/descriptive_paper_1_tables.xlsx"
)


# plots -------------------------------------------------------------------

(
fig_age_group_5_home_line <-
  age_group_by_pod %>%
  filter(cat_place_of_death == "Home & non-institution") %>%
  ggplot(
    data = .,
    aes(x = val_cohort_year, y = prop, colour = cat_age_5, group = cat_age_5, shape = cat_age_5)
  ) +
  geom_point(size = rel(2)) +
  geom_line() +
  facet_wrap(~cat_place_of_death) +
  scale_colour_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL)
)
(
fig_age_group_5_home <- 
  age_group_by_pod %>%
  filter(cat_place_of_death == "Home & non-institution") %>%
  ggplot(
    data = .,
    aes(x = val_cohort_year, y = n, group = cat_age_5, fill = cat_age_5)
  ) +
  # geom_point(size = rel(2)) +
  geom_col(position = position_dodge(width = 1)) +
  ggrepel::geom_text_repel(aes(label = scales::label_percent(accuracy=0.1)(prop))) +
  facet_wrap(~cat_place_of_death) +
  # scale_colour_viridis_d(direction = -1) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  labs(x = NULL, y = NULL) 
)
