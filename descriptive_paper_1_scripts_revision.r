## script for the revisions to descriptive paper 1

# constants ---------------------------------------------------------------

dir_output <- 
  "C:/Users/40011625/OneDrive - Edinburgh Napier University/SCADR/covid_mortality_home/drafts/descriptive paper 1/IJPDS/revision 1/"

levels_place_of_death <- c("Hospital", "Home", "Care home & other", 
                           "All")


# functions ---------------------------------------------------------------

code_place_of_death_consistently <- function(data_tbl) {
  data_tbl %>%
    mutate(
      ## shorten to "Home"
      cat_place_of_death = if_else(cat_place_of_death == "Home & non-institution", true = "Home", false = cat_place_of_death),
      cat_place_of_death = factor(cat_place_of_death, levels = levels_place_of_death)
    )
}

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(patchwork)  # for assembling plots
library(ggedit)  # for editing plots after they've been defined


source("./FUNCTIONS.R")  # load common functions


# set figure theme --------------------------------------------------------

theme_descriptive_paper_1 <-
  theme_minimal(base_size = 7) +
  theme(
    text = element_text(family = "sans"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
    # remove gap to the left of y axis title and below the x axis title
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
theme_set(theme_descriptive_paper_1)
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
  summarise(n=sum(n), .groups = "drop") %>%
  code_place_of_death_consistently

pod <- cohort_by_pod %>%
  group_by(val_cohort_year, cat_place_of_death=="All") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup %>%
  select(val_cohort_year, cat_place_of_death, n, prop) %>%
  code_place_of_death_consistently

## note, these aren't broken down by place of death and are of limited use
demographics_of_cohort <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/cohort_demographics.csv")

age_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_age_cohort_pod.csv") %>%
  code_place_of_death_consistently

## some age groups merged
## this is the newer, "decade" age group
age_group_by_pod <- 
  # read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/age_by_pod.csv") %>%
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/part 3/tbl_age_decades_by_pod.csv") %>%
  # pivot_annual_to_long() %>%
  # parse_n_prop(col_n_prop = n_prop) %>%
  code_place_of_death_consistently %>%
  mutate(cat_age_decades = factor(cat_age_decades))

## this is aggregated by cause of death but there's also an 'all causes' category
## note, only one proportion shown = proportion female
sex_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_underlying_sex.csv") %>%
  filter(cat_cod_nrs == "All causes") %>%
  select(-cat_cod_nrs) %>%  # won't need this for now
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  code_place_of_death_consistently

marstat_by_pod <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/marstat_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(cat_marital_status_condensed = factor(cat_marital_status_condensed, levels = unique(cat_marital_status_condensed))) %>%
  code_place_of_death_consistently

ethnicity_by_pod <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/ethnicity_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(cat_ethnic_group_collapsed = factor(cat_ethnic_group_collapsed, levels = unique(cat_ethnic_group_collapsed))) %>%
  code_place_of_death_consistently

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
  ungroup %>%
  code_place_of_death_consistently %>%
  mutate(cat_simd_quintile = factor(as.character(val_simd_quintile), levels = as.character(1:5)))

ur8_by_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/place_of_death_by_ur_and_cohort.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(cat_ur8 = factor(cat_ur8, levels = unique(cat_ur8))) %>%
  ## ur is reported as proportion of each UR level by PoD; for this paper I think I need proportion of URs across each PoD
  group_by(val_cohort_year, cat_place_of_death) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup %>%
  code_place_of_death_consistently

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
  code_place_of_death_consistently %>%
  left_join(
    cohort_by_pod %>% rename(denominator = n),
    by = c("cat_place_of_death", "val_cohort_year")
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = m, ci_lo_var = ci_lo, n_var = denominator, new_sd_var = sd)

num_cod_categorised_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/cod_count_categorised_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  code_place_of_death_consistently

## this includes both number of comorbidities & elixhauser index
comorb_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_comorb_index.csv") %>%
  pivot_annual_to_long(new_col = "mean_ci") %>%
  parse_m_ci(col_m_ci = mean_ci) %>%
  code_place_of_death_consistently %>%
  left_join(
    cohort_by_pod %>% rename(denominator = n),
    by = c("cat_place_of_death", "val_cohort_year")
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = m, ci_lo_var = ci_lo, n_var = denominator, new_sd_var = sd)

## number of comorbidities, categorised
num_comorb_categorised_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/comorb_count_categorised_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  code_place_of_death_consistently

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
  mutate(prop = n / denominator) %>%
  code_place_of_death_consistently

# TODO: import the breakdowns of pall care needs by other variables if needed

pal_care_needs_by_pod_simd <- NULL

## polypharmacy score by pod - BNF sections
bnf_sections_by_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/prescriptions and dispensing changes August 2024/tbl_mean_disp_bnf_chapters_pod.csv") %>%
  filter(variable == "sections") %>%
  code_place_of_death_consistently

# compose tables ----------------------------------------------------------

# outline of data wrangling for odds ratio tables
# 1. harmonise variables (variable - level - n)
# 2. remove All PoDs
# 3. merge 2015-20 - "pre-pandemic"; 2020-21 - "pandemic"
# 4. merge PoD - Home vs Institution (everything else)
# 5. sum across time & PoD
# 6. pivot wider, keeping variable groupings - pandemic X home/institution
# 7. compute odds & odds ratio

## list of variables to do this for:
# sex X
# age group X
# marstat X
# ethnicity X
# SIMD X
# U-R X


# wrangle some tables again -----------------------------------------------

sex_by_pod_2 <-
  sex_by_pod %>%
  left_join(cohort_by_pod %>% rename(denominator = n), join_by(val_cohort_year, cat_place_of_death)) %>%
  rename(
    n_female = n,
    prop_female = prop
    ) %>%
  mutate(
    n_male = denominator - n_female,
    prop_male = 1 - prop_female
    ) %>%
  (function(x){
    bind_rows(
      x %>% select(1:2, n = n_female, prop = prop_female) %>% mutate(cat_sex = "female"),
      x %>% select(1:2, n = n_male, prop = prop_male) %>% mutate(cat_sex = "male")
    )
  }) %>%
  mutate(cat_sex = factor(cat_sex)) %>%
  relocate(cat_sex, .after = val_cohort_year)

# TODO: get the more granualr age grouping from safe haven!
age_group_by_pod_2 <-
  age_group_by_pod %>%
  # mutate(cat_age_5 = fct_collapse(cat_age_5, "18-44" = c("18-24", "25-44"))) %>%
  count(val_cohort_year, cat_place_of_death, cat_age_decades, wt = n)
  

# compile odds ratios table --------------------------------------------------

## helper function to collapse place of death into Home vs Institution
## also to collapse pandemic vs pre-pandemic period
merge_pod_and_years_n <- function(data_tbl) {
  data_tbl %>%
    filter(cat_place_of_death != "All") %>%
    mutate(
      cat_place_of_death = fct_collapse(.f = cat_place_of_death, "Home" = c("Home"), "Institutional" = c("Hospital","Care home & other")) %>%
        fct_relevel("Home"),
      val_cohort_year = fct_collapse(.f = val_cohort_year, "2020-21" = c("2020-21"), other_level = "2015-20") %>%
        fct_relevel("2015-20")
      ) %>%
    group_by(val_cohort_year, cat_place_of_death, .add = TRUE) %>%
    summarise(n = sum(n), .groups = "drop")
    # summarise(n = sum(n), .groups = "keep") %>%
    # ungroup(val_cohort_year, cat_place_of_death)
}
merge_pod_and_years_m_sd <- function(data_tbl) {
  data_tbl %>%
    filter(cat_place_of_death != "All") %>%
    mutate(
      cat_place_of_death = fct_collapse(.f = cat_place_of_death, "Home" = c("Home"), "Institutional" = c("Hospital","Care home & other")) %>%
        fct_relevel("Home"),
      val_cohort_year = fct_collapse(.f = val_cohort_year, "2020-21" = c("2020-21"), other_level = "2015-20") %>%
        fct_relevel("2015-20")
    ) %>%
    group_by(val_cohort_year, cat_place_of_death, .add = TRUE) %>%
    summarise(mean = weighted.mean(mean, n_deaths),
              sd = sd_pooled(sd, n_deaths),
              .groups = "drop")
}
## helper function to merge all pre-pandemic years into one
merge_years_n <- function(data_tbl) {
  data_tbl %>%
    mutate(
      val_cohort_year = fct_collapse(.f = val_cohort_year, "2020-21" = c("2020-21"), other_level = "2015-20") %>%
        fct_relevel("2015-20")
      ) %>%
    group_by(val_cohort_year, .add = TRUE) %>%
    summarise(n = sum(n), .groups = "drop")
}
merge_years_m_sd <- function(data_tbl) {
  data_tbl %>%
    mutate(
      val_cohort_year = fct_collapse(.f = val_cohort_year, "2020-21" = c("2020-21"), other_level = "2015-20") %>%
        fct_relevel("2015-20")
    ) %>%
    group_by(val_cohort_year, .add = TRUE) %>%
    summarise(mean = weighted.mean(mean, n_deaths),
              sd = sd_pooled(sd, n_deaths),
              .groups = "drop")
}

table_with_odds <-
  pmap(
    .l = tribble(
      ~dataset, ~var_name,
      sex_by_pod_2, "cat_sex",
      age_group_by_pod_2, "cat_age_5",
      marstat_by_pod, "cat_marital_status_condensed",
      ethnicity_by_pod, "cat_ethnic_group_collapsed",
      simd_by_pod %>% mutate(val_simd_quintile = factor(val_simd_quintile)), "val_simd_quintile",
      ur2_by_pod, "cat_ur2",
    ),
    .f = function(dataset, var_name) {
      dataset %>%
        group_by(!!rlang::sym(var_name)) %>%
        merge_pod_and_years_n %>%
        pivot_wider(names_from = c(val_cohort_year, cat_place_of_death), values_from = n) %>%
        mutate(
          odds1 = `2015-20_Home` / `2015-20_Institutional`,
          odds2 = `2020-21_Home` / `2020-21_Institutional`,
          or = odds2 / odds1
        ) %>%
        mutate(
          variable = replace_with_long_variable_name(var_name)
        ) %>%
        rename(level := {{var_name}})
    }
  ) %>%
  list_rbind() %>%
  relocate(variable, level, `2015-20_Home`, `2015-20_Institutional`, odds1, `2020-21_Home`, `2020-21_Institutional`, odds2, or) %>%
  replace_na(replace = list(level = "Missing"))


# save table --------------------------------------------------------------

write_csv(table_with_odds, file = paste0(dir_output, "categorical_variables_with_odds_ratios.csv"))



# New descriptives tables: merged years -----------------------------------

# For the revised manuscript, I need a new table of descriptives that's less busy:
# - new age grouping (decades)
# - merge 2015-2019

## merged data

data_for_revised_descriptive_table <-
  list(
    "Age group, N (%)" = age_group_by_pod %>% rename(levels = cat_age_decades),
    "Sex, N (%)" = sex_by_pod_2 %>% rename(levels = cat_sex),
    "Marital status, N (%)" = marstat_by_pod %>% rename(levels = cat_marital_status_condensed),
    "Ethnicity, N (%)" = ethnicity_by_pod %>% rename(levels = cat_ethnic_group_collapsed),
    "Deprivation, N (%)" = simd_by_pod %>% rename(levels = cat_simd_quintile),
    "Urban-rural classification, N (%)" = ur2_by_pod %>% rename(levels = cat_ur2)
  )

revised_descriptive_table_categorical <-
  map(
    .x = data_for_revised_descriptive_table,
    .f = function(data_tbl) {
      data_tbl %>%
        group_by(cat_place_of_death, levels) %>%
        merge_years_n() %>%
        group_by(cat_place_of_death, val_cohort_year) %>%
        mutate(prop = n/sum(n)) %>%
        ungroup
    }
  ) %>%
  list_rbind(names_to = "measure") %>%
  mutate(value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
  select(-c(n,prop)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
  ) %>%
  arrange(measure, cat_place_of_death)

revised_descriptive_table_numeric <-
  age_by_pod %>%
  group_by(cat_place_of_death) %>% 
  merge_years_m_sd() %>%
  mutate(
    measure = "Age, Mean (SD)",
    value = glue::glue("{round(mean,2)} ({round(sd,2)})"),
    levels = " "
    ) %>%
  select(-c(mean,sd)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value)

revised_descriptive_table_pod_table <-
  pod %>%
  group_by(cat_place_of_death) %>%
  merge_years_n() %>%
  group_by(val_cohort_year, temp = cat_place_of_death=="All") %>%
  mutate(
    levels = " ",
    prop = n/sum(n),
    value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})"),
    measure = "Place of death, N (%)"
    ) %>%
  ungroup %>%
  select(-c(n, prop, temp)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value)

revised_descriptive_table <-
  bind_rows(  # merge the table "chunks"
    revised_descriptive_table_pod_table,
    revised_descriptive_table_numeric,
    revised_descriptive_table_categorical
  ) %>%
  # replace missing levels with "Missing"
  replace_na(list(levels = "Missing")) %>%
  ## leaves repeat entries in table blank for nicer formatting
  mutate(cat_place_of_death = if_else(cat_place_of_death == "Care home & other", as.factor("CH"), as.factor(cat_place_of_death))) %>%
  mutate(cat_place_of_death = factor(cat_place_of_death, levels=c("Home","Hospital","CH"))) %>%
  arrange(factor(measure, levels = unique(measure)),cat_place_of_death) %>%
  filter(cat_place_of_death!="All") %>%
  pivot_wider(names_from = cat_place_of_death, values_from = c(`2015-20`,`2020-21`), names_vary = "slowest") %>%
  group_by(measure) %>%
  mutate(
    # note: with pivoting place of death, this line is no longer needed
    # cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
  ) %>%
  ungroup %>%
  relocate(measure, levels)# rename Care home to CH

write_csv(revised_descriptive_table, file = paste0(dir_output, "revised_descriptive_table.csv"))


# Another new descriptives table: merged years & place of death -----------

# This is the same table as above, except I'll merge place of death into 2 categories: home and institution (hospital, care home, or other)

revised_descriptive_table_2_categorical <-
  map(
    .x = data_for_revised_descriptive_table,
    .f = function(data_tbl) {
      data_tbl %>%
        group_by(cat_place_of_death, levels) %>%
        merge_pod_and_years_n() %>%
        group_by(cat_place_of_death, val_cohort_year) %>%
        mutate(prop = n/sum(n)) %>%
        ungroup
    }
  ) %>%
  list_rbind(names_to = "measure") %>%
  mutate(value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
  select(-c(n,prop)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
  ) %>%
  arrange(measure, cat_place_of_death)

revised_descriptive_table_2_numeric <-
  age_by_pod %>%
  group_by(cat_place_of_death) %>% 
  merge_pod_and_years_m_sd() %>%
  mutate(
    measure = "Age, Mean (SD)",
    value = glue::glue("{round(mean,2)} ({round(sd,2)})"),
    levels = " "
  ) %>%
  select(-c(mean,sd)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value)

revised_descriptive_table_2_pod_table <-
  pod %>%
  group_by(cat_place_of_death) %>%
  merge_pod_and_years_n() %>%
  group_by(val_cohort_year, temp = cat_place_of_death=="All") %>%
  mutate(
    levels = " ",
    prop = n/sum(n),
    value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})"),
    measure = "Place of death, N (%)"
  ) %>%
  ungroup %>%
  select(-c(n, prop, temp)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value)

revised_descriptive_table_2 <-
  bind_rows(  # merge the table "chunks"
    revised_descriptive_table_2_pod_table,
    revised_descriptive_table_2_numeric,
    revised_descriptive_table_2_categorical
  ) %>%
  # replace missing levels with "Missing"
  replace_na(list(levels = "Missing")) %>%
  ## leaves repeat entries in table blank for nicer formatting
  pivot_wider(names_from = cat_place_of_death, values_from = c(`2015-20`,`2020-21`), names_vary = "slowest") %>%
  group_by(measure) %>%
  mutate(
    # note: with pivoting place of death, this line is no longer needed
    # cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
  ) %>%
  ungroup %>%
  relocate(measure, levels)

write_csv(revised_descriptive_table_2, file = paste0(dir_output, "revised_descriptive_table_merged_pod.csv"))


# clinical: palliative care needs, EI, polypharmacy table ---------------------------

revised_clinical_table2 <-
  bind_rows(
    comorb_by_pod %>%
      filter(measure == "Elixhauser comorbidity index") %>%
      rename(n_deaths = denominator, mean = m) %>%
      group_by(cat_place_of_death) %>%
      merge_years_m_sd() %>%
      mutate(measure = "EI, Mean (SD)", value = glue::glue("{round(mean,2)} ({round(sd,2)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    bnf_sections_by_pod %>%
      rename(n_deaths = n) %>%
      group_by(cat_place_of_death) %>%
      merge_years_m_sd() %>%
      mutate(measure = "Unique BNF sections, Mean (SD)", value = glue::glue("{round(mean,2)} ({round(sd,2)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
    pall_care_needs_by_pod %>%
      group_by(cat_place_of_death) %>%
      mutate(
        val_cohort_year = fct_collapse(.f = val_cohort_year, "2015-20" = c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20")) %>%
          fct_relevel("2015-20")
      ) %>%
      group_by(val_cohort_year, .add = TRUE) %>%
      summarise(n = sum(n), denominator = sum(denominator), .groups = "drop") %>%
      group_by(cat_place_of_death, val_cohort_year) %>%
      mutate(prop = n/denominator) %>%
      ungroup %>%
      mutate(measure = "Palliative care needs, N (%)", value = glue::glue("{scales::comma(n)} ({scales::label_percent(accuracy = 0.1)(prop)})")) %>%
      select(measure, val_cohort_year, cat_place_of_death, value),
  ) %>%
  pivot_wider(names_from = val_cohort_year, values_from = value) %>%
  mutate(
    measure = factor(measure, levels = unique(measure)),
  ) %>%
  arrange(measure, relevel(cat_place_of_death, "Home")) %>%
  ## leaves repeat entries in table blank for nicer formatting
  group_by(measure) %>%
  mutate(
    cat_place_of_death = if_else(condition = duplicated(cat_place_of_death), true = " ", false = as.character(cat_place_of_death)),
    measure = if_else(condition = duplicated(measure), true = " ", false = as.character(measure))
  ) %>%
  ungroup

revised_clinical_table2 %>%
  filter(cat_place_of_death!="All") %>%
  write_csv(
    ., 
    file = paste0(dir_output, "revised_clinical_table_merged_years.csv"))


# regression table -------------------------------------------------------

# without interactions
regression_coefs <- read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/part 3/tbl_regression_m1.csv")
# with interactions
regression_coefs_interactions <- read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/part 3/tbl_regression_m2.csv")

# one table per model
tbl_m1 <- 
  regression_coefs %>%
  mutate(variable = replace_with_long_variable_name(variable), 
         p = scales::label_pvalue()(p.value)
         ) %>%
  replace_na(list(level=" ")) %>%  # replace blank with space for excel handling
  mutate(ci = paste0(signif(conf.low,3),"-",signif(conf.high,3))) %>%
  select(variable, level, aOR=estimate, ci, p) %>%
  group_by(variable) %>%
  mutate(variable = if_else(duplicated(variable), " ", variable)) %>%
  ungroup

write_csv(tbl_m1, file = paste0(dir_output, "regression_coefs_m1.csv"))

tbl_m2 <-
  regression_coefs_interactions %>%
  mutate(
    is_interaction = if_else(str_detect(level, "Pandemic\\:"), TRUE, FALSE, missing = FALSE),
    variable = if_else(
      condition = is_interaction,
      true =
        str_extract(level, pattern = paste(regression_coefs_interactions$variable, collapse="|")),
      false = variable,
      missing = variable
    ),
    level = if_else(
      condition = is_interaction,
      true = str_remove(level, pattern = paste0("Pandemic\\:",variable)) %>% 
        if_else(.=="", NA_character_, .),
      false = level,
      missing = level
    )
  ) %>%
  transmute(
    variable,
    level,
    aOR = estimate,
    ci = paste0(signif(conf.low,3),"-",signif(conf.high,3)),
    p = scales::label_pvalue(accuracy = 0.0001)(p.value),
    is_interaction
  ) %>%
  arrange(factor(variable, levels = unique(variable)), factor(level, levels = unique(level)), is_interaction) %>%
  mutate(level = if_else(is_interaction, paste0(level, " × Pandemic"), level)) %>%
  select(-is_interaction) %>%
  mutate(variable = replace_with_long_variable_name(variable)) %>%
  replace_na(list(level=" ")) %>%  # to make it palatable to excel
  group_by(variable) %>%
  mutate(variable = if_else(duplicated(variable), " ", variable)) %>%
  ungroup

write_csv(tbl_m2, file = paste0(dir_output, "regression_coefs_m2.csv"))


tbl_m1_and_m2 <-
  full_join(
    # m1
    regression_coefs %>%
      mutate(is_interaction = FALSE) %>%
      transmute(
        variable,
        level,
        aOR = estimate,
        ci = paste0(signif(conf.low,3),",",signif(conf.high,3)),
        p = scales::label_pvalue(accuracy = 0.0001)(p.value),
        is_interaction
      )
    ,
    # m2
    regression_coefs_interactions %>%
      mutate(
        is_interaction = if_else(str_detect(level, "Pandemic\\:"), TRUE, FALSE, missing = FALSE),
        variable = if_else(
          condition = is_interaction,
          true =
            str_extract(level, pattern = paste(regression_coefs_interactions$variable, collapse="|")),
          false = variable,
          missing = variable
        ),
        level = if_else(
          condition = is_interaction,
          true = str_remove(level, pattern = paste0("Pandemic\\:",variable)) %>% 
            if_else(.=="", NA_character_, .),
          false = level,
          missing = level
        )
      ) %>%
      transmute(
        variable,
        level,
        aOR = estimate,
        ci = paste0(signif(conf.low,3),",",signif(conf.high,3)),
        p = scales::label_pvalue(accuracy = 0.0001)(p.value),
        is_interaction
      ),
    join_by(variable, level, is_interaction), suffix = c("_m1","_m2")
  ) %>%
    arrange(factor(variable, levels = unique(variable)), factor(level, levels = unique(level)), is_interaction) %>%
    mutate(level = if_else(is_interaction, paste0(level, " × Pandemic"), level)) %>%
    select(-is_interaction) %>%
    mutate(variable = replace_with_long_variable_name(variable)) %>%
    replace_na(list(level=" ")) %>%  # to make it palatable to excel
    group_by(variable) %>%
    mutate(variable = if_else(duplicated(variable), " ", variable)) %>%
    ungroup

write_csv(tbl_m1_and_m2, file = paste0(dir_output, "regression_coefs_m1_and_m2.csv"))


# # TODO: for testing, remove
# regression_coefs_interactions %>%
#   mutate(
#     is_interaction = if_else(str_detect(level, "Pandemic\\:"), TRUE, FALSE, missing = FALSE),
#     variable = if_else(
#       condition = is_interaction,
#       true =
#         str_extract(level, pattern = paste(regression_coefs_interactions$variable, collapse="|")),
#       false = variable,
#       missing = variable
#     ),
#     level = if_else(
#       condition = is_interaction,
#       true = str_remove(level, pattern = paste0("Pandemic\\:",variable)) %>% 
#         if_else(.=="", NA_character_, .),
#       false = level,
#       missing = level
#     ),
#     variable = factor(variable, levels = unique(variable))
#   ) %>%
#   arrange(variable, factor(level, levels = unique(level)), is_interaction) %>%
#   print_all
  

# combined regression and AME table ---------------------------------------

ame_m1 <- read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/part 3/tbl_ame_m1.csv")

ame_m1_processed <-
  ame_m1 %>%
  mutate(
    level = str_replace(contrast, "^mean\\((.*)\\) \\-.*$", replacement = "\\1"),
    level = if_else(level == "mean(dY/dX)", NA_character_, level)
    ) %>%
  transmute(
    variable=term, level,
    ame = scales::label_percent(accuracy=0.1)(estimate),
    ci_ame = paste0(scales::label_percent(accuracy=0.1)(conf.low),",",scales::label_percent(accuracy=0.1)(conf.high)),
    p_ame = scales::label_pvalue(accuracy = 0.0001)(p.value)
  )

tbl_m1_with_ame <-
  regression_coefs %>%
  transmute(
    variable, level, 
    aOR = estimate,
    ci = paste0(signif(conf.low,3),",",signif(conf.high,3)),
    p = scales::label_pvalue(accuracy = 0.0001)(p.value) 
  ) %>%
  full_join(
    ame_m1_processed, join_by(variable, level)
  ) %>%
  mutate(variable = replace_with_long_variable_name(variable)) %>%
  replace_na(list(level=" ")) %>%  # to make it palatable to excel
  group_by(variable) %>%
  mutate(variable = if_else(duplicated(variable), " ", variable)) %>%
  ungroup

write_csv(tbl_m1_with_ame, file = paste0(dir_output, "regression_coefs_and_ame_m1.csv"))
