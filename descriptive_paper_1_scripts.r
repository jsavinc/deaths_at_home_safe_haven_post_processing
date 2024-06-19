# deaths at home, descriptive paper 1 scripts




# constants ---------------------------------------------------------------

dir_output <- 
  "C:/Users/40011625/OneDrive - Edinburgh Napier University/SCADR/covid_mortality_home/drafts/descriptive paper 1/"

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

# shorten_to_home <- function(data_tbl) {
#   data_tbl %>%
#     mutate(
#       cat_place_of_death = 
#         fct_recode(cat_place_of_death, Home = "Home & non-institution")
#     )
# }

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
age_group_by_pod <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/age_by_pod.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  code_place_of_death_consistently

## this is aggregated by cause of death but there's also an 'all causes' category
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
  code_place_of_death_consistently

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


# recalculate frequencies from categorised --------------------------------

num_comorb_inferred_from_categories_by_pod <-
  num_comorb_categorised_by_pod %>%
  mutate(val_comorb = as.integer(parse_number(cat_comorb_count))) %>%
  select(cat_place_of_death, val_cohort_year, n, val_comorb) %>%
  uncount(n)

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


# demography plots illustrating proportions in table2 ---------------------

(fig_marstat_proportions <-
   marstat_by_pod %>%
   filter(cat_marital_status_condensed != "Missing") %>%
   ggplot(
     data = .,
     aes(x = val_cohort_year, y = prop, group = cat_marital_status_condensed, colour = cat_marital_status_condensed, shape = cat_marital_status_condensed)
   ) +
   geom_line() +
   geom_point(size = rel(3)) +
   facet_wrap(~cat_place_of_death) +
   scale_colour_viridis_d(direction = 1, option = "H") +
   # scale_y_continuous(labels = scales::comma, n.breaks = 6) +
   scale_y_continuous(labels = scales::label_percent(accuracy = 1), n.breaks = 6) +
   labs(x = NULL, y = NULL, title = "Marital status (%)") +
   theme(plot.title = element_text(hjust = 0.5)) +
   guides(fill=guide_legend(nrow=2,byrow=TRUE))  # two row legend
)

(fig_simd_proportions <- 
    simd_by_pod %>%
    filter(!is.na(val_simd_quintile)) %>%
    mutate(
      val_simd_quintile = if_else(val_simd_quintile==1, "1 (most deprived)", as.character(val_simd_quintile)),
      val_simd_quintile = factor(val_simd_quintile, levels = unique(val_simd_quintile))
    ) %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = prop, group = val_simd_quintile, colour = val_simd_quintile, shape = val_simd_quintile)
    ) +
    geom_line() +
    geom_point(size = rel(3)) +
    facet_wrap(~cat_place_of_death) +
    scale_colour_viridis_d(direction = 1, option = "H") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1), n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "Deprivation (%)") +
    theme(plot.title = element_text(hjust = 0.5))
)


# demography plots --------------------------------------------------------

(
fig_age_group_5 <- 
  age_group_by_pod %>%
  ggplot(
    data = .,
    aes(x = val_cohort_year, y = n, group = cat_age_5, fill = cat_age_5)
  ) +
  geom_col(position = position_stack(), colour = "grey10", linewidth = 0.1) +
  facet_wrap(~cat_place_of_death) +
  scale_fill_viridis_d(direction = -1) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  labs(x = NULL, y = NULL, title = "Age group (N)") +
  theme(plot.title = element_text(hjust = 0.5))
)

(fig_sex <-
    sex_by_pod %>%
    mutate(n_male = (n/prop)-n) %>%
    rename(n_female = n) %>%
    pivot_longer(cols = c(n_female,n_male), values_to = "n", names_to = "sex", names_transform = ~str_remove(.x, "n\\_")) %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = n, group = sex, fill = sex)
    ) +
    geom_col(position = position_dodge(width = 0.8, preserve = "total"), colour = "grey10", linewidth = 0.1) +
    facet_wrap(~cat_place_of_death) +
    scale_fill_viridis_d(direction = -1) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "Sex (N)") +
    theme(plot.title = element_text(hjust = 0.5))
    )

(
  fig_marstat <- 
    marstat_by_pod %>%
    mutate(cat_marital_status_condensed = fct_rev(cat_marital_status_condensed)) %>%
    filter(cat_marital_status_condensed != "Missing") %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = n, group = cat_marital_status_condensed, fill = cat_marital_status_condensed)
    ) +
    geom_col(position = position_stack(), colour = "grey10", linewidth = 0.1) +
    facet_wrap(~cat_place_of_death) +
    scale_fill_viridis_d(direction = 1) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "Marital status (N)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))  # two row legend
)

(
  fig_ethnicity <- 
    ethnicity_by_pod %>%
    mutate(cat_ethnic_group_collapsed = fct_relevel(cat_ethnic_group_collapsed, "White")) %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = n, group = cat_ethnic_group_collapsed, fill = cat_ethnic_group_collapsed)
    ) +
    geom_col(position = position_stack(), colour = "grey10", linewidth = 0.1) +
    facet_wrap(~cat_place_of_death) +
    scale_fill_viridis_d(direction = -1) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "Ethnicity (N)") +
    theme(plot.title = element_text(hjust = 0.5))
)


# geography plots ---------------------------------------------------------

(fig_simd <- 
    simd_by_pod %>%
    filter(!is.na(val_simd_quintile)) %>%
    mutate(
      val_simd_quintile = if_else(val_simd_quintile==1, "1 (most deprived)", as.character(val_simd_quintile)),
      val_simd_quintile = factor(val_simd_quintile, levels = unique(val_simd_quintile))
      ) %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = n, group = val_simd_quintile, fill = val_simd_quintile)
    ) +
    geom_col(position = position_dodge(width = 0.8, preserve = "total"), colour = "grey10", linewidth = 0.1) +
    facet_wrap(~cat_place_of_death) +
    scale_fill_viridis_d(direction = 1) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "Deprivation (N)") +
    theme(plot.title = element_text(hjust = 0.5))
)

(fig_ur2 <-
    ur2_by_pod %>%
    drop_na(cat_ur2) %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = n, group = cat_ur2, fill = cat_ur2)
    ) +
    geom_col(position = position_stack(), colour = "grey10", linewidth = 0.1) +
    facet_wrap(~cat_place_of_death) +
    scale_fill_viridis_d(direction = -1) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "Urban-rural location (N)") +
    theme(plot.title = element_text(hjust = 0.5))
)


# clinical / palliative characteristics plots -----------------------------
# TODO: unify the order of factors & colour palette
# TODO: grey dashed line for average across place of death?

(fig_num_cod <-
   num_cod_by_pod %>%
   ggplot(
     aes(
       x = val_cohort_year,
       y = m,
       ymin = ci_lo,
       ymax = ci_hi,
       colour = cat_place_of_death,
       shape = cat_place_of_death,
       group = cat_place_of_death,
       linetype = cat_place_of_death
     )
   ) +
   geom_line() +
   geom_errorbar(width = 0.2) + 
   geom_point(size = rel(2)) +
   scale_colour_viridis_d(option = "H", direction = 1) +
   scale_y_continuous(labels = scales::comma, n.breaks = 6) +
   scale_linetype_manual(values = c(rep("solid", 3), "dashed")) +
   labs(x = NULL, y = NULL, title = "Recorded causes of death (mean)") +
   theme(plot.title = element_text(hjust = 0.5))
)

(fig_comorb_index <-
   comorb_by_pod %>%
   filter(measure == "Elixhauser comorbidity index") %>%
   ggplot(
     aes(
       x = val_cohort_year,
       y = m,
       ymin = ci_lo,
       ymax = ci_hi,
       colour = cat_place_of_death,
       shape = cat_place_of_death,
       group = cat_place_of_death,
       linetype = cat_place_of_death
     )
   ) +
   geom_line() +
   geom_errorbar(width = 0.2) + 
   geom_point(size = rel(2)) +
    scale_colour_viridis_d(option = "H", direction = 1) +
   scale_y_continuous(labels = scales::comma, n.breaks = 6) +
   scale_linetype_manual(values = c(rep("solid", 3), "dashed")) +
   labs(x = NULL, y = NULL, title = "Elixhauser comorbidity index (mean)") +
   theme(plot.title = element_text(hjust = 0.5))
)

(fig_num_comorb <-
   comorb_by_pod %>%
   filter(measure == "Number of comorbidities incl. in the Elixhauser index") %>%
   ggplot(
     aes(
       x = val_cohort_year,
       y = m,
       ymin = ci_lo,
       ymax = ci_hi,
       colour = cat_place_of_death,
       shape = cat_place_of_death,
       group = cat_place_of_death,
       linetype = cat_place_of_death
     )
   ) +
   geom_line() +
   geom_errorbar(width = 0.2) + 
   geom_point(size = rel(2)) +
    scale_colour_viridis_d(option = "H", direction = 1) +
   scale_y_continuous(labels = scales::comma, n.breaks = 6) +
   scale_linetype_manual(values = c(rep("solid", 3), "dashed")) +
   labs(x = NULL, y = NULL, title = "Comorbidities (mean)") +
   theme(plot.title = element_text(hjust = 0.5))
)

(fig_palliative_care_needs <- 
    pall_care_needs_by_pod %>%
    filter(cat_place_of_death!="All") %>%
    ggplot(
      data = .,
      aes(x = val_cohort_year, y = n, group = cat_place_of_death, fill = cat_place_of_death), 
    ) +
    geom_col(position = position_dodge(width = 0.9, preserve = "total"), colour = "grey10", linewidth = 0.1, show.legend = FALSE) +
    scale_fill_viridis_d(option = "H", direction = 1, drop = FALSE) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6) +
    labs(x = NULL, y = NULL, title = "People with palliative care needs (N)") +
    theme(plot.title = element_text(hjust = 0.5))
)


# (
#   fig_comorb_distribution <- 
#     num_comorb_inferred_from_categories_by_pod %>%
#     mutate(cat_comorb = fct_lump_lowfreq(factor(val_comorb), other_level = "9+")) %>%
#     ggplot(
#       data = .,
#       aes(x = cat_comorb, fill = cat_comorb, group = cat_comorb)
#     ) +
#     geom_bar(colour = "grey10") +
#     facet_grid(cat_place_of_death ~ val_cohort_year) +
#     scale_fill_viridis_d(direction = -1) +
#     scale_y_continuous(labels = scales::comma, n.breaks = 6) +
#     labs(x = NULL, y = NULL) 
# )


# compose plots -----------------------------------------------------------


(fig_demography_1 <- 
    wrap_plots(
      fig_age_group_5,
      fig_sex, 
      ncol = 1
    ) +
   plot_annotation(tag_levels = "a")
)

(fig_demography_2 <- 
   wrap_plots(
     # fig_marstat,
     fig_marstat_proportions,  # replaced with line graph, more informative
     fig_ethnicity, 
     ncol = 1
   ) +
    plot_annotation(tag_levels = "a")
)

(fig_geography <- 
    wrap_plots(
      # fig_simd,  # replaced with line graph, better view of distributions
      fig_simd_proportions,
      fig_ur2, 
      ncol = 1
    ) +
    plot_annotation(tag_levels = "a")
)

(fig_clinical <-
    wrap_plots(
      fig_palliative_care_needs,
      fig_num_cod,
      fig_comorb_index,
      fig_num_comorb,
      ncol = 2, guides = "collect"
    ) +
    plot_annotation(tag_levels = "a")
  )


# save plots --------------------------------------------------------------

# fig_width_inches = 8
# fig_height_inches = 12
fig_width_cm = 14
fig_height_cm = 19

save_plot <- 
  function(plot, filename, width = fig_width_cm, height = fig_height_cm) {
    walk(
      .x = c(".png", ".emf", ".svg"),
      .f = function(extension) {
        ggsave(
          plot = plot,
          filename = paste0(dir_output, filename, extension),
          width = width,
          height = height,
          units = "cm",
          dpi = 300,
          bg = "white"
        )
      }
    )
  }


save_plot(
  plot = fig_demography_1,
  filename = "fig1_age_sex"
)

save_plot(
  plot = fig_demography_2,
  filename = "fig2_marstat_ethnicity"
)

save_plot(
  plot = fig_geography,
  filename = "fig3_simd_ur2"
)


save_plot(
  plot = fig_clinical,
  filename = "fig4_clinical",
  width = 19,
  height = 19
)


# Same plots without error bars -------------------------------------------

## Only the "clinical" plots use error bars - I use {ggedit} to `remove_geom()`

fig_num_cod_no_errorbars <-
  fig_num_cod %>%
  remove_geom(geom = "errorbar")

fig_comorb_index_no_errorbar <-
  fig_comorb_index %>%
  remove_geom(geom = "errorbar")

fig_num_comorb_no_errorbar <-
  fig_num_comorb %>%
  remove_geom(geom = "errorbar")

(fig_clinical_no_errorbars <-
   wrap_plots(
     fig_palliative_care_needs,
     fig_num_cod_no_errorbars,
     fig_comorb_index_no_errorbar,
     fig_num_comorb_no_errorbar,
     ncol = 2, guides = "collect"
   ) +
   plot_annotation(tag_levels = "a")
)

save_plot(
  plot = fig_clinical_no_errorbars,
  filename = "fig4_clinical_no_errorbars",
  width = 19,
  height = 19
)
