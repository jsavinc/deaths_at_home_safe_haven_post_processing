## Computing high-level overviews of service use - person-days in hospital and
## A&E, total number of hospital spells, totals of A&E admissions, OOH GP
## contacts, NHS24 calls, ambulance uses - all by cohort & PoD

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R", local = TRUE)  # load helper functions

# load data ---------------------------------------------------------------

## load cohort size by PoD - used to recalculate some of the usage below
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  )

## hospital LOS
hospital_los_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
  rename_all(~gsub(pattern = "admitted", replacement = "users", x = .x)) %>%
  rename(m_users=mean, sd_users = sd) %>%
  # TODO: implement this recalculation for m, sd, and cis; i guess median & quantiles won't work
  # repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  # group_by(val_cohort_year, cat_place_of_death) %>%
  # summarise(
  #   mean = weighted.mean(mean, w = n_users),
  #   sd = sqrt(sum((n_users-1) * sd^2) / sum(n_users-1)),
  #   n_users = sum(n_users),
  #   .groups = "drop"
  # ) %>%
  left_join(cohort_by_pod %>% rename(n_cohort = n), by = c("val_cohort_year","cat_place_of_death")) %>%
  mutate(
    ci_lo = m_users + qt(p = 0.025, df = n_users - 1) * sd_users /sqrt(n_users),
    ci_hi = m_users + qt(p = 0.975, df = n_users - 1) * sd_users /sqrt(n_users),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  ) %>%
  mutate(
    amt_person_days = n_users * m_users,
  )

## hospital admissions
# hospital_spells_pod <-
#   read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_underlying_mean_spells.csv") %>%
#   filter(cat_cod_nrs == "All causes") %>%
#   select(-cat_cod_nrs) %>%
#   pivot_annual_to_long() %>%
#   rename(val_cohort_year = cohort_year) %>%
#   parse_m_ci(col_m_ci = n_prop) %>%
#   rename(m_users = m, ci_lo_users = ci_lo, ci_hi_users = ci_hi) %>%
#   left_join(hospital_los_pod %>% select(val_cohort_year, cat_place_of_death, n_users, n_cohort), by = c("val_cohort_year","cat_place_of_death")) %>%
#   mutate(
#     amt_person_spells = n_users * m_users,
#     val_cohort_year = factor(val_cohort_year),
#     cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
#   )
hospital_spells_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/smr01_spells_descriptives_by_pod.csv") %>%
  recalculate_mean_for_users_only(cohort_by_pod) %>%
  mutate(
    amt_person_spells = n_users * m_users,
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

## A&E LOS - this includes non-users in the mean
ae_los_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/ae_los_descriptives_by_pod.csv") %>%
  recalculate_mean_for_users_only(cohort_by_pod) %>%
  mutate(
    amt_person_hours = n_users * m_users,
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

## A&E admissions - these were exported as part of the service use by CoD export
## Note that only users were included in the computations here, and that SD was
## not included in the export, so I'm recalculating it from the confidence
## intervals
ae_admissions_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_underlying_mean_ae_admissions.csv") %>%
  filter(cat_cod_nrs == "All causes") %>%
  select(-cat_cod_nrs) %>%
  pivot_annual_to_long() %>%
  rename(val_cohort_year = cohort_year) %>%
  parse_m_ci(col_m_ci = n_prop) %>%
  rename(m_users = m, ci_lo_users = ci_lo, ci_hi_users = ci_hi) %>%
  left_join(ae_los_pod %>% select(val_cohort_year, cat_place_of_death, n_users, n_cohort), by = c("val_cohort_year","cat_place_of_death")) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = m_users, ci_lo_var = ci_lo_users, n_var = n_users, new_sd_var = sd_users) %>%
  mutate(
    amt_person_admissions = n_users * m_users,
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

## ambulance - mean here includes non-users
sas_uses_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/sas_use_descriptives_by_pod.csv") %>%
  recalculate_mean_for_users_only(cohort_by_pod) %>%
  mutate(
    amt_person_uses = n_users * m_users,
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

## nhs24 calls - mean here includes non-users
nhs24_calls_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/nhs24_calls_descriptives_by_pod.csv") %>%
  recalculate_mean_for_users_only(cohort_by_pod) %>%
  mutate(
    amt_person_calls = n_users * m_users,
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

## OOHGP contacts - mean here includes non-users
gpooh_contacts_pod <-
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/gpooh_visits_descriptives_by_pod.csv") %>%
  recalculate_mean_for_users_only(cohort_by_pod) %>%
  mutate(
    amt_person_contacts = n_users * m_users,
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )


# combine service use data ------------------------------------------------

combined_service_uses <- pmap_dfr(
  .l = 
    ## assemble all the individual measures
    tribble(
      ~df, ~name,
      hospital_spells_pod, "Hospital spells",
      hospital_los_pod, "Hospital person-days",
      ae_admissions_pod, "A&E admissions",
      ae_los_pod, "A&E hours",
      gpooh_contacts_pod, "GPOOH contacts",
      sas_uses_pod, "Ambulance uses",
      nhs24_calls_pod, "NHS24 calls"
    ) %>% mutate(name = factor(name, levels = unique(name))),
  .f = function(df, name) {
    df %>% 
      select(val_cohort_year, cat_place_of_death, matches("amt_person"), m_users, sd_users, n_users) %>%
      rename(amt_service_total = 3) %>%  # rename 3rd variable to be the service total
      mutate(cat_measure = name)
  }
) %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death, cat_measure) %>%
  summarise(
    m_users = weighted.mean(m_users, w = n_users),
    ## pooled SD = sqrt of weighted mean of variances, weights = n-1
    sd_users = sqrt(weighted.mean(sd_users^2, w = n_users-1)),
    n_users = sum(n_users),
    amt_service_total = sum(amt_service_total),
    .groups = "drop"
  )

# tables ------------------------------------------------------------------

## table of service use summaries by cohort
combined_service_uses %>%
  write_csv("X:/R2090/2021-0312 Deaths at home/outputs/service_users/tbl_service_use_summaries.csv")

## table showing percentage change during pandemic
table_percentage_change_service_use <-
  combined_service_uses %>%
  filter(val_cohort_year %in% levels(val_cohort_year)[5:6]) %>%
  pivot_longer(cols = m_users:amt_service_total, names_to = "measure", values_to = "statistic") %>%
  pivot_wider(names_from = val_cohort_year, values_from = statistic) %>%
  mutate(percent_change = scales::label_percent(accuracy = 0.1)(`2020-21`/`2019-20` - 1))

table_percentage_change_service_use %>%
  write_csv("X:/R2090/2021-0312 Deaths at home/outputs/service_users/tbl_service_use_percent_changes_during_pandemic.csv")

# plots -------------------------------------------------------------------

## plot combining all service total usign facet_wrap




(fig_service_totals <-
  combined_service_uses %>%
    filter(cat_place_of_death!="All") %>%
    ggplot(aes(x = val_cohort_year, y = amt_service_total, shape = cat_place_of_death, colour = cat_place_of_death, group = cat_place_of_death)) +
    geom_line() +
    geom_point(size = rel(3), alpha = 0.8) +
    facet_wrap(~cat_measure, scales = "free_y", nrow = 2) +
    scale_colour_viridis_d() +
    # scale_y_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    labs(x = NULL, y = NULL, title = "Service use totals") +
    theme(plot.title = element_text(hjust = 0.5))
)

fig_service_totals %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_service_totals.png",
    dpi = 300,
    width = 11,
    height = 6,
    bg = "white"
  )

(fig_service_means <-
  combined_service_uses %>%
    filter(cat_place_of_death!="All") %>%
    ggplot(
      aes(
        x = val_cohort_year,
        y = m_users,
        shape = cat_place_of_death,
        colour = cat_place_of_death,
        group = cat_place_of_death
      )
    ) +
    geom_line() +
    geom_point(size = rel(3), alpha = 0.8) +
    facet_wrap(~cat_measure, scales = "free_y", nrow = 2) +
    scale_colour_viridis_d() +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    labs(x = NULL, y = NULL, title = "Service use averages per person") +
    theme(plot.title = element_text(hjust = 0.5))
)

fig_service_means %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_service_means.png",
    dpi = 300,
    width = 11,
    height = 6,
    bg = "white"
  )
