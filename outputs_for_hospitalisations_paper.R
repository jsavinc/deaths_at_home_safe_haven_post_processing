# script compiling tables & plots for the hospitalisations paper


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)

# constants ---------------------------------------------------------------

dir_output <- 
  "C:/Users/40011625/OneDrive - Edinburgh Napier University/SCADR/covid_mortality_home/drafts/hospitalisations paper/"

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

source("./FUNCTIONS.R")  # load common functions

# load data ---------------------------------------------------------------

cohort_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(n=sum(n), .groups = "drop")

# note: the mean hospitalisations includes non-hospitalised folks also
hosp_pod <- read_csv(
    file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/smr01_spells_descriptives_by_pod.csv"
  ) %>%
  left_join(cohort_pod %>% rename(n_cohort=n), join_by(val_cohort_year, cat_place_of_death)) %>%
  mutate(
    mean_users = m * n_cohort / n_users,
    mean_users_ci_lo = mean_users + qt(p = 0.025, df = n_users - 1) * sd /sqrt(n_users),
    mean_users_ci_hi = mean_users + qt(p = 0.975, df = n_users - 1) * sd /sqrt(n_users),
    val_cohort_year = factor(val_cohort_year),
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = mean_users, ci_lo_var = mean_users_ci_lo, n_var = n_users, new_sd_var = sd_users) %>%
  code_place_of_death_consistently

# note: here I assume that individuals with 15 or more spells had exactly 15 spells.
hosp_freq_pod <- read_csv(
  file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/smr01_spells_categorised_by_pod.csv"
) %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  code_place_of_death_consistently %>%
  mutate(
    amt_smr01_spells = as.integer(parse_number(cat_smr01_spells)),
    cat_smr01_spells = factor(cat_smr01_spells, levels = unique(cat_smr01_spells))
  )

hosp_freq_pod_recalculated_median_q90 <-
  hosp_freq_pod %>%
  select(cat_place_of_death, val_cohort_year, n, amt_smr01_spells) %>%
  filter(amt_smr01_spells != 0) %>%
  uncount(n) %>%
  group_by(cat_place_of_death, val_cohort_year) %>%
  summarise(
    median_users = median(amt_smr01_spells),
    iqr_users = IQR(amt_smr01_spells),
    q90_users = quantile(amt_smr01_spells, probs = 0.9),
    .groups = "drop"
  )

test_tbl_recalculated_median_q90 <-
  hosp_freq_pod_recalculated_median_q90 %>%
  left_join(hosp_pod %>% select(val_cohort_year, cat_place_of_death, median, q90), join_by(val_cohort_year, cat_place_of_death))

# note: the LOS here includes 0s so I compute the mean, sd, and 95%CI for hospital users only as well
los_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
  left_join(cohort_pod %>% rename(n_cohort=n), join_by(val_cohort_year, cat_place_of_death)) %>%
  mutate(
    mean_users = mean * n_cohort / n_admitted,
    mean_users_ci_lo = mean_users + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_users_ci_hi = mean_users + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = mean_users, ci_lo_var = mean_users_ci_lo, n_var = n_admitted, new_sd_var = sd_users) %>%
  code_place_of_death_consistently

dtdt_pod <- read_csv(
    file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/dtdt_smr01_descriptives_by_pod.csv"
  ) %>%
  code_place_of_death_consistently


# wrangle data ------------------------------------------------------------

total_hospitalisations_in_cohort <-
  hosp_pod %>%
  transmute(val_cohort_year, cat_place_of_death, n_hospitalisations = as.integer(n_users * mean_users)) %>%
  ({function(x) bind_rows(
    x,
    x %>% mutate(cat_place_of_death = factor("All", levels = levels(cat_place_of_death)))
  )}) %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(
    n_hospitalisations = sum(n_hospitalisations),
    .groups = "drop"
  ) %>%
  group_by(cat_place_of_death) %>%
  mutate(prop_annual_change = n_hospitalisations/lag(n_hospitalisations,n=1,default=NA_integer_)) %>%
  ungroup

# compile table -----------------------------------------------------------

# TODO: include the LOS means for service users only
# TODO: median and q90 not comparable here if only users are included...
tbl_hosp_descriptives_pod <-
  bind_rows(
    hosp_pod %>% 
      select(val_cohort_year, cat_place_of_death, m=mean_users, median, sd=sd_users, ci_lo=mean_users_ci_lo, ci_hi=mean_users_ci_hi, q90) %>% 
      mutate(outcome = "Number of hospitalisations"),
    los_pod %>% 
      select(val_cohort_year, cat_place_of_death, m=mean_users, sd=sd_users, q90, ci_lo = mean_users_ci_lo, ci_hi = mean_users_ci_hi) %>% 
      mutate(outcome = "Total LOS in hospital"),
    dtdt_pod %>% 
      select(val_cohort_year, cat_place_of_death, m, median, sd, ci_lo, ci_hi, q90) %>% 
      mutate(outcome = "Total LOS in hospital")
  ) %>% 
  relocate(outcome)


# compile plots -----------------------------------------------------------
(fig_attenders <-
   ggplot(
     hosp_pod,
     aes(
       x = val_cohort_year,
       y = n_users,
       shape = cat_place_of_death,
       group = cat_place_of_death,
       fill = cat_place_of_death
     )
   ) +
   geom_col(position = position_dodge(width=0.9)) +
   scale_fill_viridis_d(option = "D") +
   scale_y_continuous(limits = c(0,NA), breaks = scales::breaks_pretty(n=6), labels = scales::comma) +
   labs(x = NULL, y = str_wrap("Number of people who had one or more hospitalisations", width = 45))
)

(fig_hosp <-
   ggplot(
     hosp_pod,
     aes(
       x = val_cohort_year,
       y = mean_users,
       shape = cat_place_of_death,
       group = cat_place_of_death,
       colour = cat_place_of_death
     )
   ) +
   geom_point(size = rel(3)) +
   geom_line() +
   scale_colour_viridis_d(option = "D") +
   scale_y_continuous(limits = c(0,NA), breaks = scales::breaks_pretty(n=6)) +
   labs(x = NULL, y = "Mean hospitalisations")
)


(fig_los <-
    ggplot(
      los_pod,
      aes(
        x = val_cohort_year,
        y = mean_users,
        shape = cat_place_of_death,
        group = cat_place_of_death,
        colour = cat_place_of_death
      )
    ) +
    geom_point(size = rel(3)) +
    geom_line() +
    scale_colour_viridis_d(option = "D") +
    scale_y_continuous(limits = c(0,NA), breaks = scales::breaks_pretty(n=6)) +
    labs(x = NULL, y = "Mean total LOS")
)

(fig_dtdt <-
    ggplot(
      dtdt_pod,
      aes(
        x = val_cohort_year,
        y = m,
        shape = cat_place_of_death,
        group = cat_place_of_death,
        colour = cat_place_of_death
      )
    ) +
    geom_point(size = rel(3)) +
    geom_line() +
    scale_colour_viridis_d(option = "D") +
    scale_y_continuous(limits = c(0,NA), breaks = scales::breaks_pretty(n=6)) +
    labs(x = NULL, y = "Mean days between discharge & death")
)

fig_composite <-
  wrap_plots(
    fig_attenders,
    fig_hosp,
    fig_los,
    fig_dtdt,
    guides = "collect", ncol = 2
  ) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave(
  filename = paste0(dir_output, "fig_hospitalisations_composite.png"),
  plot = fig_composite,
  bg = "white",
  dpi = 300,
  width = 10,
  height = 10,
  units = "in"
)

# hospitalisation frequency plot ------------------------------------------

(fig_hosp_freq <-
  hosp_freq_pod %>%
  filter(cat_place_of_death=="Home") %>%
  ggplot(data=., aes(x = cat_smr01_spells, y = prop, group = val_cohort_year, fill = val_cohort_year)) +
  geom_col(position = position_dodge(), colour="grey10") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = scales::breaks_pretty(n = 7)) +
  labs(x = "Number of hospitalisations", y = "Frequency (%)", title = "Frequency of hospitalisations in home deaths")
)

ggsave(
  filename = paste0(dir_output, "fig_frequency_of_hospitalisations_home.png"),
  plot = fig_hosp_freq,
  bg = "white",
  dpi = 300,
  width = 7,
  height = 7/1.4,
  units = "in"
)
