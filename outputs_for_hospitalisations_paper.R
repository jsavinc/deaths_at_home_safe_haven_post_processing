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

hosp_pod <- read_csv(
    file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/smr01_spells_descriptives_by_pod.csv"
  ) %>%
  code_place_of_death_consistently

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

# compile table -----------------------------------------------------------

# TODO: include the LOS means for service users only
# TODO: median and q90 not comparable here if only users are included...
tbl_hosp_descriptives_pod <-
  bind_rows(
    hosp_pod %>% 
      select(val_cohort_year, cat_place_of_death, m, median, sd, ci_lo, ci_hi, q90) %>% 
      mutate(outcome = "Number of hospitalisations"),
    los_pod %>% 
      select(val_cohort_year, cat_place_of_death, m=mean, sd, q90, ci_lo = mean_ci_lo, ci_hi = mean_ci_hi) %>% 
      mutate(outcome = "Total LOS in hospital"),
    dtdt_pod %>% 
      select(val_cohort_year, cat_place_of_death, m, median, sd, ci_lo, ci_hi, q90) %>% 
      mutate(outcome = "Total LOS in hospital")
  ) %>% 
  relocate(outcome)


# compile plots -----------------------------------------------------------

(fig_hosp <-
   ggplot(
     hosp_pod,
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
    fig_hosp,
    fig_los,
    fig_dtdt
  )

# TODO: wrap_plots()