## Plot time spent in A&E descriptives

## Plot mean days from discharge to death

library(tidyverse)
source("./FUNCTIONS.R")

## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv")

## note, these include non-users in the mean!
ae_los_descriptives <- read_csv(
  file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/ae_los_descriptives_by_pod.csv"
  ) %>%
  recalculate_mean_for_users_only(cohort_by_pod) %>%
  mutate(
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

(fig_ae_los_mean <-
    ae_los_descriptives %>%
    ggplot(
      aes(
        x = val_cohort_year,
        y = m_users,  # plot mean for users, excluding non-users
        colour = cat_place_of_death,
        shape = cat_place_of_death,
        group = cat_place_of_death
      )
    ) +
    geom_point(size = rel(3), position=position_dodge(width = 0.5)) +
    geom_line(position=position_dodge(width = 0.5)) +
    # adjusted error bars - mean for users, excl. non-users
    geom_errorbar(aes(ymin = ci_lo_users, ymax = ci_hi_users), position = position_dodge(width = 0.5)) +
    scale_colour_viridis_d() +
    labs(
      x = NULL,
      y = NULL,
      title = "Total hours spent in A&E per person"
    )
)

fig_ae_los_mean %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_ae_los_mean_by_pod.png",
    dpi = 300,
    width = 9,
    height = 6,
    bg = "white"
  )
