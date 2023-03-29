## GPOOH visits plot

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R")

gpooh_home_visits <- 
  read_csv(
    file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/gpooh_home_visits.csv"
  ) %>%
  mutate(
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

(fig_gpooh_home_visits <-
    wrap_plots(
      ## all gpooh
      gpooh_home_visits %>%
        ggplot(aes(x = val_cohort_year, group = cat_place_of_death, colour = cat_place_of_death, shape = cat_place_of_death, y = n_all)) +
        geom_point(size = rel(3)) +
        geom_line() +
        scale_colour_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        labs(
          x = NULL, y = NULL, title = "GPOOH contacts"
        )
      ,
      ## home visits
      gpooh_home_visits %>%
        ggplot(aes(x = val_cohort_year, group = cat_place_of_death, colour = cat_place_of_death, shape = cat_place_of_death, y = n_home_visits)) +
        geom_point(size = rel(3)) +
        geom_line() +
        scale_colour_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        labs(
          x = NULL, y = NULL, title = "Home visits"
        )
      ,
      ## proportion home visits
      gpooh_home_visits %>%
        ggplot(aes(x = val_cohort_year, group = cat_place_of_death, colour = cat_place_of_death, shape = cat_place_of_death, y = prop_home_visits)) +
        geom_point(size = rel(3)) +
        geom_line() +
        scale_colour_viridis_d() +
        scale_y_continuous(labels = scales::percent) +
        labs(
          x = NULL, y = NULL, title = "Proportion of home visits"
        ), 
      guides = "collect"
    ) & theme(legend.position = "bottom")
)

fig_gpooh_home_visits %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_gpooh_home_visits.png",
    dpi = 300,
    width = 12,
    height = 6,
    bg = "white"
  )
