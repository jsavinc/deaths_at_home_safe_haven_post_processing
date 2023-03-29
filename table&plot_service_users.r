## Table of N & proportion of service users by service; The number of service
## uses is split across multiple safe haven outputs, and doesn't include the
## proportion necessarily. I'll get the cohort by PoD to compute proportions,
## and load the various sources for the number of service users

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R")

## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv")

## load & format the service users from various sources
service_users_raw <- bind_rows(
  ## hospital
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
    select(cat_place_of_death, val_cohort_year, n_users = n_admitted, prop_users = prop_admitted) %>%
    mutate(cat_service = "Hospital"),
  ## A&E
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/ae_los_descriptives_by_pod.csv") %>%
    select(cat_place_of_death, val_cohort_year, n_users) %>%
    mutate(cat_service = "A&E"),
  ## SAS
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/sas_use_descriptives_by_pod.csv") %>%
    select(cat_place_of_death, val_cohort_year, n_users) %>%
    mutate(cat_service = "Ambulance"),
  ## GPOOH
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/gpooh_visits_descriptives_by_pod.csv") %>%
    select(cat_place_of_death, val_cohort_year, n_users) %>%
    mutate(cat_service = "GPOOH"),
  ## NHS24
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/nhs24_calls_descriptives_by_pod.csv") %>%
  select(cat_place_of_death, val_cohort_year, n_users) %>%
    mutate(cat_service = "NHS24"),
)

service_users <- 
  service_users_raw %>%
  left_join(cohort_by_pod %>% rename(denominator = n), by = c("cat_place_of_death", "val_cohort_year")) %>%
  mutate(
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death),
    cat_service = factor(cat_service, levels = unique(cat_service)),
  ) %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(cat_service,val_cohort_year, cat_place_of_death) %>%
  summarise(
    n_users = sum(n_users), denominator = sum(denominator),
    prop_users = n_users / denominator,
    .groups = "drop"
    ) %>%
  relocate(cat_service, val_cohort_year, cat_place_of_death)

## write long version of table
write_csv(service_users, file = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/service_users_long.csv")

## write wide version of table
service_users %>%
  mutate(n_prop = calculate_n_prop(n = n_users, prop = prop_users, accuracy = 0.1)) %>%
  select(-c(denominator, n_users, prop_users)) %>%
  pivot_wider(names_from = val_cohort_year, values_from = n_prop) %>%
  write_csv(., file = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/service_users_wide.csv")

## plot graph of service users
(fig_service_users <-
  wrap_plots(
    ## N
    ggplot(
      service_users %>% filter(cat_place_of_death!="All"),
      aes(
        x = val_cohort_year, y = n_users,
        group = cat_service,
        colour = cat_service,
        shape = cat_service
      )
    ) +
      geom_point(size = rel(3)) +
      geom_line() +
      facet_wrap(~cat_place_of_death) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = NULL, y = NULL, title = "Number of service users")
    ,
    ## Proportion
    ggplot(
      service_users %>% filter(cat_place_of_death!="All"),
      aes(
        x = val_cohort_year, y = prop_users,
        group = cat_service,
        colour = cat_service,
        shape = cat_service
      )
    ) +
      geom_point(size = rel(3)) +
      geom_line() +
      facet_wrap(~cat_place_of_death) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(x = NULL, y = NULL, title = "Proportion of service users")
  , guides = "collect"
  )
)

(fig_service_users_all_pod <-
    wrap_plots(
      ## N
      ggplot(
        service_users %>% filter(cat_place_of_death=="All"),
        aes(
          x = val_cohort_year, y = n_users,
          group = cat_service,
          colour = cat_service,
          shape = cat_service
        )
      ) +
        geom_point(size = rel(3)) +
        geom_line() +
        scale_colour_viridis_d() +
        scale_y_continuous(labels = scales::comma) +
        labs(x = NULL, y = NULL, title = "Number of service users")
      ,
      ## Proportion
      ggplot(
        service_users %>% filter(cat_place_of_death=="All"),
        aes(
          x = val_cohort_year, y = prop_users,
          group = cat_service,
          colour = cat_service,
          shape = cat_service
        )
      ) +
        geom_point(size = rel(3)) +
        geom_line() +
        scale_colour_viridis_d() +
        scale_y_continuous(labels = scales::label_percent()) +
        labs(x = NULL, y = NULL, title = "Proportion of service users")
      , guides = "collect"
    ) &
    plot_annotation(title = "All places of death")
)

fig_service_users %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_service_users.png",
    dpi = 300,
    width = 12,
    height = 6,
    bg = "white"
    )


fig_service_users_all_pod %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_service_users_all_pod_combined.png",
    dpi = 300,
    width = 9,
    height = 6,
    bg = "white"
  )
