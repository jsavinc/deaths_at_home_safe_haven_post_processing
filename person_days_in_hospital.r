# Compute and plot person-days in hospital

library(tidyverse)
library(patchwork)

# By cohort ---------------------------------------------------------------

los_cohort <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort.csv") %>%
  mutate(
    val_cohort_year = factor(val_cohort_year),
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted)
  ) %>%
  mutate(
    val_person_days = n_admitted * mean,
    )

los_cohort %>%
  ggplot(aes(x = val_cohort_year, y = val_person_days)) +
  geom_point()

# By cohort & place of death ----------------------------------------------

los_cohort_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death))
  ) %>%
  mutate(
    val_person_days = n_admitted * mean,
  )

los_cohort_pod %>%
  ggplot(aes(x = val_cohort_year, y = val_person_days, group = cat_place_of_death, colour = cat_place_of_death)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  labs(
    x = NULL, y = NULL,
    title = "Person-hospital days by cohort year & place of death",
    caption = "Error bars represent 95% CI of mean (t-distribution)."
  ) +
  NULL


# By cohort & SIMD --------------------------------------------------------

los_cohort_simd <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_simd.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    val_simd_quintile = factor(as.integer(val_simd_quintile))
  ) %>%
  mutate(
    val_person_days = n_admitted * mean,
  )

los_cohort_simd %>%
  # filter(val_simd_quintile %in% c(1, 5)) %>%
  ggplot(aes(x=val_cohort_year, y = val_person_days, colour = val_simd_quintile, group = val_simd_quintile)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  labs(
    x = NULL, y = NULL,
    title = "Length of stay by cohort year & SIMD",
    caption = "Error bars represent 95% CI of mean (t-distribution)."
  ) +
  # facet_wrap(~val_simd_quintile) +
  NULL


# By cohort, place of death and SIMD --------------------------------------

los_cohort_pod_simd <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod_simd.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death)),
    val_cohort_year = factor(val_cohort_year),
    val_simd_quintile = factor(as.integer(val_simd_quintile))
  ) %>%
  mutate(
    val_person_days = n_admitted * mean,
  )

# fig_los_cohort_pod_simd <-
  los_cohort_pod_simd %>%
  ggplot(aes(x=val_cohort_year, y = val_person_days, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  facet_wrap(~val_simd_quintile, nrow = 1, labeller = labeller(.cols = ~if_else(.x==1, "1=most deprived", as.character(.x)))) +
  theme_minimal() +
  scale_y_continuous(labels = ~format(.x, big.mark = ",", scientific = FALSE)) +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  labs(
    x = NULL, y = NULL,
    title = "Person-hospital days by cohort year, place of death & SIMD quintile",
    # subtitle = "Person-days = N people × mean LOS",
    subtitle = expression(paste("Person-days = ", N[people], " × mean LOS", sep = "")),
    # subtitle = "Person-days = N\\_{people} × mean LOS",
    caption = "Error bars represent 95% CI of mean (t-distribution)."
  ) +
  NULL
