# Plot mean age from descriptive tables

library(tidyverse)
library(patchwork)


# Age by cohort -----------------------------------------------------------

age_cohort <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_age_cohort.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_deaths - 1) * sd /sqrt(n_deaths),
    mean_ci_hi = mean + qt(p = 0.975, df = n_deaths - 1) * sd /sqrt(n_deaths),
    val_cohort_year = factor(val_cohort_year)
  )

fig_age_cohort <-
  age_cohort %>%
  pivot_longer(cols = c(mean, median), names_to = "statistic", values_to = "value") %>%
  ggplot(., aes(x = val_cohort_year, y = value, group = statistic, linetype = statistic)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_ci_lo, ymax = mean_ci_hi), width = 0.2) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  labs(
    x=NULL, y=NULL, title = "Age by cohort year"
  ) +
  NULL

fig_age_cohort %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_mean_age_by_cohort.png", width = 12, height = 7, units = "cm", dpi = 300)


# Age by cohort & place of death ------------------------------------------

age_cohort_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_age_cohort_pod.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_deaths - 1) * sd /sqrt(n_deaths),
    mean_ci_hi = mean + qt(p = 0.975, df = n_deaths - 1) * sd /sqrt(n_deaths),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death))
    )

fig_age_cohort_pod <-
  ggplot(age_cohort_pod, aes(x = val_cohort_year, y = mean, colour = cat_place_of_death, group = cat_place_of_death, shape = cat_place_of_death)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_ci_lo, ymax = mean_ci_hi), width = 0.2) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = ~format(.x, big.mark = ",")) +
  labs(
    x=NULL, y=NULL, title = "Age by cohort year & place of death"
  ) +
  NULL

fig_age_cohort_pod %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_mean_age_by_cohort_pod.png", width = 12, height = 7, units = "cm", dpi = 300)
