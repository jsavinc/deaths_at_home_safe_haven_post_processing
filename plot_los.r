# Plot data LOS from descriptives


library(tidyverse)
library(patchwork)


# By cohort ---------------------------------------------------------------

los_cohort <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort.csv") %>%
  mutate(
    val_cohort_year = factor(val_cohort_year),
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted)
  )

fig_los_cohort <-
  los_cohort %>%
  pivot_longer(cols = c(mean, median), names_to = "statistic", values_to = "value") %>%
  ggplot(aes(x=val_cohort_year, y = value, group = statistic)) +
  geom_line(aes(linetype = statistic)) +
  geom_point() +
  geom_errorbar(data = . %>% filter(statistic=="mean"), aes(ymin = mean_ci_lo, ymax = mean_ci_hi), width = 0.3) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Length of stay by cohort year",
    caption = "Error bars represent 95% CI of mean (t-distribution)."
  ) +
  NULL


fig_los_cohort %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_los_cohort.png", width = 12, height = 7, units = "cm", dpi = 300)


# By cohort & place of death ----------------------------------------------


los_cohort_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death))
  )


fig_los_cohort_pod <-
  los_cohort_pod %>%
  pivot_longer(cols = c(mean, median), names_to = "statistic", values_to = "value") %>%
  ggplot(aes(x=val_cohort_year, y = value, colour = cat_place_of_death, group = interaction(statistic, cat_place_of_death))) +
  geom_line(aes(linetype = statistic)) +
  geom_point() +
  geom_errorbar(data = . %>% filter(statistic=="mean"), aes(ymin = mean_ci_lo, ymax = mean_ci_hi), width = 0.3) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  labs(
    x = NULL, y = NULL,
    title = "Length of stay by cohort year & place of death",
    caption = "Error bars represent 95% CI of mean (t-distribution)."
  ) +
  NULL


fig_los_cohort_pod %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_los_cohort_pod.png", width = 12, height = 7, units = "cm", dpi = 300)



# By cohort & SIMD --------------------------------------------------------


los_cohort_simd <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_simd.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    val_simd_quintile = factor(as.integer(val_simd_quintile))
  )


fig_los_cohort_simd <-
  los_cohort_simd %>%
  ggplot(aes(x=val_cohort_year, y = mean, colour = val_simd_quintile, group = val_simd_quintile)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_ci_lo, ymax = mean_ci_hi), width = 0.3) +
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
  NULL


fig_los_cohort_pod %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_los_cohort_pod.png", width = 12, height = 7, units = "cm", dpi = 300)



# By cohort, place of death and SIMD --------------------------------------


los_cohort_pod_simd <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod_simd.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death)),
    val_cohort_year = factor(val_cohort_year),
    val_simd_quintile = factor(as.integer(val_simd_quintile))
  )



fig_los_cohort_pod_simd <-
  los_cohort_pod_simd %>%
  ggplot(aes(x=val_cohort_year, y = mean, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_ci_lo, ymax = mean_ci_hi), width = 0.3) +
  facet_wrap(~val_simd_quintile, nrow = 1, labeller = labeller(.cols = ~if_else(.x==1, "1=most deprived", as.character(.x)))) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  labs(
    x = NULL, y = NULL,
    title = "Length of stay by cohort year, place of death & SIMD quintile",
    caption = "Error bars represent 95% CI of mean (t-distribution)."
  ) +
  NULL


fig_los_cohort_pod_simd %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_los_cohort_pod_simd.png", width = 24, height = 12, units = "cm", dpi = 300)
