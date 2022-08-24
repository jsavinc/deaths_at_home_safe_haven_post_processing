# Plot data on hospital attendance from LOS descriptives

library(tidyverse)
library(patchwork)


# By cohort ---------------------------------------------------------------

los_cohort <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort.csv") %>%
  mutate(
    val_cohort_year = factor(val_cohort_year)
  )

fig_attenders_cohort_n <- los_cohort %>%
  ggplot(aes(x=val_cohort_year, y=n_admitted)) +
  geom_point() +
  geom_line(aes(group=1)) +
  scale_y_continuous(labels = ~format(.x, big.mark=",")) +
  labs(
    title = str_wrap("Number & proportion of people with an inpatient admission in the last 12 months of life", width = 60),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  ) +
  NULL

fig_attenders_cohort_prop <- los_cohort %>%
  ggplot(aes(x=val_cohort_year, y=prop_admitted)) +
  geom_point() +
  geom_line(aes(group=1)) +
  scale_y_continuous(labels = ~scales::percent(.x, accuracy=1), limits = c(0.73,0.8), breaks = seq(0,1,by=.01)) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  ) +
  NULL

fig_attenders <- fig_attenders_cohort_n + fig_attenders_cohort_prop

fig_attenders %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_hospital_attenders_cohort.png", width = 15, height = 10, units = "cm", dpi = 300)


# By cohort & place of death ----------------------------------------------

los_cohort_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death))
  )

fig_attenders_cohort_pod_n <-
  los_cohort_pod %>%
  ggplot(aes(x=val_cohort_year, y=n_admitted, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = ~format(.x, big.mark=",")) +
  labs(
    title = str_wrap("Number & proportion of people with an inpatient admission in the last 12 months of life", width = 60),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  NULL

fig_attenders_cohort_pod_prop <-
  los_cohort_pod %>%
  ggplot(aes(x=val_cohort_year, y=prop_admitted, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = ~scales::percent(.x, accuracy=1)) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  NULL

fig_attenders_cohort_pod <- 
  wrap_plots(
    fig_attenders_cohort_pod_n + fig_attenders_cohort_pod_prop,
    guides = "collect"
  ) &
  theme(legend.position = "top")
    

fig_attenders_cohort_pod %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_hospital_attenders_cohort_pod.png", width = 15, height = 10, units = "cm", dpi = 300)



# By cohort, SIMD ---------------------------------------------------------


los_cohort_simd <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_simd.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    val_simd_quintile = factor(as.integer(val_simd_quintile))
  )

fig_attenders_cohort_simd_n <-
  los_cohort_simd %>%
  ggplot(aes(x=val_cohort_year, y=n_admitted, colour = val_simd_quintile, group = val_simd_quintile, shape = val_simd_quintile)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = ~format(.x, big.mark=",")) +
  labs(
    title = str_wrap("Number & proportion of people with an inpatient admission in the last 12 months of life", width = 60),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  NULL

fig_attenders_cohort_simd_prop <-
  los_cohort_simd %>%
  ggplot(aes(x=val_cohort_year, y=prop_admitted, colour = val_simd_quintile, group = val_simd_quintile, shape = val_simd_quintile)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = ~scales::percent(.x, accuracy=1)) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  NULL

fig_attenders_cohort_simd <- 
  wrap_plots(
    fig_attenders_cohort_simd_n + fig_attenders_cohort_simd_prop,
    guides = "collect"
  ) &
  theme(legend.position = "top")


fig_attenders_cohort_simd %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_hospital_attenders_cohort_simd.png", width = 15, height = 10, units = "cm", dpi = 300)


# By cohort, place of death, and SIMD -------------------------------------


los_cohort_pod_simd <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod_simd.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death)),
    val_cohort_year = factor(val_cohort_year),
    val_simd_quintile = factor(as.integer(val_simd_quintile))
  )

# fig_attenders_cohort_pod_simd_n <-
#   los_cohort_pod_simd %>%
#   ggplot(aes(x=val_cohort_year, y=n_admitted, colour = cat_place_of_death, group = cat_place_of_death)) +
#   geom_line() +
#   geom_point() +
#   scale_y_continuous(labels = ~format(.x, big.mark=",")) +
#   labs(
#     title = str_wrap("Number & proportion of people with an inpatient admission in the last 12 months of life", width = 60),
#     x = NULL, y = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     legend.title = element_blank(), legend.position = "top",
#     axis.text.x = element_text(angle=60, hjust=1)
#   ) +
#   scale_colour_viridis_d() +
#   NULL

fig_attenders_cohort_pod_simd_prop <-
  los_cohort_pod_simd %>%
  ggplot(aes(x=val_cohort_year, y=prop_admitted, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = ~scales::percent(.x, accuracy=1)) +
  facet_wrap(~val_simd_quintile, nrow = 1) +
  labs(
    title = str_wrap("Proportion of people with an inpatient admission in the last 12 months of life", width = 60),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  scale_colour_viridis_d() +
  NULL
# 
# fig_attenders_cohort_pod <- 
#   wrap_plots(
#     fig_attenders_cohort_pod_n + fig_attenders_cohort_pod_prop,
#     guides = "collect"
#   ) &
#   theme(legend.position = "top")


fig_attenders_cohort_pod_simd_prop %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_hospital_attenders_cohort_pod_simd_prop.png", width = 15, height = 10, units = "cm", dpi = 300)
