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


