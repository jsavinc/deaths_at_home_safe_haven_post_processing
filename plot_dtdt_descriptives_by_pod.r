## Plot mean days from discharge to death

library(tidyverse)
source("./FUNCTIONS.R")

dtdt_descriptives <- read_csv(
  file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/dtdt_smr01_descriptives_by_pod.csv"
) %>%
  mutate(
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

(fig_dtdt_means <-
  dtdt_descriptives %>%
  filter(cat_place_of_death != "Hospital") %>%
  ggplot(
    aes(
      x = val_cohort_year,
      y = m,
      colour = cat_place_of_death,
      shape = cat_place_of_death,
      group = cat_place_of_death
    )
  ) +
  geom_point(size = rel(3)) +
  geom_line() +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi)) +
  scale_shape_manual(values = scales::shape_pal()(3)[c(2,3)]) +
  scale_colour_manual(values = scales::viridis_pal()(3)[c(2,3)]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Days from discharge to death",
    subtitle = "Higher number = earlier discharge"
  )
)

fig_dtdt_means %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_dtdt_smr01_means.png",
    dpi = 300,
    width = 6,
    height = 6,
    bg = "white"
  )
