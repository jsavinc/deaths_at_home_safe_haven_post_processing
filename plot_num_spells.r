## Plot mean days from discharge to death

library(tidyverse)
source("./FUNCTIONS.R")

num_spells_descriptives <- read_csv(
  file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_cod_underlying_mean_spells.csv"
  ) %>%
  mutate(
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  ) %>%
  pivot_annual_to_long(new_col = "mean_ci") %>%
  parse_m_ci(col_m_ci = "mean_ci")

(fig_num_spells_mean <-
    num_spells_descriptives %>%
    filter(cat_cod_nrs == "All causes") %>%
    mutate(cat_place_of_death = fct_relevel(cat_place_of_death, levels(cat_place_of_death)[2])) %>%
    ggplot(
      aes(
        # x = cohort_year,
        x = val_cohort_year,
        y = m,
        colour = cat_place_of_death,
        shape = cat_place_of_death,
        group = cat_place_of_death
      )
    ) +
    geom_point(size = rel(3), position = position_dodge(width = 0.5)) +
    geom_line(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.5)) +
    scale_shape_manual(values = scales::shape_pal()(3)[c(2,1,3)]) +
    scale_colour_manual(values = scales::viridis_pal()(3)[c(2,1,3)]) +
    labs(
      x = NULL,
      y = NULL,
      title = "Number of hospitalisations per person"
    )
)

fig_num_spells_mean %>%
  ggsave(
    filename = "X:/R2090/2021-0312 Deaths at home/outputs/service_users/fig_num_spells_mean.png",
    dpi = 300,
    width = 9,
    height = 6,
    bg = "white"
  )
