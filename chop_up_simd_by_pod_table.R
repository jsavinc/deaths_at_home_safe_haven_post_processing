## Chop up SIMD by PoD table two or more ways For descriptives, I need to report
## SIMD by PoD proportions in hte preliminary results, I reported the proportion
## of PoD within each SIMD quintile, but for the paper it's probably better to
## report the SIMD proportions within each PoD

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R")

simd_by_pod <-
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/place_of_death_by_simd_and_cohort.csv") %>%
  pivot_longer(
    cols = `2015-16`:`2020-21`,
    names_to = "val_cohort_year",
    values_to = "n_prop"
  ) %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  rename(prop_within_simd = prop) %>%
  group_by(val_cohort_year,cat_place_of_death) %>%
  mutate(prop_within_pod = n / sum(n)) %>%
  ungroup %>%
  relocate(val_cohort_year) %>%
  mutate(
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death),
    val_simd_quintile = fct_explicit_na(factor(val_simd_quintile), na_level = "Missing")
    )

write_csv(simd_by_pod, file = "X:/R2090/2021-0312 Deaths at home/outputs/descriptives/simd_by_pod.csv")

(
fig_simd_by_pod <-
  wrap_plots(
    ## N
    simd_by_pod %>%
      filter(val_simd_quintile!="Missing") %>%  # very few missing values, can be safely removed
      ggplot(aes(x = val_cohort_year, y = n, shape = val_simd_quintile, group = val_simd_quintile, colour = val_simd_quintile)) +
      geom_point(size = 2) +
      geom_line() +
      scale_y_continuous(labels = ~format(.x, big.mark=",")) +
      facet_wrap(~cat_place_of_death) +
      labs(
        title = "Number of deaths",
        x = NULL, y = NULL, 
        colour = "SIMD quintile (1=most deprived)",
        shape = "SIMD quintile (1=most deprived)"
      ) +
      scale_colour_grey() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top"
      )
    ,
    ## Proportions
    simd_by_pod %>%
      filter(val_simd_quintile!="Missing") %>%  # very few missing values, can be safely removed
      ggplot(aes(x = val_cohort_year, y = prop_within_pod, shape = val_simd_quintile, group = val_simd_quintile, colour = val_simd_quintile)) +
      geom_point(size = 2) +
      geom_line() +
      scale_y_continuous(labels = ~scales::percent(.x,accuracy = 1)) +
      facet_wrap(~cat_place_of_death) +
      labs(
        title = "Proportion of deaths",
        x = NULL, y = NULL, 
        colour = "SIMD quintile (1=most deprived)",
        shape = "SIMD quintile (1=most deprived)"
      ) +
      scale_colour_grey() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top"
      )
    ,
    nrow = 2,
    guides = "collect"
  ) +
  plot_annotation(
    theme = 
      theme(
        axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "bottom"
      )
  ) & guides(
    colour = guide_legend(nrow=1,byrow=TRUE),
    shape = guide_legend(nrow=1,byrow=TRUE)
    )
)

ggsave(
  plot = fig_simd_by_pod,
  filename = "X:/R2090/2021-0312 Deaths at home/outputs/descriptives/fig_simd_by_pod_n_and_prop.png",
  dpi = 300, 
  width = 18, units = "cm", 
  height = 15
  )
