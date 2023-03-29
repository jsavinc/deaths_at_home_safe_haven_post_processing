## Chop up Urban-rural by PoD table two or more ways For descriptives, I need to
## report Urban-rural by PoD proportions in hte preliminary results, I reported
## the proportion of PoD within each SIMD quintile, but for the paper it's
## probably better to report the Urban-rural proportions within each PoD

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R")

ur_by_pod <-
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/place_of_death_by_ur_and_cohort.csv") %>%
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
    cat_ur8 = fct_explicit_na(factor(cat_ur8, levels = order_ur8), na_level = "Missing")
  )

## Large & Other urban combined:
ur_by_pod %>%
  select(-prop_within_simd) %>%
  filter(cat_ur8 %in% order_ur8[1:2]) %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(across(n:prop_within_pod, ~sum(.x)))

write_csv(ur_by_pod, file = "X:/R2090/2021-0312 Deaths at home/outputs/descriptives/ur_by_pod.csv")


## define the numeric values for the 'shape' aesthetic by default a maximum of 6
## are used, so I';'ll add enough for all levels of urban-rural 8-fold
point_shapes <- c(
  scales::shape_pal()(6), 10, 11
  )

(
  fig_ur_by_pod <-
    wrap_plots(
      ## N
      ur_by_pod %>%
        filter(cat_ur8!="Missing") %>%  # very few missing values, can be safely removed
        ggplot(aes(x = val_cohort_year, y = n, shape = cat_ur8, group = cat_ur8, colour = cat_ur8)) +
        geom_point(size = 2) +
        geom_line() +
        scale_y_continuous(labels = ~format(.x, big.mark=",")) +
        facet_wrap(~cat_place_of_death) +
        labs(
          title = "Number of deaths",
          x = NULL, y = NULL, 
          colour = "Urban-rural indicator",
          shape = "Urban-rural indicator"
        ) +
        scale_colour_grey() +
        scale_shape_manual(values = point_shapes) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle=45, hjust=1),
          legend.position = "top"
        )
      ,
      ## Proportions
      ur_by_pod %>%
        filter(cat_ur8!="Missing") %>%  # very few missing values, can be safely removed
        ggplot(aes(x = val_cohort_year, y = prop_within_pod, shape = cat_ur8, group = cat_ur8, colour = cat_ur8)) +
        geom_point(size = 2) +
        geom_line() +
        scale_y_continuous(labels = ~scales::percent(.x,accuracy = 1)) +
        facet_wrap(~cat_place_of_death) +
        labs(
          title = "Proportion of deaths",
          x = NULL, y = NULL, 
          colour = "Urban-rural indicator",
          shape = "Urban-rural indicator"
        ) +
        scale_colour_grey() +
        scale_shape_manual(values = point_shapes) +
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
    )
)

ggsave(
  plot = fig_ur_by_pod,
  filename = "X:/R2090/2021-0312 Deaths at home/outputs/descriptives/fig_ur_by_pod_n_and_prop.png",
  dpi = 300, 
  width = 18, units = "cm", 
  height = 15
)
