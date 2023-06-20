## number & proportion of high frequency users by year & PoD

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R", local = TRUE)  # load helper functions

# load data ---------------------------------------------------------------

## load cohort size by PoD - used to recalculate some of the usage below
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(
    n = sum(n),
    .groups = "drop"
  )

## hospital spells - these include non-users
hospital_spells_pod <- read_csv(
  file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/smr01_spells_categorised_by_pod.csv"
  ) %>%
  pivot_annual_to_long() %>%
  # rename(val_cohort_year = cohort_year) %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(
    cat_smr01_spells = factor(cat_smr01_spells, levels = c(as.character(0:14),"15+")),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
  )

## narrow it down to high freq users
hospital_spells_pod %>%
  mutate(
    cat_smr01_spells_hifreq = cat_smr01_spells %in% levels(cat_smr01_spells)[9:16]
  ) %>%
  count(val_cohort_year, cat_place_of_death, cat_smr01_spells_hifreq, wt = n) %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  mutate(prop = n/sum(n)) %>%
  ungroup %>%
  filter(cat_smr01_spells_hifreq) %>%
  ggplot(aes(x = val_cohort_year, y = prop, group = cat_place_of_death, colour = cat_place_of_death, shape = cat_place_of_death)) +
  geom_point(size = rel(3), alpha = 0.8) +
  geom_line() +
  scale_colour_viridis_d() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = NULL, y = NULL)
  # ggplot(aes(x = val_cohort_year, y = prop, group = cat_smr01_spells_hifreq, fill = cat_smr01_spells_hifreq)) +
  # geom_col(position = position_dodge(width = 1)) +
  # facet_wrap(~cat_place_of_death)
