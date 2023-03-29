## Combined table of service use characteristics. This combines various metrics
## like LOS, number of hospital hours, etc., on different scales - all the
## metrics are represented as ratios of the 2019/2020 value, which is set to 1.0

library(tidyverse)
library(patchwork)

source("./FUNCTIONS.R")

# Load data ---------------------------------------------------------------

## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv")


dtdt_descriptives <- read_csv(
  file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/dtdt_smr01_descriptives_by_pod.csv"
)

gpooh_uses <- read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/exploratory_descriptives/gpooh_visits_descriptives_by_pod.csv")

# TODO: should there be normalising between PoD also? e.g. home = 1
dtdt_descriptives %>%
  group_by(cat_place_of_death) %>%
  mutate(ratio = m / m[val_cohort_year=="2019-20"])



# %>%
#   mutate(
#     cat_place_of_death = factor(cat_place_of_death, levels = order_place_of_death)
#   )