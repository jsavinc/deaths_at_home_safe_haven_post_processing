# Extract cohort size per year

library(tidyverse)

demographics <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/cohort_demographics.csv")

cohort_size <- tibble(
  raw = names(demographics)[-1]  # the cohort size is in the column names, except the 1st column
) %>%
  extract(col = raw, into = c("val_cohort_year", "n"), regex = "(20\\d{2}\\-\\d{2}), N \\= (.*)", remove = TRUE) %>%
  mutate(n = as.integer(str_remove_all(n, ",")))

write_csv(cohort_size, file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size.csv")


cohort_size_pod <-
  demographics %>%
  rename(cat_place_of_death = Characteristic) %>%
  slice(6:8) %>%  # the rows where the POD nubmers are
  pivot_longer(
    cols = 2:7,
    names_to = "val_cohort_year",
    names_transform = ~str_sub(.x, 1, 7),
    values_to = "n",
    values_transform = ~str_replace_all(.x, pattern = "^(\\d{0,3}\\,{0,1}\\d{0,3})\\s\\(.*$", replacement = "\\1")
  )

write_csv(cohort_size_pod, file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv")
