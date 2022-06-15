# Extract cohort size per year

library(tidyverse)

demographics <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/cohort_demographics.csv")

cohort_size <- tibble(
  raw = names(demographics)[-1]  # the cohort size is in the column names, except the 1st column
) %>%
  extract(col = raw, into = c("cohort_year", "n"), regex = "(20\\d{2}\\-\\d{2}), N \\= (.*)", remove = TRUE) %>%
  mutate(n = as.integer(str_remove_all(n, ",")))

write_csv(cohort_size, file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size.csv")
