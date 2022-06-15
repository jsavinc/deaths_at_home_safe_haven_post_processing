# Compute mean SIMD over time from frequency table

library(tidyverse)
library(patchwork)



# Load data ---------------------------------------------------------------


demographics <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/cohort_demographics.csv")

parse_num_prop <- function(data_tbl, num_prop) {
  data_tbl %>%
    extract(
      col = rlang::sym(num_prop), 
      into = c("n","prop"), 
      regex = "(\\d+\\,{0,1}\\d*) \\((\\d+\\.{0,1}\\d*)\\%\\)", 
      remove = TRUE
    ) %>%
    mutate(
      n = as.numeric(str_remove_all(n, "\\,")),
      prop = as.numeric(prop) / 100
    )
}

simd <- 
  demographics %>%
  slice(15:24) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  parse_num_prop(num_prop = "value") %>%
  mutate(val_cohort_year = factor(val_cohort_year)) %>%
  rename(val_simd = Characteristic) %>%
  mutate(val_simd = as.integer(val_simd))

simd_descriptives <-
  simd %>%
  select(-prop) %>%
  uncount(n) %>%
  group_by(val_cohort_year) %>%
  summarise(
    mean = mean(val_simd),
    sd = sd(val_simd),
    median = median(val_simd),
    q25 = quantile(val_simd, 0.25),
    q75 = quantile(val_simd, 0.75),
    q90 = round(quantile(val_simd, 0.9)),
    q95 = round(quantile(val_simd, 0.95)),
    .groups = "drop"
  )
