# Script to prettify the demographics table

library(tidyverse)

demographics <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/cohort_demographics.csv") %>%
  mutate(
    Variable = case_when(
      Characteristic == "amt_age" ~ "Age, Median(IQR)",
      Characteristic == "cat_sex" ~ "Sex, N (%)",
      Characteristic == "cat_place_of_death" ~ "Place of death, N (%)",
      TRUE ~ NA_character_
    )
  )
