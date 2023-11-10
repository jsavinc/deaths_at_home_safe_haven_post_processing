## script to make odds ratio plots from regressions for home death


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)

# load functions ----------------------------------------------------------

source("./FUNCTIONS.R")

# load data ---------------------------------------------------------------

two_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_two_models_pre_vs_pandemic.csv")
two_models2 <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_two_models_pre_vs_pandemic_part2.csv")

four_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_four_models_home_vs_hosp_vs_ch_pre_vs_pandemic.csv")
four_models2 <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_four_models_home_vs_hosp_vs_ch_pre_vs_pandemic_part2.csv")
  
multinomial_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/multinomial_logistic_regression_two_models.csv")
multinomial_models2 <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/multinomial_logistic_regression_two_models_part2.csv")

ame_two_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/ame_two_models_pre_vs_pandemic.csv")
ame_four_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/ame_four_models_pre_vs_pandemic.csv")

mem_two_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/mem_two_models_pre_vs_pandemic.csv")
mem_multinomial <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/mem_multinomial_pre_vs_pandemic.csv")


## cohort size, number of people who died due to Covid-19
n_people_in_model <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_n_individuals_included.csv")
n_people_in_model2 <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_n_individuals_included_part2.csv")

# wrangle data ------------------------------------------------------------

## merge the exports from different times:

two_models <- bind_rows(two_models, two_models2)
four_models <- bind_rows(four_models, four_models2)
multinomial_models <- bind_rows(multinomial_models, multinomial_models2)

n_people_in_model <- 
  bind_rows(
    n_people_in_model, 
    n_people_in_model2 %>% mutate(cat_cohort = "Pandemic, excl. people who died from Covid-19")
  ) %>%
  distinct %>%
  rename(cohort = cat_cohort)

## mainly I need to make nice variable names
make_nice_variable_labels <- function(data_tbl) {
  data_tbl %>%
    mutate(
      variable = replace_with_long_variable_name(variable),
      label = if_else(!is.na(level), paste0(variable,": ",level),variable),
      label = factor(label, levels = unique(label))
    )
}

make_nice_variable_labels_marginal <- function(data_tbl) {
  data_tbl %>%
    mutate(
      contrast = 
        if_else(
          condition = str_starts(term, "bin\\_"),
          true = contrast %>%
            str_replace_all(., pattern = "\\bFALSE\\b|\\b0\\b", replacement = "No") %>%
            str_replace_all(., pattern = "\\bTRUE\\b|\\b1\\b", replacement = "Yes"),
          false = contrast
        ),
      term = replace_with_long_variable_name(term),
      label = if_else(!is.na(contrast), paste0(term,": ",contrast),term),
      label = factor(label, levels = unique(label))
    )
}

## helper function to only keep pandemic cohort excluding covid-19
use_pandemic_cohort_excl_covid <- function(data_tbl) {
  data_tbl %>% filter(cohort!="Pandemic")
}

two_models <-
  two_models %>% 
  use_pandemic_cohort_excl_covid %>%
  make_nice_variable_labels

four_models <- 
  four_models %>% 
  use_pandemic_cohort_excl_covid %>%
  make_nice_variable_labels

multinomial_models <- 
  multinomial_models %>% 
  use_pandemic_cohort_excl_covid %>%
  make_nice_variable_labels %>%
  mutate(  # invert the ORs so they indicate OR of home death vs. hosp/CH
    or_inverse = 1 / or,
    or_ci_low_inverse = 1/or_ci_high,
    or_ci_high_inverse = 1/or_ci_low
  )

ame_two_models <-
  ame_two_models %>%
  arrange(term = factor(term, levels = unique(two_models$variable))) %>%
  use_pandemic_cohort_excl_covid %>%
  make_nice_variable_labels_marginal

n_people_in_model <-
  n_people_in_model %>%
  use_pandemic_cohort_excl_covid()


# report number of people in model ----------------------------------------

n_people_in_model %>%
  group_by(cohort) %>%
  summarise(n = sum(n)) %>%
  write_csv("X:/R2090/2021-0312 Deaths at home/outputs/home_death_associations/n_individuals_two_models.csv")


# make plots! -------------------------------------------------------------

# TODO: maybe frame the service usage coefficients in terms of multiples/average service uses? right now the effects look small
# TODO: split up into demographics, geographical, and service use - the graph is too big at the minute!
# TODO: for presentation, present the simpler model first (two models), and then 'zoom in' on the multinomial model wher there are substantial differences: marital status, SIMD, age, palliative care needs & dispensing, service use


(fig_or_two_models <- ggplot(
  data = two_models,
  aes(x = or, xmin = or_ci_low, xmax = or_ci_high, y = fct_rev(label), colour = cohort, shape = cohort)
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = rel(2), position = position_dodge(width = 0.7)) +
  geom_linerange(position = position_dodge(width = 0.7)) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 10)) +
  labs(
    x = NULL, y = NULL,
    title = "Home death associations: Odds ratios"
  ) +
  theme(legend.position = "top")
)


(fig_or_four_models <- ggplot(
  data = four_models,
  aes(x = or, xmin = or_ci_low, xmax = or_ci_high, y = fct_rev(label), colour = comparison, shape = cohort)
) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(size = rel(2), position = position_dodge(width = 0.7)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 10)) +
    labs(
      x = NULL, y = NULL
    ) +
    theme(legend.position = "top")
) 

(fig_or_multinomial <- ggplot(
  data = multinomial_models,
  aes(x = or_inverse, xmin = or_ci_low_inverse, xmax = or_ci_high_inverse, y = fct_rev(label), colour = response_level, shape = cohort)
) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(size = rel(2), position = position_dodge(width = 0.7)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 10)) +
    labs(
      x = NULL, y = NULL
    ) +
    theme(legend.position = "top")
) 

## Marginal effects
(fig_ame_two_models <- ggplot(
  data = ame_two_models,
  aes(x = estimate, xmin = conf.low, xmax = conf.high, y = fct_rev(label), colour = cohort, shape = cohort)
) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = rel(2), position = position_dodge(width = 0.7)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks(n = 5)) +
    labs(
      x = NULL, y = NULL, title = "Home death associations: Average Marginal Effect (AME)"
    ) +
    theme(legend.position = "top")
)


## Chopped up OR graphs
(fig_or_two_models_demographics <- ggplot(
    data = two_models %>% filter(variable %in% c("(Intercept)", "Sex", "Age group", "Marital status")),
    aes(x = or, xmin = or_ci_low, xmax = or_ci_high, y = fct_rev(label), colour = cohort, shape = cohort)
  ) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(size = rel(2), position = position_dodge(width = 0.7)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 10)) +
    labs(
      x = NULL, y = NULL
    ) +
    theme(legend.position = "top")
  )

(fig_or_two_models_geographics <- ggplot(
  data = two_models %>% filter(variable %in% c("SIMD quintile (1=most deprived)", "Urban-rural 3-fold", "Driving time to nearest hospital (minutes)")),
  aes(x = or, xmin = or_ci_low, xmax = or_ci_high, y = fct_rev(label), colour = cohort, shape = cohort)
) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(size = rel(2), position = position_dodge(width = 0.7)) +
    geom_linerange(position = position_dodge(width = 0.7)) +
    scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 10)) +
    labs(
      x = NULL, y = NULL
    ) +
    theme(legend.position = "top")
)


# Save figures ------------------------------------------------------------

ggsave(fig_or_two_models, filename = "X:/R2090/2021-0312 Deaths at home/outputs/home_death_associations/fig_or_two_models.png", dpi = 300, bg = "white", width = 10, height = 7)

ggsave(fig_ame_two_models, filename = "X:/R2090/2021-0312 Deaths at home/outputs/home_death_associations/fig_ame_two_models.png", dpi = 300, bg = "white", width = 10, height = 7)


# Tables of results -------------------------------------------------------

ame_two_models %>%
  select(cohort, label, estimate, p.value, conf.low, conf.high) %>%
  # filter(str_detect(label, "Number|alliative|Age|Married")) %>%
  mutate(
    percent = scales::label_percent(accuracy = 0.1)(estimate),
    ci_95 = glue::glue("{scales::label_percent(accuracy = 0.1)(conf.low)}, {scales::label_percent(accuracy = 0.1)(conf.high)}")
  ) %>%
  # TODO: is this the most succinct way of presenting this?
  select(cohort, label, percent) %>%
  pivot_wider(names_from = cohort, values_from = percent)
