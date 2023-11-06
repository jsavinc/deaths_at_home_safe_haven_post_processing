## script to make odds ratio plots from regressions for home death


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)

# load functions ----------------------------------------------------------

source("./FUNCTIONS.R")

# load data ---------------------------------------------------------------

two_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_two_models_pre_vs_pandemic.csv")

four_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/logistic_regression_four_models_home_vs_hosp_vs_ch_pre_vs_pandemic.csv")
  
multinomial_models <- read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/associations with home death, logistic regression/multinomial_logistic_regression_two_models.csv")


# wrangle data ------------------------------------------------------------

## mainly I need to make nice variable names
make_nice_variable_labels <- function(data_tbl) {
  data_tbl %>%
    mutate(
      variable = replace_with_long_variable_name(variable),
      label = if_else(!is.na(level), paste0(variable,": ",level),variable),
      label = factor(label, levels = unique(label))
    )
}

two_models <- two_models %>% make_nice_variable_labels
four_models <- four_models %>% make_nice_variable_labels
multinomial_models <- 
  multinomial_models %>% 
  make_nice_variable_labels %>%
  mutate(
    or_inverse = 1 / or,
    or_ci_low_inverse = 1/or_ci_high,
    or_ci_high_inverse = 1/or_ci_low
  )


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
    x = NULL, y = NULL
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
