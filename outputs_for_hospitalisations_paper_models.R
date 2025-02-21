# script compiling tables & plots for the hospitalisations paper


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)

# constants ---------------------------------------------------------------

dir_output <- 
  "C:/Users/40011625/OneDrive - Edinburgh Napier University/SCADR/covid_mortality_home/drafts/hospitalisations paper/"

levels_place_of_death <- c("Hospital", "Home", "Care home & other", 
                           "All")


# functions ---------------------------------------------------------------


code_place_of_death_consistently <- function(data_tbl) {
  data_tbl %>%
    mutate(
      ## shorten to "Home"
      cat_place_of_death = if_else(cat_place_of_death == "Home & non-institution", true = "Home", false = cat_place_of_death),
      cat_place_of_death = factor(cat_place_of_death, levels = levels_place_of_death)
    )
}

source("./FUNCTIONS.R")  # load common functions

# load data ---------------------------------------------------------------

tbl_regression_los_zinb <-
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/modelling_hospitalisations/tbl_models_los_zinb.csv")

tbl_regression_hospitalisations_negbin <-
  read_csv("X:/R2090/2021-0312 Deaths at home/safe_haven_exports/modelling_hospitalisations/tbl_models_hosp_negbin.csv")

# TODO: import data once I have it!
tbl_regression_discharge <- NULL


# wrangle data ------------------------------------------------------------

tbl_regression_los_zinb_for_paper <-
  tbl_regression_los_zinb %>%
  filter(variable!="dispersion") %>%  # remove the dispersion parameter, can be reported elsewhere
  # re-calculate the IRR for age & driving time
  mutate(
    across(matches("exp\\_"), ~if_else(variable %in% c("amt_age"), .x^10, .x)),
    level = if_else(variable=="amt_age", "10 years", level)
  ) %>%
  mutate(
    across(matches("exp\\_"), ~if_else(variable %in% c("amt_drive_to_hosp_mins"), .x^15, .x)),
    level = if_else(variable=="amt_drive_to_hosp_mins", "15 min", level)
  ) %>%
  mutate(
    across(matches("exp\\_"), ~if_else(variable %in% c("val_elixhauser_index_vanwalraven"), .x^5, .x)),
    level = if_else(variable=="val_elixhauser_index_vanwalraven", "5 points", level)
  ) %>%
  # compute reciprocal of pandemic variable so that pre-pandemic is the reference
  mutate(
    level = if_else(level== "Pre-pandemic", "Pandemic", level),
    exp_coef_new = if_else(variable == "cat_cohort", 1/exp_coef, exp_coef),
    exp_CI_low_new = if_else(variable == "cat_cohort", 1/exp_CI_high, exp_CI_low),
    exp_CI_high_new = if_else(variable == "cat_cohort", 1/exp_CI_low, exp_CI_high),
  ) %>%
  select(-c(exp_coef, exp_CI_low, exp_CI_high)) %>%
  rename_all(~str_remove_all(., pattern = "\\_new$")) %>%
  # rearrange for a nice regression table
  select(Component, variable, level, exp_coef, exp_CI_low, exp_CI_high, p) %>%
  mutate(
    variable = replace_with_long_variable_name(variable),  # rename to nice names
    irr = exp_coef,
    irr_ci = paste0(format(exp_CI_low,nsmall=2,digits=2),"-",format(exp_CI_high,nsmall=2,digits=2)),
    p = scales::pvalue(p)
  ) %>%
  select(-c(exp_coef, exp_CI_low, exp_CI_high)) %>%
  mutate(Component = factor(str_sub(Component, 1, 4), levels = c("zero", "cond"))) %>%
  arrange(Component) %>%
  pivot_wider(id_cols = c(variable, level), names_from = Component, values_from = c(irr, irr_ci, p), names_vary = "slowest") %>%
  # rename irr to or for zero-inflated component
  rename(or_zero = irr_zero, or_ci_zero = irr_ci_zero)

tbl_regression_hospitalisations_negbin_for_paper <-
  tbl_regression_hospitalisations_negbin %>%
  # re-calculate the IRR for age, driving time, elixhauser index
  mutate(
    across(matches("IRR\\_"), ~if_else(variable %in% c("amt_age"), .x^10, .x)),
    level = if_else(variable=="amt_age", "10 years", level)
  ) %>%
  mutate(
    across(matches("IRR\\_"), ~if_else(variable %in% c("amt_drive_to_hosp_mins"), .x^15, .x)),
    level = if_else(variable=="amt_drive_to_hosp_mins", "15 min", level)
  ) %>%
  mutate(
    across(matches("IRR\\_"), ~if_else(variable %in% c("val_elixhauser_index_vanwalraven"), .x^5, .x)),
    level = if_else(variable=="val_elixhauser_index_vanwalraven", "5 points", level)
  ) %>%
  # compute reciprocal of pandemic variable so that pre-pandemic is the reference
  mutate(
    level = if_else(level== "Pre-pandemic", "Pandemic", level),
    IRR_new = if_else(variable == "cat_cohort", 1/IRR, IRR),
    IRR_CI_low_new = if_else(variable == "cat_cohort", 1/IRR_CI_high, IRR_CI_low),
    IRR_CI_high_new = if_else(variable == "cat_cohort", 1/IRR_CI_low, IRR_CI_high),
  ) %>%
  select(-c(IRR, IRR_CI_low, IRR_CI_high)) %>%
  rename_all(~str_remove_all(., pattern = "\\_new$")) %>%
  # rearrange for a nice regression table
  select(variable, level, IRR, IRR_CI_low, IRR_CI_high, p) %>%
  mutate(
    variable = replace_with_long_variable_name(variable),  # rename to nice names
    irr = IRR,
    irr_ci = paste0(format(IRR_CI_low,nsmall=2,digits=2),"-",format(IRR_CI_high,nsmall=2,digits=2)),
    p = scales::pvalue(p)
  ) %>%
  select(-c(IRR, IRR_CI_low, IRR_CI_high)) %>%
  relocate(p, .after = irr_ci)

# TODO: format table for paper once I have it
tbl_regression_discharge_for_paper <- NULL

# compile table -----------------------------------------------------------

tbl_models_for_paper_los_and_hospitalisations <-
  full_join(
    tbl_regression_los_zinb_for_paper,
    tbl_regression_hospitalisations_negbin_for_paper %>%
      rename(irr_hosp = irr, irr_ci_hosp = irr_ci, p_hosp = p),
    join_by(variable, level)
  )

# save table --------------------------------------------------------------

write_csv(x = tbl_models_for_paper_los_and_hospitalisations, file = paste0(dir_output, "table_regressions_los_and_hospitalisations.csv"))


# compile plots -----------------------------------------------------------

## wrangle data for plots
model_coefs_for_fig <-
  bind_rows(
    tbl_models_for_paper_los_and_hospitalisations %>%
      select(1:2, matches("\\_zero")) %>% 
      rename_with(.fn=~c("variable","level","estimate","ci","p")) %>%
      mutate(
        outcome = "No admission (zero-inflated component)",
        measure = "OR"
        ),
    tbl_models_for_paper_los_and_hospitalisations %>%
      select(1:2, matches("\\_cond")) %>% 
      rename_with(.fn=~c("variable","level","estimate","ci","p")) %>%
      mutate(
        outcome = "Days in hospital (negative binomial component)",
        measure = "IRR"
        ),
    tbl_models_for_paper_los_and_hospitalisations %>%
      select(1:2, matches("\\_hosp")) %>% 
      rename_with(.fn=~c("variable","level","estimate","ci","p")) %>%
      mutate(
        outcome = "Number of admissions (negative binomial)",
        measure = "IRR"
        ),
  ) %>%
  # split CI range into low & high again
  separate_wider_delim(
    cols = ci, names = c("ci_low","ci_high"), delim = "-", 
    ) %>%
  mutate(across(c(ci_low,ci_high), as.numeric)) %>%
  mutate(
    variable = factor(variable, levels = unique(variable)),
    reference_level = case_when(
      variable == "Study period" ~ "Pre-pandemic",
      variable == "Sex" ~ "Female",
      variable == "Marital status" ~ "Single",
      variable == "Urban-rural 2-fold" ~ "Urban", 
      variable == "Cause of death (NRS categories)" ~ "Cancer",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(outcome, variable) %>%  # need outcome also, one variable per model!
  mutate(
    variable_label = case_when(
      n() == 1 ~ paste0(
        variable,
        ":\n",
        level,
        if_else(!is.na(reference_level), paste0(" (ref: ", reference_level, ")"), "")
      ),
      row_number() == 1 ~ paste0(
        variable,
        ":\n",
        level,
        if_else(!is.na(reference_level), paste0(" (ref: ", reference_level, ")"), "")
      ),
      TRUE ~ level
    ) %>% factor(., levels = unique(.)
    )
  ) %>%
  ungroup
# TODO: reverse order within each factor grouping
  
# paste0(
#   if_else(n()==1, true = paste0(variable[1],":\n"), false = ),
#   level
# ) %>% 

# TODO: improve this for legibility

(fig_models_1 <-
  model_coefs_for_fig %>%
  mutate(outcome = paste0(measure, ", ", outcome) %>% factor(., levels = unique(.))) %>%
  filter(variable != "(Intercept)") %>%
  ggplot(
    data = ., aes(
      x = estimate,
      xmin = ci_low,
      xmax = ci_high,
      # y = paste0(variable,": ",level),
      y = fct_rev(variable_label),
      # shape = outcome,
      # colour = outcome
    )
  ) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(shape = 4, size = rel(2), position = position_dodge(width = 1)) +
    geom_linerange(position = position_dodge(width = 1)) +
    facet_wrap(~outcome, labeller = as_labeller(~str_wrap(.x, width = 30)), strip.position = "bottom") +
    scale_x_continuous(limits = c(0,6), breaks = scales::breaks_pretty(n=6)) +
    labs(x = NULL, y = NULL) +
    theme(
      # facet title instead of x-axis 
      strip.placement = "outside",
      # border around facets
      panel.border = element_rect(fill = NA, colour = "grey20")
      ) +
    NULL
)

ggsave(
  filename = paste0(dir_output, "fig_models_los_and_hospitalisations.png"),
  plot = fig_composite,
  bg = "white",
  dpi = 300,
  width = 10,
  height = 10,
  units = "in"
)


