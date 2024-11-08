# Script to produce plots/tables for the IPDLN 2024 presentation

# constants ---------------------------------------------------------------

dir_output <- 
  "C:/Users/40011625/OneDrive - Edinburgh Napier University/SCADR/covid_mortality_home/presentations/IPDLN 2024/"

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

# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(patchwork)  # for assembling plots
library(ggedit)  # for editing plots after they've been defined


source("./FUNCTIONS.R")  # load common functions


# set figure theme --------------------------------------------------------

theme_descriptive_paper_1 <-
  theme_minimal(base_size = 7) +
  theme(
    text = element_text(family = "sans"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
    # remove gap to the left of y axis title and below the x axis title
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
theme_set(theme_descriptive_paper_1)

# load & wrangle data -----------------------------------------------------


## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(n=sum(n), .groups = "drop") %>%
  code_place_of_death_consistently

pod <- cohort_by_pod %>%
  group_by(val_cohort_year, cat_place_of_death=="All") %>%
  mutate(prop = n / sum(n)) %>%
  ungroup %>%
  select(val_cohort_year, cat_place_of_death, n, prop) %>%
  code_place_of_death_consistently

## recalculated from the breakdown by sex
pall_care_needs_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/palliative_care_needs_exploration/palliative_estimates_murtaugh_upper_mid_by_cohort_pod_sex.csv") %>%
  pivot_annual_to_long() %>%
  parse_n_prop(col_n_prop = n_prop) %>%
  mutate(denominator = round(n / prop)) %>%  # infer denominator from proportions
  group_by(cat_place_of_death, val_cohort_year) %>%
  summarise(
    n = sum(n), 
    denominator = sum(denominator), 
    .groups="drop"
  ) %>%
  mutate(prop = n / denominator) %>%
  code_place_of_death_consistently

anticipatory_meds_by_pod <-
  # read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/prescriptions and dispensing changes August 2024/tbl_individuals_palliative_care_needs_dispensing_pod.csv") %>%
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/prescriptions and dispensing changes August 2024/tbl_individuals_with_dispensing_pod.csv") %>%
  filter(medication == "Anticipatory medicines") %>%
  code_place_of_death_consistently

# Note: As of 2024-09-12 I discovered that the below was exported wrong - I
# accidentally exported the same table for all recipients instead of for
# recitipents with palliative care needs. I think I can reconstruct the table
# from another, however:
anticipatory_meds_pall_needs_only_by_pod <-
  # below is the table that should contain only data on pwople with estimated palliative care
  # read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/prescriptions and dispensing changes August 2024/tbl_individuals_with_dispensing_pod_pall.csv") %>%
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/prescriptions and dispensing changes August 2024/tbl_individuals_palliative_care_needs_dispensing_pod.csv") %>%
  filter(medication == "Anticipatory medicines") %>%
  ## keep only the people w/ palliative care needs; the calculated prop is
  ## already percentage of all people with estimated pall.care needs
  filter(bin_palliative_murtagh_mid_upper) %>%
  code_place_of_death_consistently


## Home palliative deaths increased by XX%
pall_care_needs_by_pod$n[pall_care_needs_by_pod$cat_place_of_death=="Home" & pall_care_needs_by_pod$val_cohort_year=="2020-21"] / pall_care_needs_by_pod$n[pall_care_needs_by_pod$cat_place_of_death=="Home" & pall_care_needs_by_pod$val_cohort_year=="2019-20"]
## compared to increase in home deaths in total
pod$n[pod$cat_place_of_death=="Home" & pod$val_cohort_year=="2020-21"] / pod$n[pod$cat_place_of_death=="Home" & pod$val_cohort_year=="2019-20"]


# plots -------------------------------------------------------------------

(fig_palliative_care_needs <- 
   pall_care_needs_by_pod %>%
   filter(cat_place_of_death!="All") %>%
   filter(val_cohort_year!="2020-21, excl. Covid") %>%
   ggplot(
     data = .,
     aes(
       x = val_cohort_year,
       y = n,
       group = cat_place_of_death,
       colour = cat_place_of_death,
       shape = cat_place_of_death
     ), 
   ) +
   geom_line() +
   geom_point(size = rel(3)) +
   scale_colour_viridis_d(option = "H", direction = 1) +
   scale_y_continuous(labels = scales::comma, n.breaks = 6, limits = c(0,NA)) +
   labs(x = NULL, y = NULL, title = "Decedents with palliative care needs (N)") +
   theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
)


(fig_anticipatory_medicine_recipients <- 
    anticipatory_meds_by_pod %>%
    filter(cat_place_of_death!="All") %>%
    ggplot(
      data = .,
      aes(
        x = val_cohort_year,
        y = n,
        group = cat_place_of_death,
        colour = cat_place_of_death,
        shape = cat_place_of_death
      ), 
    ) +
    geom_line() +
    geom_point(size = rel(3)) +
    scale_colour_viridis_d(option = "H", direction = 1) +
    scale_y_continuous(labels = scales::comma, n.breaks = 6, limits = c(0,NA)) +
    labs(x = NULL, y = NULL, title = "Decedents receiving anticipatory medicines (N)") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
)

( # not sure this is so informative...
  fig_palliative_care_needs_prop <-
    fig_palliative_care_needs <- 
    pall_care_needs_by_pod %>%
    filter(cat_place_of_death!="All") %>%
    filter(val_cohort_year!="2020-21, excl. Covid") %>%
    ggplot(
      data = .,
      aes(
        x = val_cohort_year,
        y = prop,
        group = cat_place_of_death,
        fill = cat_place_of_death,
        shape = cat_place_of_death
      ), 
    ) +
    facet_grid(cols=vars(cat_place_of_death)) +
    geom_col(position = position_dodge(width = 1), colour = "grey10") +
    scale_fill_viridis_d(option = "H", direction = 1) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = NULL, y = NULL, title = "Decedents with palliative care needs (N)") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
)

(fig_anticipatory_medicine_recipients_prop <- 
    anticipatory_meds_by_pod %>%
    filter(cat_place_of_death!="All") %>%
    ggplot(
      data = .,
      aes(
        x = val_cohort_year,
        y = prop,
        group = cat_place_of_death,
        colour = cat_place_of_death,
        shape = cat_place_of_death
      ), 
    ) +
    geom_line() +
    geom_point(size = rel(3)) +
    scale_colour_viridis_d(option = "H", direction = 1) +
    scale_y_continuous(labels = scales::label_percent(), n.breaks = 6, limits = c(0,NA)) +
    labs(x = NULL, y = NULL, title = "Proportion decedents receiving anticipatory medicines (%)") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
)

(fig_anticipatory_medicine_recipients_with_pall_needs_prop <- 
    anticipatory_meds_pall_needs_only_by_pod %>%
    filter(cat_place_of_death!="All") %>%
    ggplot(
      data = .,
      aes(
        x = val_cohort_year,
        y = prop,
        group = cat_place_of_death,
        colour = cat_place_of_death,
        shape = cat_place_of_death
      ), 
    ) +
    geom_line() +
    geom_point(size = rel(3)) +
    scale_colour_viridis_d(option = "H", direction = 1) +
    scale_y_continuous(labels = scales::label_percent(), n.breaks = 6, limits = c(0,NA)) +
    labs(x = NULL, y = NULL, title = str_wrap("Proportion decedents with palliative care needs receiving anticipatory medicines (%)", width = 50)) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top")
)



# patchwork plots ---------------------------------------------------------


(fig1_pall_care_anticipatory_meds <-
   wrap_plots(
     fig_palliative_care_needs,
     fig_anticipatory_medicine_recipients, 
     guides = "collect"
   ) & theme(legend.position = "top")
)

(fig2_anticipatory_meds_proportions <-
    wrap_plots(
      fig_anticipatory_medicine_recipients_prop,
      fig_anticipatory_medicine_recipients_with_pall_needs_prop,
      guides = "collect"
    ) & theme(legend.position = "top")
)

# save plots --------------------------------------------------------------

save_plot <- 
  function(plot, filename, width, height) {
    walk(
      .x = c(".png", ".emf", ".svg"),
      .f = function(extension) {
        ggsave(
          plot = plot,
          filename = paste0(dir_output, filename, extension),
          width = width,
          height = height,
          units = "cm",
          dpi = 300,
          bg = "white"
        )
      }
    )
  }


save_plot(
  plot = fig1_pall_care_anticipatory_meds,
  filename = "fig1_pall_care_anticipatory_meds",
  width = 16,
  height = 10
)
save_plot(
  plot = fig2_anticipatory_meds_proportions,
  filename = "fig2_anticipatory_meds_proportions",
  width = 16,
  height = 10
)
