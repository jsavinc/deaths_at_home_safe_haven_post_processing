# Script to make a 3-part plot of average age, comorbidity, and palliative care
# needs, for the SCADR blog

# constants ---------------------------------------------------------------

dir_output <- 
  "C:/Users/40011625/OneDrive - Edinburgh Napier University/SCADR/covid_mortality_home/blog_scadr/"

levels_place_of_death <- c("Hospital", "Home", "Care home & other", 
                           "All")


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)


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


# set figure theme --------------------------------------------------------

theme_descriptive_paper_1 <-
  theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = "sans"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
    # remove gap to the left of y axis title and below the x axis title
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.text = element_text(size = 9),
    # legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 60, hjust = 1)
  )
theme_set(theme_descriptive_paper_1)


# load data ---------------------------------------------------------------

## load cohort size by PoD
cohort_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  repeat_data_adding_a_catchall_category(var_to_change = cat_place_of_death, label_for_catchall = "All") %>%
  group_by(val_cohort_year, cat_place_of_death) %>%
  summarise(n=sum(n), .groups = "drop") %>%
  code_place_of_death_consistently


age_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_age_cohort_pod.csv") %>%
  code_place_of_death_consistently

## this includes both number of comorbidities & elixhauser index
comorb_by_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/service_usage_by_cause_of_death/deaths_pod_comorb_index.csv") %>%
  pivot_annual_to_long(new_col = "mean_ci") %>%
  parse_m_ci(col_m_ci = mean_ci) %>%
  code_place_of_death_consistently %>%
  left_join(
    cohort_by_pod %>% rename(denominator = n),
    by = c("cat_place_of_death", "val_cohort_year")
  ) %>%
  recalculate_sd_from_mean_ci_and_n(mean_var = m, ci_lo_var = ci_lo, n_var = denominator, new_sd_var = sd)

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


# individual plots --------------------------------------------------------

fig_age <-
  ggplot(age_by_pod, aes(x=val_cohort_year, y = mean, shape = cat_place_of_death, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point(size = rel(3)) + 
  scale_colour_viridis_d(option = "E") +
  theme_descriptive_paper_1 +
  labs(
    x = NULL,
    y = NULL,
    colour = "Place of death",
    shape = "Place of death",
    title = str_wrap("Average age at death (years)", width = 40)
  )

fig_comorb <-
  comorb_by_pod %>%
  filter(measure == "Number of comorbidities incl. in the Elixhauser index") %>%
  filter(cat_place_of_death != "All") %>%
  ggplot(aes(x=val_cohort_year, y = m, shape = cat_place_of_death, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point(size = rel(3)) +
  scale_colour_viridis_d(option = "E") +
  theme_descriptive_paper_1 +
  labs(
    x = NULL,
    y = NULL,
    colour = "Place of death",
    shape = "Place of death",
    title = str_wrap("Average number of comorbidities", width = 40)
  )

fig_palliative <-
  pall_care_needs_by_pod %>%
  filter(cat_place_of_death != "All") %>%
  filter(val_cohort_year != "2020-21, excl. Covid") %>%
  ggplot(aes(x=val_cohort_year, y = n, shape = cat_place_of_death, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point(size = rel(3)) +
  # NOTE: I think adding percentages makes the plot harder to read
  # geom_text(aes(
  #   x = val_cohort_year,
  #   label = scales::label_percent(accuracy = 0.1)(prop)
  # ), ) +
  scale_colour_viridis_d(option = "E") +
  theme_descriptive_paper_1 +
  scale_y_continuous(label = scales::comma) +
  labs(
    x = NULL,
    y = NULL,
    colour = "Place of death",
    shape = "Place of death",
    title = str_wrap(
      "Number of people who would have benefitted from palliative care needs",
      width = 40
    )
  )

# compile plot ------------------------------------------------------------

(fig_blog <-
  wrap_plots(
    fig_age, fig_comorb, fig_palliative, 
    ncol = 2, 
    guides = "collect"
  ) + plot_annotation(tag_levels = "a", tag_suffix = ")")
)

ggsave(
  filename = paste0(dir_output,"fig_age_comorb_pall_care_needs.png"), plot = fig_blog, width = 7, height = 7/1.4,  dpi = 300, bg = "white"
)
