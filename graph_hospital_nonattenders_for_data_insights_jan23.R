## Hi-resolution graph of hospital non-attenders by PoD for SCADR data insights,
## January 2023

library(tidyverse)
library(patchwork)

# TODO: none of this bullshit with importing fonts works
## this is to deal with error when changing font and the plots now rendering
# Error in grid.Call(L_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
#                      Polygon edge not found
## install particular version of Rttf2pt1 if not installed yet
# if (!is.element("Rttf2pt1", installed.packages()[,1])) remotes::install_version("Rttf2pt1", version = "1.3.8")
# library(Rttf2pt1)

# Load & wrangle data -----------------------------------------------------

los_cohort_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/safe_haven_exports/imgs2022_preliminary_results/descriptives_los_cohort_pod.csv") %>%
  mutate(
    mean_ci_lo = mean + qt(p = 0.025, df = n_admitted - 1) * sd /sqrt(n_admitted),
    mean_ci_hi = mean + qt(p = 0.975, df = n_admitted - 1) * sd /sqrt(n_admitted),
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death))
  )

cohort_size_pod <- 
  read_csv(file = "X:/R2090/2021-0312 Deaths at home/outputs/cohort_size_pod.csv") %>%
  mutate(
    val_cohort_year = factor(val_cohort_year),
    cat_place_of_death = factor(cat_place_of_death, levels = unique(cat_place_of_death))
  )

los_cohort_pod_nonattenders <-
  los_cohort_pod %>%
  left_join(cohort_size_pod, by = c("val_cohort_year","cat_place_of_death")) %>%
  mutate(
    n_nonusers = n-n_admitted,
    prop_nonusers = 1-prop_admitted
  )


# Define theme settings ---------------------------------------------------

# extrafont::loadfonts(device = "win")


theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  axis.title.y = element_text(margin = margin(0, 20, 0, 0)),  # remove gap to the left of y axis title and below the x axis title
                  axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                  # text=element_text(family="Calibri"),
                  axis.text = element_text(size = 9),
                  # legend.title = element_blank(), 
                  legend.position = "bottom",
                  axis.text.x = element_text(angle=60, hjust=1)
            ))

# Plot --------------------------------------------------------------------

fig_nonattenders_cohort_pod_n <-
  los_cohort_pod_nonattenders %>%
  ggplot(aes(x=val_cohort_year, y=n_nonusers, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Number of people who died",
    x = NULL, y = NULL
  ) +
  scale_colour_viridis_d() +
  NULL

fig_nonattenders_cohort_pod_prop <-
  los_cohort_pod_nonattenders %>%
  ggplot(aes(x=val_cohort_year, y=prop_nonusers, colour = cat_place_of_death, group = cat_place_of_death)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = ~scales::percent(.x, accuracy=1)) +
  labs(
    title = "Proportion of all deaths",
    x = NULL, y = NULL
  ) +
  scale_colour_viridis_d() +
  NULL

(fig_nonattenders_cohort_pod <- 
  wrap_plots(
    fig_nonattenders_cohort_pod_n + fig_nonattenders_cohort_pod_prop,
    guides = "collect"
  ) +
  plot_annotation(
    title = str_wrap("People without hospital admissions in their last 12 months of life", width = 60)
    ) &
  labs(group = "Place of death", colour = "Place of death")
)

fig_nonattenders_cohort_pod %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/data_insights/fig_hospital_nonattenders_cohort_pod.png", width = 15, height = 10, units = "cm", dpi = 300)

