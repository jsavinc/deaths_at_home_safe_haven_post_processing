# Script to plot demographics changes of entire cohort over time for presentation

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


# Individual variables ----------------------------------------------------


age <-
  demographics %>% 
  slice(1) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  mutate(val_cohort_year = factor(val_cohort_year)) %>%
  extract(col = value, into = c("median", "q25", "q75"), regex = "(\\d+) \\((\\d+)\\, (\\d+)\\)", remove = TRUE, convert = TRUE)

sex <-
  demographics %>% 
  slice(3:4) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  parse_num_prop(num_prop = "value") %>%
  rename(cat_sex = Characteristic) %>%
  mutate(cat_sex = factor(cat_sex, levels = c("Male", "Female")))

pod <-
  demographics %>% 
  slice(10:13) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  parse_num_prop(num_prop = "value") %>%
  mutate(cat_place_of_death = factor(Characteristic, levels = unique(Characteristic))) %>%
  select(-Characteristic)

marstat <-
  demographics %>% 
  slice(76:79) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  parse_num_prop(num_prop = "value") %>%
  mutate(cat_marital_status = factor(Characteristic, levels = unique(Characteristic))) %>%
  select(-Characteristic)

simd <- 
  demographics %>%
  slice(15:24) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  parse_num_prop(num_prop = "value") %>%
  mutate(val_cohort_year = factor(val_cohort_year)) %>%
  rename(val_simd = Characteristic) %>%
  mutate(val_simd = factor(as.integer(val_simd)))

simd_quintile <- 
  demographics %>%
  slice(27:31) %>%
  pivot_longer(cols = 2:7, names_to = "val_cohort_year", names_transform = ~str_extract(.x, pattern = "\\d{4}\\-\\d{2}"), values_to = "value") %>%
  parse_num_prop(num_prop = "value") %>%
  mutate(val_cohort_year = factor(val_cohort_year)) %>%
  rename(val_simd_quintile = Characteristic) %>%
  mutate(val_simd_quintile = factor(as.integer(val_simd_quintile)))


# Individual figures ------------------------------------------------------

fig_age <-
  ggplot(age, aes(x=val_cohort_year, y=median, group=1)) +
  geom_line() +
  geom_line(aes(y=q25), linetype = "dashed")+
  geom_line(aes(y=q75), linetype = "dashed")+
  theme_minimal() +
  theme(
    legend.title = element_blank(), legend.position = "top",
    axis.text.x = element_text(angle=60, hjust=1)
  ) +
  labs(
    x=NULL, y=NULL, title = "Age", subtitle = "Median age with IQR (dashed)"
  ) +
  NULL

fig_sex <-
  wrap_plots(
    ggplot(sex, aes(x=val_cohort_year, y=n, group=cat_sex, colour=cat_sex)) +
      geom_line() +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
        ) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~format(.x, big.mark = ",")) +
      labs(
        x=NULL, y=NULL, title = "Sex"
      ) +
      NULL
    ,
    ggplot(sex %>% filter(cat_sex=="Female"), aes(x=val_cohort_year, y=prop, group=1)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      scale_y_continuous(labels = ~scales::percent(.x, accuracy = 0.1), limits = c(0.48, 0.52)) +
      labs(
        x=NULL, y=NULL, subtitle = "Proportion female (%)"
      ) +
      NULL
  )
  
fig_pod <-
  wrap_plots(
    ggplot(pod, aes(x=val_cohort_year, y=n, group=cat_place_of_death, colour=cat_place_of_death)) +
      geom_line() +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~format(.x, big.mark = ",")) +
      labs(
        x=NULL, y=NULL, title = "Place of death"
      ) +
      NULL
    ,
    ggplot(pod, aes(x=val_cohort_year, y=prop, group=cat_place_of_death, colour=cat_place_of_death)) +
      geom_line(show.legend = FALSE) +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~scales::percent(.x, accuracy = 0.1)) +
      labs(
        x=NULL, y=NULL, subtitle = "Proportion (%)"
      ) +
      NULL
  )

  
fig_marstat <-
  wrap_plots(
    ggplot(marstat, aes(x=val_cohort_year, y=n, group=cat_marital_status, colour=cat_marital_status)) +
      geom_line() +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~format(.x, big.mark = ",")) +
      labs(
        x=NULL, y=NULL, title = "Marital status"
      ) +
      NULL
    ,
    ggplot(marstat, aes(x=val_cohort_year, y=prop, group=cat_marital_status, colour=cat_marital_status)) +
      geom_line(show.legend = FALSE) +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~scales::percent(.x, accuracy = 0.1)) +
      labs(
        x=NULL, y=NULL, subtitle = "Proportion (%)"
      ) +
      NULL
  )

fig_simd_quintile <-
  wrap_plots(
    ggplot(simd_quintile, aes(x=val_cohort_year, y=n, group=val_simd_quintile, colour=val_simd_quintile)) +
      geom_line() +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      # guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~format(.x, big.mark = ",")) +
      labs(
        x=NULL, y=NULL, title = "SIMD quintile (1=most deprived)"
      ) +
      NULL
    ,
    ggplot(simd_quintile, aes(x=val_cohort_year, y=prop, group=val_simd_quintile, colour=val_simd_quintile)) +
      geom_line(show.legend = FALSE) +
      theme_minimal() +
      theme(
        legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=60, hjust=1)
      ) +
      scale_colour_viridis_d() +
      scale_y_continuous(labels = ~scales::percent(.x, accuracy = 0.1)) +
      labs(
        x=NULL, y=NULL, subtitle = "Proportion (%)"
      ) +
      NULL
  )

fig_simd_quintile %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_simd_quintile_cohort.png", width = 12, height = 7, units = "cm", dpi = 300)

# Combined figure ---------------------------------------------------------


fig_combined <-
  wrap_plots(
    # list(fig_age, fig_sex, fig_pod, fig_marstat) %>% map(., ~.x+theme(plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm")))
    list(fig_age, fig_sex, fig_pod, fig_marstat)
  )

fig_combined %>%
  ggsave(plot = ., filename = "X:/R2090/2021-0312 Deaths at home/outputs/fig_demographics_cohort_combined.png", width = 25, height = 14, units = "cm", dpi = 300)
