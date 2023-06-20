## FUNCTIONS.R
## This includes various functions for use in the post-processing of the deaths at home data, after they've been exported from the Safe Haven

require(tidyverse)


# Constants ---------------------------------------------------------------

## For ordering factors etc.
order_place_of_death <-
  c("Hospital", "Home & non-institution", "Care home & other")
order_ur8 <-
  c(
    "Large Urban Areas",
    "Other Urban Areas",
    "Accessible Small Towns",
    "Remote Small Towns",
    "Very Remote Small Towns",
    "Accessible Rural Areas",
    "Remote Rural Areas",
    "Very Remote Rural Areas",
    "Missing"
  )


# Theme settings ----------------------------------------------------------

theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  axis.title.y = element_text(margin = margin(0, 20, 0, 0)),  # remove gap to the left of y axis title and below the x axis title
                  axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                  axis.text = element_text(size = 9),
                  legend.title = element_blank(),
                  legend.position = "bottom",
                  axis.text.x = element_text(angle=60, hjust=1)
            ))


# Print all ---------------------------------------------------------------

print_all <- function(x) print(x, n = nrow(x))


# Parse N (%) notation ----------------------------------------------------

## Numbers & proportions are given in the same cell for ease of use
## They can be interpreted using regex, e.g. using tidyr::extract()
## This function takes a data.frame/tibble and a specified column and extracts
## the N and proportion from a single column, removing the column in the process

parse_n_prop <- function(data_tbl, col_n_prop, remove = TRUE) {
  quo_col_n_prop <- enquo(col_n_prop)
  data_tbl %>%
    tidyr::extract(
      data = ., 
      col = !!quo_col_n_prop, 
      into = c("n","prop"), 
      regex = "(\\d*\\,{0,1}\\d+) \\((.*)\\%{0,1}\\)", 
      remove = remove) %>%
    dplyr::mutate(n = parse_number(n), prop = parse_number(prop) / 100)
}

# Parse M (CI) notation ---------------------------------------------------

## Like the above function for N and proportions, means and CIs are given in the
## same cell for ease of use They can be interpreted using regex, e.g. using
## tidyr::extract() This function takes a data.frame/tibble and a specified
## column and extracts the M and CI from a single column, removing the column in
## the process

parse_m_ci <- function(data_tbl, col_m_ci, remove = TRUE) {
  quo_col_m_ci <- enquo(col_m_ci)
  data_tbl %>%
    tidyr::extract(
      data = ., 
      col = !!quo_col_m_ci, 
      into = c("m","ci_lo","ci_hi"), 
      regex = "(\\d[\\d.,]*\\b) \\[(\\d[\\d.,]*), (\\d[\\d.,]*)\\]", 
      remove = remove) %>%
    dplyr::mutate(across(c(m,ci_lo,ci_hi), parse_number))
}


# Convert annual columns to long format -----------------------------------

pivot_annual_to_long <- function(data_tbl, new_col = "n_prop") {
  data_tbl %>%
    tidyr::pivot_longer(
      cols = matches("\\d{4}\\-\\d{2}"),
      names_to = "val_cohort_year",
      values_to = new_col, 
      values_drop_na = TRUE  # remove blank columns, e.g. for Covid-19 before pandemic period
    )
}


# Calculate N (%) from n and prop -----------------------------------------

calculate_n_prop <- function(n, prop, accuracy = 0.1) {
  if_else(
    ## replace with NA where there are 0s
    condition = is.na(n)|is.na(prop),
    true = NA_character_,
    false = 
      paste0(scales::comma(x = n),  # thousands separator
             " (",
             scales::percent(x = prop, accuracy = accuracy),  # pretty percent
             ")"
    )
  )
}



# Replace percentage with 100% --------------------------------------------

## This is a daft helper that finds a percentage figure and replaces with 100%
replace_percent_with_100 <- function(some_string) {
  str_replace(string = some_string,
              pattern = "\\d+\\.{0,1}\\d*\\%",
              replacement = "100%")
}


# Repeat data and change variable to 'All' or similar ---------------------

repeat_data_adding_a_catchall_category <-
  function(data_tbl,
           var_to_change,
           label_for_catchall = "All") {
    if (is.factor(data_tbl %>% pull({{var_to_change}}))) {
      old_levels <- data_tbl %>% pull({{var_to_change}}) %>% levels
    } else {
      old_levels <- data_tbl %>% pull({{var_to_change}}) %>% unique
    }
    new_levels <- c(old_levels, label_for_catchall)
    bind_rows(
      data_tbl,
      data_tbl %>% mutate({{var_to_change}} := label_for_catchall)
    ) %>%
      mutate(
        {{var_to_change}} := factor({{var_to_change}}, levels = new_levels)
      )
  }


# Recalculate mean, sd and confidence intervals for users only ------------

## helper function to recalculate descriptive statistics for usage data that had
## been calculated including non-users (e.g. individuals with zero
## hospitalisations or zero hospital length of stay).To do this I use a separate
## table of 'denominators' - i.e. the total number of individuals in the cohort,
## to adjust the mean, SD, and confidence intervals
## note: this expects an 'n_users' column in data_tbl
recalculate_mean_for_users_only <- function(data_tbl, denominator_tbl) {
  joining_columns <- intersect(
    names(data_tbl),
    names(denominator_tbl %>% select(-any_of("n")))
  )
  data_tbl %>%
    left_join(
      denominator_tbl %>% rename(n_cohort = n),
      by = joining_columns
    ) %>%
    mutate(
      m_users = m * n_cohort / n_users,
      sd_users = sd * sqrt(n_cohort / n_users),
      ci_lo_users = m_users + qt(p = 0.025, df = n_users - 1) * sd_users/sqrt(n_users),
      ci_hi_users = m_users + qt(p = 0.975, df = n_users - 1) * sd_users/sqrt(n_users)
    )
}


# Recalculate SD from mean, confidence interval, and N --------------------

# Because I know how I calculated the confidence interval, I can
# 'back-calculate' the standard deviation from the confidence interval, mean,
# and number
# e.g. 
# ci_lo_users = m_users + qt(p = 0.025, df = n_users - 1) * sd_users/sqrt(n_users)

recalculate_sd_from_mean_ci_and_n <- function(data_tbl, mean_var, ci_lo_var, n_var, new_sd_var) {
  data_tbl %>%
    mutate(
      {{new_sd_var}} :=
        sqrt({{n_var}}) * ({{ci_lo_var}} - {{mean_var}}) / qt(p = 0.025, df = {{n_var}} - 1)
    )
}

