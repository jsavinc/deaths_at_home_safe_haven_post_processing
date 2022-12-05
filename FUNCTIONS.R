## FUNCTIONS.R
## This includes various functions for use in the post-processing of the deaths at home data, after they've been exported from the Safe Haven

require(tidyverse)


# Constants ---------------------------------------------------------------

## For ordering factors etc.
order_place_of_death <- c("Hospital", "Home & non-institution", "Care home & other")

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


# Convert annual columns to long format -----------------------------------

pivot_annual_to_long <- function(data_tbl) {
  data_tbl %>%
    tidyr::pivot_longer(cols = matches("\\d{4}\\-\\d{2}"), names_to = "cohort_year", values_to = "n_prop")
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
