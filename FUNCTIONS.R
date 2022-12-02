## FUNCTIONS.R
## This includes various functions for use in the post-processing of the deaths at home data, after they've been exported from the Safe Haven

require(magrittr)

# Parse N (%) notation ----------------------------------------------------

## Numbers & proportions are given in the same cell for ease of use
## They can be interpreted using regex, e.g. using tidyr::extract()
## This function takes a data.frame/tibble and a specified column and extracts
## the N and proportion from a single column, removing the column in the process

parse_n_prop <- function(data_tbl, col_n_prop, remove = TRUE) {
  require(tidyverse)
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

