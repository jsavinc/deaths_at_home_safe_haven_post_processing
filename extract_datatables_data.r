# Extract tables from exported .html file that includes 'datatables'

library(tidyverse)
library(rvest)
library(jsonlite)

# Get the JSON that is used to create the table
json_data <-
  read_html("../imgs2022_preliminary_results/imgs2022_preliminary_results.html") %>% 
  html_elements(css = "#age-descriptive-statistics") %>%
  html_elements("script") %>%
  html_text() %>%
  jsonlite::fromJSON()

# Extract data and reshape it
table <-
  json_data$x[["data"]] %>%
  as_tibble() %>%
  t() %>% 
  as_tibble()

# Extract variable names from html stored in the json
variable_names <-
  minimal_html(json_data$x[["container"]]) %>%
  html_table() %>%
  .[[1]] %>%
  colnames()

# Add the correct column names
colnames(table) <- variable_names

table
