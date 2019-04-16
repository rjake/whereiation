library(tidyverse)

my_lib <- installed.packages()


rdatasets <-
  read.csv(
    "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/datasets.csv"
  ) %>%
  rename_all(tolower) %>% 
  filter(
    package %in% my_lib,
    cols > 2,
    rows > 100
  )


scan_dataset <- function(data, package) {
  df <- get(data(list = data, package = package))
  
  tibble(
    package = package,
    data = data,
    n_row = nrow(df),
    n_char = sum(sapply(df, function(x) class(x) %in% c("character", "factor"))),
    n_numeric = sum(sapply(df, function(x) class(x) %in% c("numeric", "integer"))),
    n_logical = sum(sapply(df, function(x) class(x) %in% c("logical"))),
    n_date = sum(sapply(df, function(x) class(x) %in% c("date")))
  )
}


scan_dataset("iris", "datasets")


scan_all <-
  map2_dfr(rdatasets$item, rdatasets$package, scan_dataset)




