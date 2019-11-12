library(tidyverse)

all_data <- # readRDS("find_datasets.rds")
  read.csv(
    "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/datasets.csv"
  ) %>%
  rename_all(tolower)

installed_data <- installed.packages()

good_datasets <-
  all_data %>%
  filter(
    package %in% installed_data,
    n_row > 50,
    between(n_col, 20, 100),
    n_char > 2,
    n_char != n_col
  )

View(good_datasets)






# find all locally ###########################################################


pkg_data <-
  data(package = .packages(all.available = TRUE))$results %>%
  as_tibble() %>%
  rename_all(tolower) %>%
  select(-libpath)

scan_dataset <- function(data, package) {

  # data = "cd4.nested";package = "boot";
  e <- new.env(
    hash = TRUE,
    parent = parent.frame(),
    size = 29L
  )

  tryCatch({
    scan_df <- (data(list = data, package = package, envir = e))

    df <- e[[data]]

    print(paste(package, data))

    if (any(class(df) %in% "data.frame")) {
      meta <-
        tibble(
          package = package,
          data = data,
          n_row = nrow(df),
          n_col = ncol(df),
          n_char = sum(sapply(df, function(x) class(x)[1] %in% c("character", "factor", "ordered"))),
          n_numeric = sum(sapply(df, function(x) class(x)[1] %in% c("numeric", "integer"))),
          n_logical = sum(sapply(df, function(x) class(x)[1] %in% c("logical"))),
          n_date = sum(sapply(df, function(x) class(x)[1] %in% c("Date", "POSIXct"))),
          n_other = n_col - n_char - n_numeric - n_logical - n_date
        )
    }
  })
}


scan_dataset("CNES", "sem")

system.time(
  scan_all <-
    map2_dfr(pkg_data$item, pkg_data$package, scan_dataset)
)

# saveRDS(scan_all, "find_datasets.rds")
