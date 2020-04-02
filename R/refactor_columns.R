#' Convert data to a table of factors
#'
#' @inheritParams variation_plot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull mutate select one_of row_number mutate_if bind_cols
#' @importFrom lubridate is.Date is.POSIXct
#'
#' @export
#' @family manipulation functions
refactor_columns <- function(df,
                             dep_var,
                             n_cat = 10,
                             n_quantile = 10,
                             avg_type = c("mean", "median"),
                             ignore_cols = NA_character_) {
  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

  avg <- eval(parse(text = avg_name))

  dv_name <-
    dep_var %>%
    gsub(pattern = " .*", replacement = "") %>%
    gsub(pattern = "\\(", replacement = "")


  suppressWarnings(
    keep_cols <-
      as_tibble(df) %>%
      mutate(
        y_outcome = eval(parse(text = dep_var)),
        y_id = row_number()
      ) %>%
      select(
        .data$y_outcome,
        .data$y_id,
        everything(),
        -one_of(c(ignore_cols, dv_name))
      )
  )

  keep_cols %>%
    select(-c(1:2)) %>%
    mutate_if(~(is.Date(.) | is.POSIXct(.)), as.numeric) %>%
    mutate_if(~check_cut_numeric(., n_quantile), cut_custom, n_quantile) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, collapse_cat, n = n_cat) %>%
    bind_cols(select(keep_cols, c(1:2)), .) %>%
    mutate_if(is.logical, as.integer)
}
