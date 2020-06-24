#' Convert data to a table of factors
#' @param split_on variable to split data / group by
#' @inheritParams variation_plot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter pull mutate select one_of row_number mutate_if bind_cols
#' @importFrom lubridate is.Date is.POSIXct
#'
#' @export
#' @family manipulation functions
#' @examples
#' refactor_columns(df = iris, dep_var = "Sepal.Length")
refactor_columns <- function(df,
                             dep_var,
                             split_on = NA_character_,
                             n_cat = 10,
                             n_quantile = 10,
                             n_digits = 2,
                             avg_type = c("mean", "median"),
                             ignore_cols = NA_character_) {
  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

  if (missing(dep_var)) {
    dep_var <- "1"
  }

  if (missing(split_on)) {
    split_on <- "1"
    split_name <- ""
  } else {
    split_name <- extract_field_name(split_on)
  }

  avg <- eval(parse(text = avg_name))

  dv_name <- extract_field_name(dep_var)

  suppressWarnings(
    keep_cols <-
      as_tibble(df) %>%
      mutate(
        y_outcome = eval(parse(text = dep_var)),
        y_id = row_number(),
        y_split = eval(parse(text = split_on))
      ) %>%
      select(
        .data$y_outcome,
        .data$y_id,
        .data$y_split,
        everything(),
        -one_of(c(ignore_cols, dv_name, split_name))
      )
  )

  keep_cols %>%
    select(-c(1:3)) %>%
    mutate_if(~(is.Date(.) | is.POSIXct(.)), as.numeric) %>%
    mutate_if(
      ~check_cut_numeric(., n_quantile),
      cut_custom, n_quantile, n_digits
    ) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, collapse_cat, n = n_cat) %>%
    bind_cols(select(keep_cols, c(1:3)), .) %>%
    mutate_if(is.logical, as.integer)
}
