#' Convert data to a table of factors
#'
#' @param df dataframe to evaluate
#' @param dep_var dependent variable to use (column name)
#' @param id field to use as ID
#' @param n_cat for categorical variables, the max number of unique values
#' to keep. This field feeds the \code{forcats::fct_lump(n = )} argument.
#' @param n_quantile for numeric/date fields, the number of quantiles used
#' to split the data into a factor. Fields that have less than this amount
#' will not be changed.
#' @param n_digits for numeric fields, the number of digits to keep in the breaks
#' ex: [1.2345 to 2.3456] will be [1.23 to 2.34] if \code{n_digits = 2}
#' @param avg_type mean or median
#' @param ignore_cols columns to ignore from analysis. Good candidates are
#' fields that have have no duplicate values (primary keys) or fields with
#' a large proportion of null values
#'
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename_at vars filter pull mutate select one_of row_number mutate_if bind_cols
#' @importFrom lubridate is.Date is.POSIXct
#' @importFrom glue glue
#'
#' @export
#' @family manipulation functions
#' @examples
#' refactor_columns(df = iris, dep_var = "Sepal.Length")
refactor_columns <- function(df,
                             dep_var,
                             id = NULL,
                             n_cat = 10,
                             n_quantile = 10,
                             n_digits = 2,
                             avg_type = c("mean", "median"),
                             ignore_cols = NA_character_) {
  avg_name <- match.arg(avg_type)

  avg <- eval(parse(text = avg_name))

  dv_name <-
    dep_var %>%
    gsub(pattern = " .*", replacement = "") %>%
    gsub(pattern = "\\(", replacement = "")

  # add ID
  if (is.null(id)) {
    df$unique_id <- seq_len(nrow(df))
  } else {
    if (any(duplicated(df[[id]]))) {
      stop(glue("the id field '{id}' is not unique"))
    }
    if (!"unique_id" %in% names(df)) {
      df <- df %>% rename_at(vars(id), ~"unique_id")
    }
  }

  # create standard cols
  suppressWarnings(
    keep_cols <-
      as_tibble(df) %>%
      mutate(y_outcome = eval(parse(text = dep_var))) %>%
      select(
        .data$y_outcome,
        .data$unique_id,
        everything(),
        -one_of(c(ignore_cols, dv_name))
      )
  )

  keep_cols %>%
    select(-c(1:2)) %>%
    mutate_if(~(is.Date(.) | is.POSIXct(.)), as.numeric) %>%
    mutate_if(
      ~check_cut_numeric(., n_quantile),
      cut_custom, n_quantile, n_digits
    ) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, collapse_cat, n = n_cat) %>%
    bind_cols(select(keep_cols, c(1:2)), .) %>%
    mutate_if(is.logical, as.integer)
}
