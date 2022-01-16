#' Convert all fields to factors (same method as under plots)
#'
#' @param df dataframe to evaluate
#' @param dv dependent variable to use (column name)
#' @param split_on variable to split data / group by
#' @param id field to use as ID
#' @param n_cat for categorical variables, the max number of unique values
#' to keep. This field feeds the \code{forcats::fct_lump(n = )} argument.
#' @param collapse_by should \code{n_cat} collapse by the distance to the grand
#' mean \code{"dv"} leaving the extremes as is and grouping factors closer to the
#' grand mean as "other" or should it use size \code{"n"}
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
#' @importFrom dplyr rename vars filter pull enquo mutate select one_of row_number mutate_if bind_cols
#' @importFrom lubridate is.Date is.POSIXct
#' @importFrom glue glue
#'
#' @export
#' @examples
#' refactor_columns(df = iris, dv = Sepal.Length)
refactor_columns <- function(df,
                             dv,
                             split_on = NA_character_,
                             id = NULL,
                             n_cat = 10,
                             collapse_by = c("dv", "n"),
                             n_quantile = 10,
                             n_digits = 2,
                             avg_type = c("mean", "median"),
                             ignore_cols = NULL) {

  avg_name <- match.arg(avg_type)
  collapse_by <- match.arg(collapse_by)

  avg <- eval(parse(text = avg_name))
  if (missing(dv)) {
    dv <- "1"
  }

  if (missing(split_on)) {
    split_on <- "1"
    split_name <- ""
  }

  # add ID
  if (missing(id)) {
    df$unique_id <- seq_len(nrow(df))
  } else {
    not_unique <-
      df %>%
      pull({{id}}) %>%
      duplicated()

    if (any(not_unique)) {
      stop(
        glue(
          "the id field '{field}' is not unique",
          field = deparse(substitute(id))
        )
      )
    }
    if (!"unique_id" %in% names(df)) {
      df <- df %>% rename(unique_id = {{id}})
    }
  }

  if (!missing(ignore_cols)) {
    df <- select(df, -c(!!enquo(ignore_cols)))
  }


  # create standard cols
  keep_cols <-
    as_tibble(df) %>%
    mutate(
      y_outcome = {{dv}},
      y_split = {{split_on}},
      .keep = "unused"
    ) %>%
    select(
      .data$y_outcome,
      .data$y_split,
      .data$unique_id,
      everything()
    )

  # wt for collapsing
  if (collapse_by == "dv") {
    wt <- abs(keep_cols$y_outcome - avg(keep_cols$y_outcome))
  } else (
    wt <- NULL
  )

  clean_cols <-
    keep_cols %>%
    select(-c(1:3)) %>%
    mutate_if(~(is.Date(.x) | is.POSIXct(.x)), as.numeric) %>%
    mutate_if(
      ~check_cut_numeric(.x, n_quantile),
      cut_custom, n_quantile, n_digits
    ) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, collapse_cat, n = n_cat, w = wt)


  keep_cols %>%
    select(1:3) %>%
    bind_cols(clean_cols) %>%
    mutate_if(is.logical, as.integer)
}
