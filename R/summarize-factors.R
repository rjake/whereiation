#' Calculate the importance of ALL factors in a ALL fields
#'
#' Pivots data and summarizes factor frequencies by field and generates stats
#' used for plotting
#'
#' @param return option to return a dataframe when TRUE and a list when FALSE.
#' The list option includes the original min/max of the data and the
#' grand average.
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#'
#' @importFrom dplyr filter n_distinct select select_if mutate group_by summarise ungroup row_number
#' @importFrom broom glance
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
#' @importFrom scales rescale_mid
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' summarize_factors_all_fields(iris, dv = Sepal.Length)
#' summarize_factors_all_fields(iris, dv = Sepal.Length, return = "list")
summarize_factors_all_fields <- function(df,
                         ...,
                         avg_type = c("mean", "median"),
                         return = c("data", "list")
                         ) {
  base_data <-
    refactor_columns(df, ...) %>%
    filter(!is.na(.data$y_outcome))

  # get average method to use for of DV
  avg_name <- match.arg(avg_type)

  avg <- eval(parse(text = avg_name))
  grand_avg <- avg(base_data$y_outcome)

  # range of original DV
  orig_min <- quantile(base_data$y_outcome, 0.02)
  orig_max <- quantile(base_data$y_outcome, 0.98)

  if (orig_min == orig_max && orig_min == 0) {
    orig_max <- max(base_data$y_outcome)
  }

  # find fields to use
  get_vars <-
    base_data %>%
    select(-c(1:2)) %>%
    select_if(function(x) n_distinct(x) > 1) %>%
    names()

  # map_dfr all fields
  factor_stats <-
    map_dfr(get_vars, summarize_factors_one_field, df = base_data, avg_fn = avg)

  field_stats <- map_dfr(get_vars, summarize_one_field, df = base_data)

  # restretch group averages values
  agg_data <-
    factor_stats %>%
    filter(!is.na(.data$value)) %>%
    left_join(field_stats) %>%
    mutate(
      rescale_factor_avg = rescale_mid(
        x = .data$factor_avg,
        to = c(orig_min, orig_max),
        mid = grand_avg
      ),
      grand_avg = grand_avg
    ) %>%
    group_by(.data$field) %>%
    filter(max(row_number()) > 1) %>%
    ungroup() %>%
    mutate(
      field_wt = abs(.data$field_r_sq_adj),
      field = fct_reorder(.data$field, .data$field_wt, .fun = max, .desc = TRUE)
    )

  # return the either a dataframe or a list
  x_is <- match.arg(return)

  if (x_is == "data") {
    x <- agg_data
  } else {
    x <-
      list(
        data = agg_data,
        field_stats = field_stats,
        factor_stats = factor_stats,
        grand_avg = grand_avg,
        orig_min = orig_min,
        orig_max = orig_max
      )
  }

  x
}


#' Calculate the importance of each FACTOR in a given field
#'
#' @param var variable/field to group by
#' @param df refactored data
#' @param avg_fn mean or median
#'
#' @importFrom dplyr select mutate group_by summarise n ungroup filter everything
#'
#' @noRd
#' @examples
#' # summarize_factors_one_field(
#' #   var = "Sepal.Length",
#' #   df = refactor_columns(iris, dv = Sepal.Width),
#' #   avg_fn = mean
#' # )
summarize_factors_one_field <- function(var, df, avg_fn) {
  df %>%
    select(value = var, .data$y_outcome) %>%
    mutate(value = as.character(.data$value)) %>%
    group_by(.data$value) %>%
    summarise(
      factor_avg = avg_fn(.data$y_outcome),
      n = n()
    ) %>%
    ungroup() %>%
    mutate(field = var) %>%
    filter(.data$n > 5) %>%
    select(.data$field, everything())
}


#' Calculate the importance of the FIELD
#'
#' @param var variable/field to group by
#' @param df refactored data
#'
#' @importFrom dplyr select do mutate
#' @importFrom broom glance
#' @importFrom stats lm
#'
#' @noRd
#' @examples
#' # summarize_one_field("Sepal.Length", refactor_columns(iris, Sepal.Width))
summarize_one_field <- function(var, df) {
  df %>%
    select(value = var, .data$y_outcome) %>%
    do(glance(lm(.data$y_outcome ~ .data$value, data = .))) %>%
    mutate(field = var) %>%
    select(
      .data$field,
      field_r_sq = .data$r.squared,
      field_r_sq_adj = .data$adj.r.squared,
      field_p_value = .data$p.value
    )
}
