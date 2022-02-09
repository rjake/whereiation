#' Calculate the importance of ALL factors in a ALL fields
#'
#' Pivots data and summarizes factor frequencies by field and generates stats
#' used for plotting
#'
#' The list option includes the original min/max of the data and the
#' grand average.
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#'
#' @importFrom dplyr filter n_distinct left_join select select_if mutate group_by summarise ungroup row_number
#' @importFrom broom glance
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
#' @importFrom stats quantile
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' summarize_factors_all_fields(df = iris, dv = Sepal.Length)
#'
#' # similar to other functions, you can see the attributes
#' summarize_factors_all_fields(df = iris, dv = Sepal.Length) %>% attr("about")
summarize_factors_all_fields <- function(df,
                                         ...
                                         ) {
  base_data <-
    refactor_columns(df, ...) %>%
    filter(!is.na(.data$y_outcome))

  # get average method to use for of DV
  attr_about <- attributes(base_data)$about
  avg_name <- attr_about$avg_type
  avg <- attr_about$avg_fn

  # find fields to use
  get_vars <-
    base_data %>%
    select(-c(1:3)) %>%
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
    left_join(field_stats, by = "field") %>%
    mutate(grand_avg = attr_about$grand_avg) %>%
    group_by(.data$field) %>%
    filter(max(row_number()) > 1) %>%
    ungroup() %>%
    mutate(
      field = fct_reorder(
        .f = .data$field,
        .x = abs(.data$field_p_value),
        .fun = min
      )
    )

  attributes(agg_data)$about <- attr_about

  agg_data
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
#' summarize_factors_one_field(
#'   var = "Sepal.Length",
#'   df = refactor_columns(df = iris, dv = Sepal.Width),
#'   avg_fn = mean
#' )
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
#' @importFrom dplyr select summarise transmute
#' @importFrom rstatix chisq_test kruskal_test
#' @importFrom tidyr unnest
#'
#' @noRd
#' @examples
#' summarize_one_field(var = "Sepal.Length", df = refactor_columns(iris, Sepal.Width))
#' summarize_one_field(var = "Species", df = refactor_columns(iris, Sepal.Width > 3))
summarize_one_field <- function(var, df) {
  # if binary use chi-square test
  if (is_binary(df$y_outcome)) {
    stats_df <-
      df %>%
      select(value = var, .data$y_outcome) %>%
      summarise(pval = chisq_test(.data$value, y = .data$y_outcome)) %>%
      unnest(.data$pval) %>%
      suppressWarnings()

  } else {
    # if continuous use kruskal-wallis test
    stats_df <-
      df %>%
      select(value = var, .data$y_outcome) %>%
      kruskal_test(y_outcome ~ value) %>%
      mutate(method = paste(.data$method, "test"))
  }

  stats_df %>%
    transmute(
      field = var,
      field_p_value = .data$p,
      method = .data$method,
      statistic = .data$statistic,
      df = .data$df
    )
}
