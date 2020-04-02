#' Summarize fields
#'
#' Pivots data and summarizes factor frequencies by field and generates stats
#' used for plotting
#'
#' @param return option to return a dataframe when TRUE and a list when FALSE.
#' The list option includes the original min/max of the data and the
#' grand average.
#'
#' @inheritDotParams variation_plot
#' @inheritParams variation_plot
#'
#' @importFrom dplyr select starts_with select_if mutate group_by summarise n ungroup row_number filter do rename
#' @importFrom broom glance
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
#' @importFrom stats sd lm
#' @importFrom rlang .data
#'
#' @export
#' @family manipulation functions
summarize_factors <- function(df,
                              ...,
                              avg_type = c("mean", "median"),
                              return = c("data", "list")
                              ) {
  base_data <-
    refactor_columns(df, ...) %>%
    filter(!is.na(.data$y_outcome))

  # get average of DV
  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

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
  get_fields <- map_dfr(get_vars, agg_fields, base_data, avg_fn = avg)
  field_stats <- map_dfr(seq_along(get_vars), get_stats)

  agg_data <-
    get_fields %>%
    filter(!is.na(.data$value)) %>%
    left_join(field_stats) %>%
    mutate(
      group_avg = change_range(.data$group_avg, orig_min, orig_max),
      grand_avg = grand_avg#,
      #group_dist = .data$group_avg - .data$grand_avg
      # value_diff = group_avg - grand_avg,
      # abs_value_diff = abs(value_diff)
    ) %>%
    group_by(.data$field) %>%
    filter(max(row_number()) > 1) %>%
    # mutate(
      # field_variance = var(group_avg),
      # extreme_group = max(abs(group_avg)),
      # field_range = max(.data$group_avg) - min(.data$group_avg)
    # ) %>%
    ungroup() %>%
    mutate(
      field_wt = abs(.data$field_r_sq_adj),
      field = fct_reorder(.data$field, .data$field_wt, .fun = max, .desc = TRUE)
      #field_wt_old = .data$field_range / max(.data$field_range),
    )

  if (missing(return)) {
    x_is <- "data"
  } else {
    x_is <- match.arg(return)
  }

  if (x_is == "data") {
    x <- agg_data
  } else {
    x <-
      list(
        data = agg_data,
        field_stats = field_stats,
        value_stats = get_fields,
        grand_avg = grand_avg,
        orig_min = orig_min,
        orig_max = orig_max
      )
  }

  x
}





#' Title
#'
#' @param df refactored data
#' @param var variable/field to group by
#' @param avg_fn mean or median
#'
#' @import dplyr select mutate group_by summarise n ungroup filter everything
#'
#' @examples
#' agg_fields("Sepal.Length", refactor_columns(iris, "Sepal.Width"), "mean")
agg_fields <- function(var, df, avg_fn) {
  df %>%
    select(value = var, .data$y_outcome) %>%
    mutate(value = as.character(.data$value)) %>%
    # factor avg
    group_by(.data$value) %>%
    summarise(
      group_avg = avg_fn(.data$y_outcome),
      n = n()
    ) %>%
    ungroup() %>%
    mutate(field = var) %>%
    filter(.data$n > 5) %>%
    select(field, everything())
}


get_stats <- function(df, var) {
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
