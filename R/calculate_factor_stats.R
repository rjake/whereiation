#' Generate stats for each observation at the factor level
#'
#' The dataset returned will be the length of the # of columns x # of rows
#' @param train_data training dataset generated from summarize_factors
#' @inheritDotParams variation_plot
#' @inheritParams variation_plot
#'
#' @importFrom tidyr gather drop_na
#' @importFrom dplyr mutate left_join filter arrange desc group_by ungroup
#' @importFrom grDevices boxplot.stats
#' @importFrom stats complete.cases weighted.mean
#' @importFrom rlang .data
#'
#' @export
#' @family manipulation functions
calculate_factor_stats <- function(df, train_data, dep_var, ...) {

  if (missing(train_data)) {
    base_data <- refactor_columns(df, dep_var = dep_var, ...)
    group_stats <- summarize_factors(base_data, ..., return = "list")
  } else {
    base_data <- df
    group_stats <- summarize_factors(train_data, ..., return = "list")
  }

  group_stats_data <- group_stats$data
  orig_min <- group_stats$orig_min
  orig_max <- group_stats$orig_max


  suppressWarnings(
    base_data %>%
      gather(
        key = field, value = value,
        -c(.data$datascanr_id, .data$datascanr_outcome)
      ) %>%
      mutate(value = as.character(.data$value)) %>%
      left_join(
        group_stats_data, by = c("field", "value")
      ) %>%
      group_by(.data$datascanr_id) %>%
      mutate(complete = sum(!is.na(.data$group_avg))) %>%
      ungroup() %>%
      drop_na(.data$group_avg:.data$field_wt) %>%
      #arrange(desc(.data$field_wt)) %>%
      mutate(group_avg_wt = .data$group_avg * .data$field_wt) %>%
      group_by(.data$datascanr_id) %>%
      mutate(estimate = weighted.mean(.data$group_avg, .data$field_wt)) %>%
      ungroup() %>%
      mutate(estimate = change_range(.data$estimate, orig_min, orig_max))
  )
}
