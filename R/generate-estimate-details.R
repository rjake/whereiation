#' Generate stats for each observation at the factor level
#'
#' The dataset returned will be the length of the # of columns x # of rows
#' @param train_data training dataset generated from summarize_factors
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#'
#' @importFrom tidyr gather drop_na
#' @importFrom dplyr mutate left_join filter arrange desc group_by ungroup
#' @importFrom grDevices boxplot.stats
#' @importFrom scales rescale_mid
#' @importFrom stats complete.cases weighted.mean
#' @importFrom rlang .data
#' @noRd
#' @examples
#' generate_estimate_details(df = iris, dv = Sepal.Length)
generate_estimate_details <- function(df, train_data, dv, ...) {

  if (missing(train_data)) {
    base_data <- refactor_columns(df, dv = {{dv}}, ...)
    group_stats <- summarize_factors_all_fields(df, dv = {{dv}}, ..., return = "list")
  } else {
    base_data <- df
    group_stats <- summarize_factors_all_fields(train_data, dv = {{dv}}, ..., return = "list")
  }

  group_stats_data <- group_stats$data
  orig_min <- group_stats$orig_min
  orig_max <- group_stats$orig_max


  base_data %>%
    gather(key = "field", value = "value", -c(1, 2, 3)) %>%
    mutate(value = as.character(.data$value)) %>%
    left_join(group_stats_data, by = c("field", "value")) %>%
    group_by(.data$unique_id) %>%
    mutate(complete = sum(!is.na(.data$factor_avg))) %>%
    ungroup() %>%
    drop_na(.data$factor_avg:.data$field_wt) %>%
    #arrange(desc(.data$field_wt)) %>%
    mutate(factor_avg_wt = .data$factor_avg * .data$field_wt) %>%
    group_by(.data$unique_id) %>%
    mutate(estimate = weighted.mean(.data$factor_avg, .data$field_wt)) %>%
    ungroup() %>%
    mutate(
      rescale_estimate = rescale_mid(
        x = .data$estimate,
        to = c(orig_min, orig_max),
        mid = group_stats$grand_avg
      )
    )
}
