#' Generate stats for one observation at the factor level
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
#' isolate_record(df = iris, dv = Sepal.Length)
isolate_record <- function(df, dv, id = 1,...) {

  base_data <- refactor_columns(df, dv = {{dv}}, ...)
  group_stats <- summarize_factors_all_fields(df, dv = {{dv}}, ...)

  base_data %>%
    filter(.data$unique_id == id) %>%
    gather(key = "field", value = "value", -c(1, 2, 3)) %>%
    mutate(value = as.character(.data$value)) %>%
    left_join(group_stats, by = c("field", "value"))
}
