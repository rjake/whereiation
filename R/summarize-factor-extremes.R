#' Identify the factors with the most extreme averages for each field
#'
#' @inheritDotParams refactor_columns
#'
#' @export
#'
#' @importFrom rlang inform
#' @importFrom dplyr group_by arrange slice mutate row_number ungroup desc select
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' summarize_factor_extremes(df = ggplot2::mpg, dv = hwy)
summarize_factor_extremes <- function(...) {
  # prep_df <- summarize_factors_all_fields(df = mpg, dv = hwy)

  prep_df <- summarize_factors_all_fields(...)

  inform(
    paste(
      "exploring:", attr(prep_df, "about")$dv,
      "\np-values from", prep_df$method[1])
  )

  factor_stats <-
    prep_df %>%
    group_by(.data$field) %>%
    arrange(.data$factor_avg) %>%
    slice(c(1, n())) %>%
    mutate(position = ifelse(row_number() == 1, "low", "high")) %>%
    ungroup() %>%
    arrange(.data$field_p_value) %>%
    #mutate(rank = row_number()) %>%
    select(
      .data$field, .data$value, .data$n, .data$position,
      avg = .data$factor_avg,
      p_value = .data$field_p_value
    )

  factor_stats %>%
    pivot_wider(
      names_from = .data$position,
      values_from = c(.data$value, .data$avg, .data$n)
    ) %>%
    select(
      .data$p_value,
      .data$field,

      lowest_avg = .data$avg_low,
      highest_avg = .data$avg_high,

      lowest_factor = .data$value_low,
      lowest_n = .data$n_low,

      highest_factor = .data$value_high,
      highest_n = .data$n_high
    )
}
