#' Identify the factors with the most extreme averages for each field
#'
#' @inheritDotParams refactor_columns
#'
#' @export
#'
#' @importFrom dplyr group_by arrange slice mutate row_number ungroup desc select
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' summarize_factor_extremes(df = ggplot2::mpg, dep_var = "hwy")
summarize_factor_extremes <- function(...) {
  # prep_df <- summarize_factors_all_fields(df = mpg, dep_var = "hwy")

  prep_df <- summarize_factors_all_fields(...)

  grand_avg <- prep_df$grand_avg[1]

  factor_stats <-
    prep_df %>%
    group_by(.data$field) %>%
    arrange(.data$factor_avg) %>%
    slice(c(1, n())) %>%
    mutate(position = ifelse(row_number() == 1, "low", "high")) %>%
    ungroup() %>%
    arrange(desc(abs(.data$field_r_sq_adj))) %>%
    #mutate(rank = row_number()) %>%
    select(
      .data$field, .data$value, .data$n, .data$position,
      avg = .data$factor_avg,
      adj_r_sq = .data$field_r_sq_adj,
      p_value = .data$field_p_value
    )

  factor_stats %>%
    pivot_wider(
      names_from = .data$position,
      values_from = c(.data$value, .data$avg, .data$n)
    ) %>%
    select(
      .data$adj_r_sq, .data$p_value,
      .data$n_low, .data$value_low, .data$avg_low,
      .data$field,
      .data$avg_high, .data$value_high, .data$n_high
    )
}

