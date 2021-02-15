#' Visualize difference from avg. by factor (lollipop)
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_vline geom_segment geom_point facet_wrap scale_alpha theme element_rect labs
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' plot_deltas(df = ggplot2::mpg, dep_var = "hwy")
plot_deltas <- function(df,
                        dep_var,
                        ...,
                        avg_type = c("mean", "median")
                        ) {
  avg_name <- match.arg(avg_type)

  factor_stats <-
    summarize_factors_all_fields(
      df = df,
      dep_var = dep_var,
      ...
    )

  # <- factor_stats$grand_avg[1]

  factor_stats %>%
    mutate(
      field = fct_reorder(.data$field, .data$field_wt, .fun = max, .desc = TRUE),
      delta = abs(.data$factor_avg - .data$grand_avg),
      color = ifelse(.data$factor_avg > .data$grand_avg, "above", "below")
    ) %>%
    ggplot(
      aes(
        x = .data$factor_avg, y = .data$value,
        color = .data$color, alpha = .data$delta
      )
    ) +
    geom_vline(aes(xintercept = .data$grand_avg)) +
    geom_segment(aes(xend = .data$grand_avg, yend = .data$value)) +
    geom_point(aes(size = .data$n)) +
    facet_wrap(~.data$field, scales = "free_y") +
    theme(panel.background = element_rect(fill = "white", color = "grey60")) +
    scale_alpha(range = c(0.2, 1), guide = FALSE) +
    labs(
      title = paste("Difference in", avg_name, dep_var, "from grand", avg_name, "across all factors of all fields"),
      subtitle = paste("Each chart shows the different values (factors) within each field and the", avg_name, dep_var, "for each. Charts with the highest \nadjusted R-square start in the top left. Factors with 5 or fewer observations have been excluded and the vertical line \nis the grand", avg_name, "across all observations"),
      y = NULL,
      x = paste(avg_name, dep_var),
      size = "# of \n observations"
    )
}

