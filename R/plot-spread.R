#' Visualize spread of avg. values among all factors for all variables
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_vline geom_line geom_point guides theme element_rect labs
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' plot_spread(ggplot2::mpg, dv = hwy)
plot_spread <- function(df,
                        dv,
                        ...
                        ) {
  factor_stats <-
    summarize_factors_all_fields(
      df = df,
      dv = {{dv}},
      ...
    )

  attr_about <- attributes(factor_stats)$about
  avg_name <- attr_about$avg_type
  grand_avg <- attr_about$grand_avg

  dv_name <- deparse(substitute(dv))

  group_value_ranks <-
    factor_stats %>%
    # left_join(field_ranks) %>%
    mutate(
      field = fct_reorder(.data$field, .data$field_r_sq_adj, .fun = max),
      label = paste0(
        toupper(.data$value),
        "\n----------------------------",
        "\n", avg_name, ": ", round(.data$factor_avg, 3),
        "\nN: ", .data$n,
        "\nAdj. R2: ", round(.data$field_r_sq_adj, 3)
      )
    )

#p<-
  group_value_ranks %>%
    ggplot(
      aes(
        x = .data$factor_avg, y = .data$field,
        color = .data$field, fill = .data$field,
        label = .data$label
        )
    ) +
    geom_vline(
      xintercept = grand_avg, color = "grey60", size = 1, linetype = "dotted"
    ) +
    geom_line(size = 6, alpha = 0.2, lineend = "round") +
    geom_point(
      aes(size = .data$n),
      shape = 21, color = "black", stroke = 0.5
    ) +
    guides(color = FALSE, fill = FALSE) +
    theme(panel.background = element_rect(fill = "white", color = "grey60")) +
    labs(
      title = paste0(dv_name, " across all factors of all fields"),
      subtitle = paste0("each point represents a factor in the field along the y axis, factors with 5 or fewer observations have been excluded \nthe dotted line is the grand ", avg_name, " across all observations"),
      y = "Field ranked by adjusted r-squared",
      x = paste0("group ", avg_name, " of the dependent variable for each factor"),
      size = "# of \n observations"
    )
}


#' Visualize variation and logic for a single observation
#'
#' @param labels when TRUE will show the labels of the factor levels outlined in the plot
#' @param isolate_id the unique id from the field specified in
#' \code{unique_id} or the row number when \code{unique_id} is unspecified
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#'
#' @importFrom dplyr filter select
#' @importFrom ggplot2 ggplot geom_vline aes geom_segment geom_point labs geom_text
#' @importFrom rlang .data
#'
#' @export
#' @describeIn plot_spread highlight a single observation
#'
#' @examples
#' plot_spread_single_obs(df = employee_attrition[,1:5], dv = attrition)
#' plot_spread_single_obs(df = employee_attrition[,1:5], dv = attrition, labels = TRUE)
plot_spread_single_obs <- function(df,
                                   dv,
                                   ...,
                                   labels = FALSE,
                                   isolate_id = 1) {

  compare_values <-
    isolate_record(
      df = df,
      dv = {{dv}},
      isolate_id = isolate_id,
      ...
    )

  attr_about <- attributes(compare_values)$about
  avg_name <- attr_about$avg_type
  avg <- attr_about$avg_fn

  dv_name <- deparse(substitute(dv))

  one_obs_profile <-
    compare_values %>%
    select(.data$field, .data$value, .data$factor_avg) %>%
    mutate(label = "")

  plot_orig <-
    plot_spread(df = df, dv = {{dv}}, ...) +
    geom_point(
      data = one_obs_profile,
      color = "black", size = 5, shape = 21, stroke = 2
    ) +
    labs(
      title = paste0(dv_name, " across all factors of all fields", " - single observation")
    )

  if (labels == TRUE) {
    plot_orig <-
      plot_orig +
      geom_text(
        data = one_obs_profile, size = 2.5,
        aes(label = .data$value),
        vjust = 2.5, color = "black"
      )
  }

  plot_orig
}



#' @export
#' @inheritDotParams refactor_columns
#' @describeIn plot_spread utilizing ggplotly
#' @importFrom plotly ggplotly
#' @examples
#' plot_spread_interactive(ggplot2::mpg, dv = hwy)
plot_spread_interactive <- function(...) {
  p <- plot_spread(...)
  ggplotly(p, tooltip = c("label"))
}
