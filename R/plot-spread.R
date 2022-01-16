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
                        ...,
                        avg_type = c("mean", "median")
                        ) {
  avg_name <- match.arg(avg_type)
  dv_name <- deparse(substitute(dv))

  factor_stats <-
    summarize_factors_all_fields(
      df = df,
      dv = {{dv}},
      ...
    )

  grand_avg <- factor_stats$grand_avg[1]

  group_value_ranks <-
    factor_stats %>%
    # left_join(field_ranks) %>%
    mutate(
      field = fct_reorder(.data$field, .data$field_wt, .fun = max),
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
#' @param id is id (row number) from \code{base_data()} to use
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#'
#' @importFrom dplyr filter select
#' @importFrom ggplot2 ggplot geom_vline aes geom_segment geom_point labs
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data
#'
#' @export
#' @describeIn plot_spread highlight a single observation
#'
#' @examples
#' plot_spread_single_obs(ggplot2::mpg, dv = hwy)
plot_spread_single_obs <- function(df,
                                   dv,
                                   ...,
                                   avg_type = c("mean", "median"),
                                   labels = FALSE,
                                   id = 1) {

  avg_name <- match.arg(avg_type)
  dv_name <- deparse(substitute(dv))

  avg <- eval(parse(text = avg_name))

  compare_values <-
    generate_estimate_details(
      df = df,
      dv = {{dv}},
      avg_type = avg_name,
      ...
    )

  get_id <- id

  one_obs_profile <-
    compare_values %>%
    filter(.data$unique_id == get_id) %>%
    select(
      .data$field, .data$value, .data$field_wt,
      .data$factor_avg, #.data$group_dist,
      .data$factor_avg_wt,
      .data$estimate
    ) %>%
    mutate(label = "")


  obs_estimate <- one_obs_profile$estimate[1]

  plot_orig <-
    plot_spread(df = df, dv = {{dv}}, ...) +
    geom_vline(xintercept = obs_estimate, size = 1, alpha = .5) +
    geom_segment(
      data = one_obs_profile, xend = obs_estimate,
      aes(yend = .data$field, size = .data$field_wt*10, alpha = .data$field_wt*10),
      color = "black", show.legend = FALSE
    ) +
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
      geom_label_repel(
        data = one_obs_profile, size = 4,
        aes(label = paste0(.data$field, ": ", .data$value)),
        color = "black",
        segment.color = NA,
        hjust = -0.15
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
