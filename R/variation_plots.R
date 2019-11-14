#' Visualize variation among all factor levels for all variables
#'
#' @param df dataframe to evaluate
#' @param dep_var dependent variable to use (column name)
#' @param avg_type mean or median
#' @param ignore_cols columns to ignore from analysis. Good candidates are
#' fields that have have no duplicate values (primary keys) or fields with
#' a large proportion of null values
#' @param n_cat for categorical variables, the max number of unique values
#' to keep. This field feeds the \code{forcats::fct_lump(n = )} argument.
#' @param n_quantile for numeric/date fields, the number of quantiles used
#' to split the data into a factor. Fields that have less than this amount
#' will not be changed.
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_vline geom_line geom_point guides theme element_rect labs
#' @importFrom rlang .data
#' @examples
#' variation_plot(ggplot2::mpg, "hwy")
variation_plot <- function(df,
                           dep_var,
                           n_cat = 10,
                           n_quantile = 10,
                           avg_type = c("mean", "median"),
                           ignore_cols = NA_character_) {
  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

  factor_stats <-
    summarize_factors(
      df = refactor_columns(df, dep_var),
      dep_var = dep_var,
      n_cat = n_cat,
      n_quantile = n_quantile,
      avg_type = avg_name,
      ignore_cols = ignore_cols
    )

  grand_avg <- factor_stats$grand_avg[1]

  group_value_ranks <-
    factor_stats %>%
    # left_join(field_ranks) %>%
    mutate(
      field = fct_reorder(.data$field, .data$field_range, .fun = max),
      label = paste(.data$field, ": \n", .data$value)
    )

  group_value_ranks %>%
    ggplot(aes(.data$group_avg, .data$field, color = .data$field, fill = .data$field)) +
    geom_vline(xintercept = grand_avg, color = "grey60", size = 1, linetype = "dotted") +
    geom_line(size = 6, alpha = 0.2, lineend = "round") +
    geom_point(aes(size = .data$n), shape = 21, color = "black", stroke = 0.5) +
    guides(color = FALSE, fill = FALSE) +
    theme(panel.background = element_rect(fill = "white", color = "grey60")) +
    labs(
      title = paste0(dep_var, " across all factors of all fields"),
      subtitle = paste0("each point represents a factor in the field along the y axis, factors with 5 or fewer observations have been excluded \nthe dotted line is the grand ", avg_name, " across all observations"),
      y = "Field",
      x = paste0("group ", avg_name, " of the dependent variable for each factor"),
      size = "# of \n observations"
    )
}


#' Visualize variation and logic for a single observation
#'
#' @param labels when TRUE will show the labels of the factor levels outlined in the plot
#' @param id is id (row number) from \code{base_data()} to use
#'
#' @export
#'
#' @inheritDotParams variation_plot
#' @inheritParams variation_plot
#'
#' @importFrom dplyr filter select
#' @importFrom ggplot2 ggplot geom_vline aes geom_segment geom_point labs
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang .data
#'
#' @examples
#' variation_plot_single_obs(ggplot2::mpg, "hwy")
variation_plot_single_obs <- function(df,
                                      dep_var,
                                      ...,
                                      avg_type = c("mean", "median"),
                                      labels = FALSE,
                                      id = 1) {

  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

  avg <- eval(parse(text = avg_name))

  compare_values <-
    calculate_factor_stats(
      df = df,
      dep_var = dep_var,
      avg_type = avg_name,
      ...
    )

  get_id <- id

  one_obs_profile <-
    compare_values %>%
    filter(.data$datascanr_id == get_id) %>%
    select(
      .data$field, .data$value, .data$field_wt,
      .data$group_avg, #.data$group_dist,
      .data$group_avg_wt,
      .data$estimate
    )


  obs_estimate <- one_obs_profile$estimate[1]

  plot_orig <-
    variation_plot(df = df, dep_var = dep_var, ...) +
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
      title = paste0(dep_var, " across all factors of all fields", " - single observation")
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
