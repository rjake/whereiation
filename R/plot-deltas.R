#' Visualize difference from avg. by factor (lollipop)
#'
#' @param trunc_length number of charcters to print on y-axis
#' @param return_data When TRUE will return data frame instead of a plot.
#' @param n_field How many fields/facets should the plot return.
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_trunc str_remove_all
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_vline geom_segment geom_point facet_wrap scale_alpha theme element_rect labs
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' # show distances (deltas) from grand mean
#' plot_deltas(df = ggplot2::mpg, dv = hwy)
#'
#' # adjust aesthetics with 'trunc_length' and 'n_field'
#' plot_deltas(
#'   df = ggplot2::mpg,
#'   dv = hwy,
#'   trunc_length = 15,
#'   n_field = 5
#' )
plot_deltas <- function(df,
                        dv,
                        ...,
                        trunc_length = 100,
                        return_data = FALSE,
                        n_field = 9,
                        avg_type = c("mean", "median")) {
  avg_name <- match.arg(avg_type)
  dv_name <- deparse(substitute(dv))

  factor_stats <-
    summarize_factors_all_fields(
      df = df,
      dv = {{dv}},
      ...
    )

  # prep plots/facets
  plot_data <-
    factor_stats %>%
    select(-.data$rescale_factor_avg) %>%
    mutate(
      field =
        fct_reorder(.data$field, .data$field_wt, .fun = max, .desc = TRUE),
      value =
        str_trunc(.data$value, trunc_length) %>%
        fct_reorder(.data$factor_avg, .fun = max, .desc = TRUE),
      delta = .data$factor_avg - .data$grand_avg,
      abs_delta = abs(.data$delta),
      color = ifelse(.data$factor_avg > .data$grand_avg, "above", "below")
    )

  # return table if requesed
  if (return_data) { # return data
    return(plot_data)
  }

  # return plot
  # filter # of facets if n_field specified
  if (!is.null(n_field)) {
    plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
  }

  plot_data %>%
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
    theme(
      axis.text.y = element_text(size = 9),
      panel.background = element_rect(color = "grey70", fill = "white"),
      plot.title.position = "plot",
      legend.position = "bottom"
    ) +
    scale_alpha(range = c(0.2, 1), guide = FALSE) +
    labs(
      title = glue("Difference in {avg_name} {dv_name} from grand {avg_name} across all factors of all fields"),
      subtitle = glue(
        "Each chart shows the different values (factors) within each field and the {avg_name} {dv_name} for each. Charts with the highest \nadjusted R-square start in the top left. Factors with 5 or fewer observations have been excluded and the vertical line \nis the grand {avg_name} across all observations"
      ),
      y = NULL,
      x = paste(avg_name, dv_name),
      size = "# of \n observations"
    )
}


