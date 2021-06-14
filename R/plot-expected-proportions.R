#' Helper function to categorize fields
#'
#' @param field text field of column/condition
#' @param df refactored dataframe to
#' @noRd
#' @importFrom dplyr group_by summarise n ungroup mutate case_when
#' @importFrom tidyr replace_na
#' @examples
#' summarize_over_under_proportions(
#'   df = refactor_columns(iris, dep_var = "Sepal.Length > 5"),
#'   "Species"
#' )
summarize_over_under_proportions <- function(field, df) {
  df %>%
    transmute(
      .data$y_outcome,
      .data$y_split,
      value =             # values from column selected
        as.character(get(field)) %>%
        replace_na("NA"),
      field = field       # character string of field name
    ) %>%
    group_by(
      field = .data$field,
      value = .data$value
    )  %>%
    summarise(
      n = n(),
      total = sum(.data$y_outcome, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      expected = .data$n / sum(.data$n) * 100,
      actual = .data$total / sum(.data$total) * 100,
      delta = .data$actual - .data$expected,
      abs_delta = abs(.data$delta),
      field_delta = sum(.data$abs_delta),
      category = case_when(
        .data$delta > 0 ~ "over",
        .data$delta < 0 ~ "under",
        TRUE ~ "same"
      )
    )
}



#' Prep for plot_expected_proportions() function
#'
#' @param df data frame to analyze
#' @inheritDotParams refactor_columns
#' @noRd
#' @importFrom dplyr select mutate
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
#' @examples
#' map_over_under_proportions(df = iris, dep_var = "Sepal.Length > 5")
map_over_under_proportions <- function(df, ...) {
  refactor_df <-
    refactor_columns(df, ...) %>%
    select(-.data$unique_id)

  # give warning if dep_var isn't 0/1 or T/F
  check_01_binary(refactor_df$y_outcome)

  names(refactor_df %>% select(-.data$y_outcome)) %>%
    map_dfr(summarize_over_under_proportions, df = refactor_df) %>%
    mutate(field = fct_reorder(.data$field, .data$field_delta, .desc = TRUE))
}

#' Visualize variation between the expected & actual percentages
#'
#' Ideal for a 0/1 dichotomous variable.
#'
#' @param df data to be analyzed
#' @param dep_var dependent variable
#' @param trunc_length length to shorten y-axis labels
#' @param sort_by should data be sorted by expected or actual percentages
#' @param threshold the cut-off (percentage difference) between actual and
#' expected values. This allows the chart to focus on the bigger changes.
#' Use \code{NULL} to keep all values
#' @param return_data if TRUE will return a data frame instead of a plot
#' @param n_field the max number of facets to show. The fields are sorted in
#' descending order by those that have the most change (the 'field_delta'
#' column).
#' @param color_over color name/hex code for values that are over-represented
#' @param color_under color name/hex code for values that are under-represented
#'
#' @export
#' @inheritDotParams refactor_columns
#' @importFrom glue glue
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_trunc
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_point
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 facet_wrap guides labs theme element_text element_rect
#'
#' @examples
#' plot_expected_proportions(df = iris, dep_var = "Sepal.Length > 5")
#'
#' # sorted by the expected representation (default)
#' plot_expected_proportions(
#'   df = mtcars,
#'   dep_var = "mpg > 15",
#' )
#'
#' # sorted by the actual representation
#' plot_expected_proportions(
#'   df = mtcars,
#'   dep_var = "mpg > 15",
#'   sort_by = "actual"
#' )
#'
#' # you can return the dataframe if you want
#' plot_expected_proportions(
#'   df = mtcars,
#'   dep_var = "mpg > 15",
#'   return_data = TRUE
#' )
#'
#' # an example with more parameters
#' plot_expected_proportions(
#'   df = mtcars, # data to use
#'   dep_var = "mpg > 15", # can be a field name or an evaluation
#'   n_cat = 5, # collapse field values into 5 categories
#'   n_field = 3, # keep the frist 3 facets
#'   threshold = NULL # keep all values
#' )
plot_expected_proportions <- function(df,
                                      dep_var,
                                      ...,
                                      trunc_length = 100,
                                      sort_by = c("expected", "actual"),
                                      threshold = 0.02,
                                      return_data = FALSE,
                                      n_field = 9,
                                      color_over = "navyblue",
                                      color_under = "red") {

  # to be used with scale_color... and scale_fill...
  fill_colors <- c(
    "over" = color_over,
    "under" = color_under
  )

  # sort by actual or expected %
  sort_by <- match.arg(sort_by)

  # get values/captions for threshold if not specified
  if (is.null(threshold)) {
    threshold <- 0
    threshold_caption <- ""
    threshold_astrisk <- ""
  } else {
    threshold <- round(threshold * 100, 1)
    threshold_caption <-
      glue("* Values are excluded if difference is < {threshold}%")
    threshold_astrisk <- "*"
  }

  base_data <-
    map_over_under_proportions(df, dep_var, ...) %>%
    filter(.data$abs_delta > threshold)

  # return table or plot
  if (return_data) { # return data
    base_data
  } else { # return plot
    plot_data <-
      base_data %>%
      mutate(
        value =
          str_trunc(.data$value, trunc_length) %>%
            fct_reorder(get(sort_by))
      )


    # filter # of facets if n_field specified
    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
    }

    # make plot
    ggplot(plot_data, aes(y = .data$value, color = .data$category)) +
      geom_col(
        aes(x = .data$expected, fill = .data$category),
        alpha = 0.2, color = NA
      ) +
      geom_segment(
        aes(
          x = .data$actual, xend = .data$expected,
          yend = .data$value,
          color = .data$category,
          group = .data$value
        )
      ) +
      geom_point(aes(x = .data$expected, size = .data$n), shape = "|") +
      geom_point(aes(x = .data$actual, size = .data$n)) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = fill_colors) +
      facet_wrap(~ .data$field, scales = "free_y") +
      guides(color = FALSE) +
      labs(
        title = glue("Over/Under Representatin of '{dep_var}'"),
        subtitle =
          glue(
            "% of population (line) compared to \\
            % of obs. with {dep_var} (circle){threshold_astrisk}"
          ),
        caption = threshold_caption,
        x = "Representation %",
        y = "",
        size = "# of obs.",
        color = "Difference"
      ) +
      theme(
        axis.text.y = element_text(size = 9),
        panel.background = element_rect(color = "grey70", fill = "white"),
        legend.position = "left"
      )
  }
}
