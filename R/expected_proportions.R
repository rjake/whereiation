#' Helper function to categorize fields
#'
#' @param field text field of column/condition
#' @param df refactored dataframe to
#' @examples
#' over_under_rep(
#'   df = refactor_columns(iris, dep_var = "Sepal.Length > 5"),
#'   "Species"
#' )
over_under_rep <- function(field, df) {
  df %>%
    group_by(
      field = field,
      value = as.character(get(field))
    ) %>%
    summarise(
      n = n(),
      total = sum(.data$y_outcome)
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



#' Prep for expected_proportions() function
#'
#' @param df data frame to analyze
#' @inheritDotParams variation_plot
#'
#' @examples
#' expected_prop_prep(df = iris, dep_var = "Sepal.Length > 5")
expected_prop_prep <- function(df, ...) {
  refactor_df <-
    refactor_columns(df, ...) %>%
    select(-.data$y_id)

  # give warning if dep_var isn't 0/1 or T/F
  check_01_binary(refactor_df$y_outcome)

  names(refactor_df %>% select(-.data$y_outcome)) %>%
    map_dfr(over_under_rep, df = refactor_df) %>%
    mutate(field = fct_reorder(.data$field, .data$field_delta, .desc = TRUE))
}


#' Compare representation of proportions (over/under)
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
#' @inheritDotParams variation_plot
#' @importFrom glue glue
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_trunc
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_point
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom ggplot2 facet_wrap guides labs theme element_text element_rect
#'
#' @examples
#' expected_proportions(df = iris, dep_var = "Sepal.Length > 5", sort_by = "a")
#'
#' # sorted by the expected representation (default)
#' expected_proportions(
#'   df = mtcars,
#'   dep_var = "mpg > 15",
#' )
#'
#' # sorted by the actual representation
#' expected_proportions(
#'   df = mtcars,
#'   dep_var = "mpg > 15",
#'   sort_by = "actual"
#' )
#'
#' # you can return the dataframe if you want
#' expected_proportions(
#'   df = mtcars,
#'   dep_var = "mpg > 15",
#'   return_data = TRUE
#' )
#'
#' # an example with more parameters
#' expected_proportions(
#'   df = mtcars,          # data to use
#'   dep_var = "mpg > 15", # can be a field name or an evaluation
#'   n_cat = 5,            # collapse field values into 5 categories
#'   n_field = 3,          # keep the frist 3 facets
#'   threshold = NULL      # keep all values
#' )
expected_proportions <- function(df,
                                 dep_var,
                                 ...,
                                 trunc_length = 100,
                                 sort_by = c("expected", "actual"),
                                 threshold = 0.02,
                                 return_data = FALSE,
                                 n_field = 9,
                                 color_over = "navyblue",
                                 color_under = "red"
                                 ) {

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
    expected_prop_prep(df, dep_var, ...) %>%
    filter(abs_delta > threshold)

  # return table or plot
  if (return_data) {# return data
    base_data
  } else {# return plot
    plot_data <-
      base_data %>%
      filter(abs_delta > threshold) %>%
      mutate(
        value =
          str_trunc(.data$value, trunc_length) %>%
          fct_reorder(get(sort_by))
      )


    # filter # of facets if n_field specified
    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(field) <= n_field)
    }

    # make plot
    ggplot(plot_data, aes(y = value, color = category)) +
      geom_col(
        aes(x = expected, fill = category),
        alpha = 0.2, color = NA
      ) +
      geom_segment(
        aes(
          x = actual, xend = expected,
          yend = value,
          color = category,
          group = value
        )
      ) +
      geom_point(aes(x = expected, size = n), shape = "|") +
      geom_point(aes(x = actual, size = n)) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = fill_colors) +
      facet_wrap(~ field ,scales = "free_y") +
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

