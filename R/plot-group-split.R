#' Prep to find over/under values for group split
#'
#'
#' @param df refactored data frame
#' @param field field to evaluate
#' @inheritParams group_split
#' @importFrom dplyr group_by dense_rank summarise n ungroup mutate case_when
#' @importFrom tidyr pivot_wider replace_na fill
#' @importFrom stats na.omit
#' @noRd
#' @examples
#' summarize_over_under_split(
#'   df = refactor_columns(ggplot2::mpg, "hwy", split_on = "year", n_cat = NULL),
#'   field = "model",
#'   type = "dv",
#'   n_cat = 5
#' )
summarize_over_under_split <- function(df,
                                       field,
                                       type,
                                       n_cat,
                                       base_group = "1") {
  # df <- head(ggplot2::mpg, 100); df$y_split <- df$year; split <- "year"; field <- "model"; type = "percent_factor"; base_group = "1"
  group_df <-
    df %>%
    group_by(
      field = field,
      split = .data$y_split,
      split_ord = paste0("group_", dense_rank(.data$split)),
      value =
        as.character(get(field)) %>%
          replace_na("NA")
    )


  if (type == "dv") {
    agg_fn <- .mean
    calc_df <-
      group_df %>%
      summarise(
        n = n(),
        x = mean(.data$y_outcome)
      )
  } else if (type == "count") {
    agg_fn <- .sum
    calc_df <-
      group_df %>%
      summarise(
        n = n(),
        x = n()
      )
  } else if (type == "percent_field") {
    agg_fn <- .mean
    calc_df <-
      group_df %>%
      count() %>%
      group_by(.data$split_ord) %>%
      mutate(x = .data$n / sum(.data$n) * 100)
  } else if (type == "percent_factor") {
    agg_fn <- .mean
    calc_df <-
      group_df %>%
      count() %>%
      group_by(.data$value) %>%
      mutate(x = .data$n / sum(.data$n) * 100)
  }

  final_df <-
    calc_df %>%
    ungroup() %>%
    mutate(
      split_ord = ifelse(
        str_detect(split_ord, base_group), "bar", "point")
    ) %>%
    pivot_wider(
      names_from = .data$split_ord,
      values_from = c(.data$split, .data$x, .data$n),
      values_fill = list(n = 0, x = 0)
    ) %>%
    mutate(
      delta = .data$x_point - .data$x_bar,
      abs_delta = abs(.data$delta),
      value = collapse_cat(.data$value, n = n_cat, w = .data$abs_delta)
    ) %>%
    group_by(.data$field, .data$value) %>%
    mutate(
      has_bar = max(!is.na(.data$split_bar)),
      has_point = max(!is.na(.data$split_point))
    ) %>%
    ungroup() %>%
    fill(split_bar, split_point, .direction = "updown") %>%
    group_by(
      .data$field, .data$value,
      .data$has_bar, .data$has_point,
      .data$split_bar, .data$split_point
    ) %>%
    summarise(
      x_bar = agg_fn(.data$x_bar),
      x_point = agg_fn(.data$x_point),
      n_bar = sum(.data$n_bar),
      n_point = sum(.data$n_point)
    ) %>%
    ungroup() %>%
    mutate(
      expected = mean(df$y_outcome),
      delta = .data$x_point - .data$x_bar
    )

  if (type == "percent_factor") {
    # return df with expected %
    final_df <-
      final_df %>%
      mutate(
        expected =
          .sum(.data$n_point) / .sum(calc_df$n) * 100,
        delta = .data$x_point - .data$expected
      )
  } else if (type == "percent_field") {
    final_df$expected <- NA_integer_
  }

  # else return data
  final_df %>%
    mutate(
      abs_delta = abs(.data$delta),
      field_delta = sum(.data$abs_delta, na.rm = TRUE),
      category =
        case_when(
          .data$has_bar == 0 | .data$has_point == 0 ~ "missing",
          .data$delta > 0 ~ "higher",
          .data$delta < 0 ~ "lower",
          TRUE ~ "same"
        )
    )
}


#' Prep for group_split by iterating summarize_over_under_split()
#' @inheritDotParams refactor_columns
#' @inheritParams group_split
#' @inheritParams refactor_columns
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @noRd
#' @examples
#' map_over_under_split(
#'   df = ggplot2::mpg,
#'   dep_var = "hwy",
#'   split_on = "year",
#'   type = "dv",
#'   n_cat = 5
#' )
map_over_under_split <- function(df,
                                 split_on = NULL,
                                 type,
                                 dep_var,
                                 n_cat,
                                 base_group = "1",
                                 ...) {
  calc_type <- type

  dep_var <- check_dv_has_value(calc_type, missing(dep_var), dep_var)

  refactor_df <-
    refactor_columns(
      df,
      dep_var = dep_var,
      ...,
      n_cat = NULL,
      split_on = split_on
    )

  # give warning if dep_var isn't 0/1 or T/F
  check_binary(refactor_df$y_split)

  names(refactor_df)[4:length(refactor_df)] %>%
    map_dfr(summarize_over_under_split, df = refactor_df, type = type, n_cat = n_cat, base_group = base_group) %>%
    mutate(field = fct_reorder(.data$field, .data$field_delta, .desc = TRUE))
}




#' Visualize variation between two groups
#'
#' @param type the outcome or dependent variable ("dv"), the percent of obs.
#' ("percent"), or the number of obs. ("count")
#' @param n_cat the number of factors to keep in the y-axis. Factors will be
#' prioritized by the size of the difference and may not match the way
#' categories are collapsed in \code{refactor_columns()}
#' @param trunc_length number of charcters to print on y-axis
#' @param threshold threshold for excluding nominal differences. The value
#' should reflect the type, if the count is in the hundreds you might use 20,
#' meaning when viewing count differences, values where the difference is <20
#' will be excluded. For proportion/percent and the dv type, the default is 0.02
#' or 2 percept
#' @param base_group Should group 1 or group 2 be the base. This group will be
#' the bar and the other will be the point.
#' @param return_data When TRUE will return data frame instead of a plot.
#' @param n_field How many fields/facets should the plot return.
#' @param color_over Color to use when point is higher than bar
#' @param color_under Color to use when point is lower than bar
#' @param color_missing Color to use when either a point or bar is missing
#' @param title title for chart
#' @param subtitle subtitle for chart
#' @param caption caption for chart
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#' @importFrom glue glue
#' @importFrom dplyr case_when filter select
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_point scale_fill_manual scale_color_manual guides facet_wrap labs theme element_text element_rect
#' @export
#'
#' @examples
#' plot_group_split(ggplot2::mpg, split_on = "year", type = "dv", dep_var = "cty")
#' plot_group_split(ggplot2::mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "2")
#' plot_group_split(ggplot2::mpg, split_on = "year", type = "count", threshold = 10)
#' plot_group_split(ggplot2::mpg, split_on = "year", type = "percent")
#' plot_group_split(
#'   ggplot2::mpg %>% dplyr::select(year, cty, trans),
#'   split_on = "year",
#'   type = "dv",
#'   dep_var = "cty",
#'   base_group = "1", # return_data = TRUE,
#'   color_missing = "violet"
#' )
#'
#' plot_group_split(ggplot2::mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "1")
#' plot_group_split(ggplot2::mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "2")
plot_group_split <- function(df,
                             split_on,
                             type = c("dv", "percent", "count"),
                             dep_var,
                             ...,
                             n_cat = 10,
                             trunc_length = 100,
                             threshold = 0.02,
                             base_group = c("1", "2"),
                             return_data = FALSE,
                             n_field = 9,
                             color_over = "navyblue",
                             color_under = "red",
                             color_missing = "grey50",
                             title = NULL,
                             subtitle = NULL,
                             caption = NULL) {
  # to be used with scale_color... and scale_fill...
  fill_colors <- c(
    "higher" = color_over,
    "lower" = color_under,
    "missing" = color_missing
  )

  # dv, percent of pop, or count
  calc_type <- match.arg(type)
  ref_group <- match.arg(base_group)

  dep_var <- check_dv_has_value(calc_type, missing(dep_var), dep_var)

  base_data <-
    map_over_under_split(
      df = df,
      split_on = split_on,
      type = calc_type,
      dep_var = dep_var,
      n_cat = n_cat,
      base_group = ref_group,
      ...
    ) %>%
    mutate(
      split_bar = ifelse(.data$has_bar == 0, NA, .data$split_bar),
      split_point = ifelse(.data$has_point == 0, NA, .data$split_point),
      x_bar = ifelse(.data$has_bar == 0, NA, .data$x_bar),
      x_point = ifelse(.data$has_point == 0, NA, .data$x_point)
    )


  plot_data <-
    plot_group_split_prep(base_data, threshold, ref_group, trunc_length)
  # return table or plot
  if (return_data) { # return data
      plot_data
  } else { # return plot
    # filter # of facets if n_field specified
    if (!is.null(n_field)) {
      plot_data <-
        plot_data %>%
        filter(as.integer(.data$field) <= n_field) %>%
        filter(.data$n_bar > 10 & .data$n_point > 10) %>%
        mutate(
          x_end = ifelse(rep(type == "percent_factor", n()), .data$expected, .data$x_bar)
        )
    }

    group_counts <- summarize_group_split_metadata(base_data, ref_group, split_on)

    # labels
    # get values/captions for threshold if not specified
    if (is.null(threshold)) {
      threshold_value <- 0
      threshold_caption <- ""
    } else {
      threshold_value <-
        ifelse(
          type == "percent",
          round(threshold * 100, 1),
          threshold
        )

      threshold_caption <-
        glue("* Values are excluded if difference is < {threshold_value}")
    }

    # x-axis
    x_axis <- case_when(
      calc_type == "dv" ~ dep_var,
      calc_type == "percent" ~ "Represenation %",
      TRUE ~ "# of Obs."
    )

    # title
    if (!is.null(title)) {
      title_text <- title
    } else {
      title_text <-
        case_when(
          calc_type == "dv" ~ glue("Change in outcome ({dep_var})"),
          calc_type == "percent" ~ "Change in Proportion of Population",
          TRUE ~ "Change in # of Obs."
        )
    }

    # subtitle
    subtitle_text <- paste(group_counts$text, collapse = "\n")

    if (!is.null(subtitle)) {
      subtitle_text <- paste(subtitle, subtitle_text, sep = "\n")
    }

    # caption
    caption_text <-
      ifelse(
        !is.null(caption),
        paste(threshold_caption, caption, sep = "\n"),
        threshold_caption
      )

    # plot
    ggplot(plot_data, aes(y = .data$value, color = .data$category)) +
      geom_col(
        aes(x = .data$plot_bar, fill = .data$category),
        alpha = 0.2, color = NA
      ) +
      geom_segment(
        data = filter(plot_data, !is.na(.data$point)),
        aes(
          x = .data$plot_bar, xend = .data$plot_point, yend = .data$value,
          color = .data$category, group = .data$value
        )
      ) +
      geom_point(
        aes(x = .data$plot_point),
        size = 3
      ) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = fill_colors) +
      guides(color = FALSE) +
      facet_wrap(~ .data$field, scales = "free_y") +
      labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        x = x_axis,
        y = "",
        size = paste0("# of obs. when\n", group_counts$label[2]),
        fill = "Difference"
      ) +
      theme(
        axis.text.y = element_text(size = 9),
        panel.background = element_rect(color = "grey70", fill = "white"),
        plot.title.position = "plot",
        legend.position = "bottom"
      )
  }
}



#' Aggregate data for group_split labels
#' @param base_data data frame
#' @importFrom dplyr filter select everything count mutate arrange
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @noRd
#' @examples
#' summarize_group_split_metadata(
#'   base_data =
#'     map_over_under_split(
#'       df = ggplot2::mpg,
#'       split_on = "year",
#'       type = "dv",
#'       dep_var = "hwy",
#'       n_cat = 5
#'     ),
#'   ref_group = "1",
#'   split_on = "year"
#' )
summarize_group_split_metadata <- function(base_data, ref_group, split_on) {
  base_data %>%
    filter(as.integer(.data$field) == min(as.integer(.data$field))) %>%
    select(.data$split_bar, .data$split_point, .data$n_bar, .data$n_point) %>%
    pivot_longer(
      everything(),
      names_to = c(".value", "group"),
      names_pattern = "(.*)_(.*)",
      values_drop_na = TRUE
    ) %>%
    filter(n != 0) %>%
    count(.data$group, .data$split, wt = .data$n, name = "n") %>%
    arrange(.data$group) %>%
    mutate(
      label = glue("{split_on} is {split}"),
      text = glue("{label} ({group}), n = {n}")
    )
}

#' Prep data for group_split plotting
#' @param base_data data frame
#' @importFrom dplyr filter mutate coalesce arrange
#' @importFrom forcats fct_inorder
#' @noRd
#' @examples
#' plot_group_split_prep(
#'   base_data =
#'     map_over_under_split(
#'       df = ggplot2::mpg,
#'       split_on = "year",
#'       type = "dv",
#'       dep_var = "hwy",
#'       n_cat = 5
#'     ),
#'   ref_group = "1",
#'   threshold = 0.02,
#'   trunc_length = 20
#' )
plot_group_split_prep <- function(base_data, threshold, ref_group, trunc_length) {
  base_data %>%
    filter(is.na(.data$delta) | .data$abs_delta > threshold) %>%
    mutate(
      ref_group_1 = ref_group == "1",
      bar = ifelse(.data$ref_group_1, .data$x_group_1, .data$x_group_2),
      point = ifelse(.data$ref_group_1, .data$x_group_2, .data$x_group_1),
      plot_bar = coalesce(as.numeric(.data$bar), 0),
      plot_point = coalesce(as.numeric(.data$point), 0)
    ) %>%
    arrange(.data$plot_bar, .data$plot_point) %>%
    mutate(value = fct_inorder(str_trunc(.data$value, trunc_length)))
}


#' Assign default value to dep_var or throw error depending on type
#'
#' @param type evaluation type to use
#' @param cond condition for evaluation \code{missing(dep_var)}
#' @param dep_var dependent variable
#'
#' @noRd
check_dv_has_value <- function(type, cond, dep_var) {
  if (type != "dv") {
    "1"
  } else if (cond) {
    stop("'dep_var' required when type = 'dv'")
  } else {
    dep_var
  }
}
