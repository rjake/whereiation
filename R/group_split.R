#' Prep to find over/under values for group split
#'
#'
#' @param df refactored data frame
#' @param field field to evaluate
#' @param type dv, count, or proportion
#' @importFrom dplyr group_by dense_rank replace_na summarise n mutate case_when
#' @importFrom tidyr pivot_wider
#' @noRd
#' @examples
#' over_under_split(df = refactor_columns(mpg, "hwy", split = "year"), field = "class", type = "dv")
over_under_split <- function(df,
                             field,
                             type
                             ) {
  # df <- ggplot2::mpg; split <- "year"; field <- "model"
  group_df <-
    df %>%
    group_by(
      field = field,
      split = y_split,
      split_ord = paste0("group_", dense_rank(split)),
      value =
        as.character(get(field)) %>%
          replace_na("NA")
    )


  if (type == "dv") {
    calc_df <-
      group_df %>%
      summarise(
        n = n(),
        x = mean(y_outcome)
      )
  } else if (type == "count") {
    calc_df <-
      group_df %>%
      summarise(
        n = n(),
        x = n()
      )
  } else if (type == "percent") {
    calc_df <-
      group_df %>%
      summarise(
        n = n(),
        x = n()
      ) %>%
      group_by(split_ord) %>%
      mutate(x = x / sum(x) * 100)
  }

  calc_df %>%
    ungroup() %>%
    pivot_wider(
      names_from = split_ord,
      values_from = c(split, x, n),
      values_fill = list(n = 0)
    ) %>%
    mutate(
      delta = x_group_2 - x_group_1,
      abs_delta = abs(delta),
      field_delta = sum(abs_delta, na.rm = TRUE),
      category =
        case_when(
          is.na(x_group_1) | is.na(x_group_2) ~ "missing",
          delta > 0 ~ "higher",
          delta < 0 ~ "lower",
          TRUE ~ "same"
        )
    )
}


#' Prep for group_split by iterating over_under_split()
#' @param df normal data frame
#' @param split_on value to split on
#' @param type dv,count, or proportion
#' @param dep_var dependent variable
#' @param ... inherited from \code{variation_plot()}
#' @inheritDotParams variation_plot
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
#' @importFrom forcats fct_reorder
#' @noRd
#' @examples
#' group_split_prep(mpg, dep_var = "hwy", split = "year", type = "dv")
group_split_prep <- function(df,
                             split_on = NULL,
                             type,
                             dep_var,
                             ...
                             ) {

  calc_type <- type

  dep_var <- check_dv_has_value(calc_type, missing(dep_var), dep_var)

  refactor_df <-
    refactor_columns(
      df,
      dep_var = dep_var,
      ...,
      split_on = split_on
    )

  # give warning if dep_var isn't 0/1 or T/F
  check_binary(refactor_df$y_split)

  names(refactor_df)[4:length(refactor_df)] %>%
    map_dfr(over_under_split, df = refactor_df, type = type) %>%
    mutate(field = fct_reorder(.data$field, .data$field_delta, .desc = TRUE))
}




#' Evaluate difference between two groups based on value of a field
#'
#' @param df data frame to evaluate
#' @param split_on field that differentiates the two groups
#' @param type the outcome or dependent variable ("dv"), the percent of obs.
#' ("percent"), or the number of obs. ("count")
#' @param ... inherited from \code{variation_plot()}
#' @param trunc_length number of charcters to print on y-axis
#' @param threshold threshold for excluding nominal differences. The value
#' should reflect the type, if the count is in the hundreds you might use 20,
#' meaning when viewing count differences, values where the difference is <20
#' will be excluded. For proportion/percent and the dv type, the default is 0.02
#' or 2%
#' @param base_group Should group 1 or group 2 be the base. This group will be
#' the bar and the other will be the point.
#' @param return_data When TRUE will return data frame instead of a plot.
#' @param n_field How many fields/facets should the plot return.
#' @param color_over Color to use when point is higher than bar
#' @param color_under Color to use when point is lower than bar
#' @param color_missing Color to use when either a point or bar is missing
#' @inheritDotParams variation_plot
#' @importFrom glue glue
#' @importFrom dplyr case_when filter select
#' @importFrom ggplot ggplot aes geom_col geom_segment geom_point scale_fill_manual scale_color_manual guides facet_wrap labs theme element text element_rect
#' @export
#'
#' @examples
#' group_split(mpg, split_on = "year", type = "dv", dep_var = "cty")
#' group_split(mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = 2)
#' group_split(mpg, split_on = "year", type = "count", threshold = 10)
#' group_split(mpg, split_on = "year", type = "percent")
#' group_split(
#'   mpg %>% select(year, cty, trans),
#'   split_on = "year",
#'   type = "count",
#'   dep_var = "cty",
#'   base_group = "1", #return_data = TRUE,
#'   color_missing = "violet"
#' )
#'
#' group_split(mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "1")
#' group_split(mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "2")
group_split <- function(df,
                        split_on,
                        type = c("dv", "percent", "count"),
                        dep_var,
                        ...,
                        trunc_length = 100,
                        threshold = 0.02,
                        base_group = c("1", "2"),
                        return_data = FALSE,
                        n_field = 9,
                        color_over = "navyblue",
                        color_under = "red",
                        color_missing = "grey20") {
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

  x_axis <- case_when(
    calc_type == "dv" ~ dep_var,
    calc_type == "percent" ~ "Represenation %",
    TRUE ~ "# of Obs."
  )

  title <- case_when(
    calc_type == "dv" ~ glue("Change in outcome ({dep_var})"),
    calc_type == "percent" ~ "Change in Proportion of Population",
    TRUE ~ "Change in # of Obs."
  )

  base_data <-
    group_split_prep(df, split_on, type = calc_type, dep_var)

  plot_data <-
    group_split_plot_data(base_data, threshold, ref_group, trunc_length)

  # return table or plot
  if (return_data) {# return data
    select(plot_data, -c(abs_delta, ref_group_1, plot_bar, plot_point))
  } else {# return plot
    # filter # of facets if n_field specified
    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
    }

    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
    }

    group_counts <- group_split_counts(base_data, ref_group, split_on)

    ggplot(plot_data, aes(y = value, color = category)) +
      geom_col(
        aes(x = plot_bar, fill = category), alpha = 0.2, color = NA) +
      geom_segment(
        data = filter(plot_data, !is.na(point)),
        aes(
          x = plot_bar, xend = plot_point, yend = value,
          color = category, group = value
        )
      ) +
      geom_point(
        aes(x = plot_point),
        size = 3
      ) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = fill_colors) +
      guides(color = FALSE) +
      facet_wrap(~field, scales = "free_y") +
      labs(
        title = title,
        subtitle = paste(group_counts$text, collapse = "\n"),
        caption = threshold_caption,
        x = x_axis,
        y = "",
        size = paste0("# of obs. when\n", group_counts$label[2]),
        fill = "Difference"
      ) +
      theme(
        axis.text.y = element_text(size = 9),
        panel.background = element_rect(color = "grey70", fill = "white"),
        legend.position = "left"
      )
  }
}



#' Aggregate data for group_split labels
#' @param base_data data frame
#' @importFrom dplyr filter select everythiong count mutate arrange
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#' @noRd
#' @examples
#' group_split_counts(
#'   base_data =
#'     group_split_prep(
#'       df = ggplot2::mpg,
#'       split_on = "year",
#'       type = "dv",
#'       dep_var = "hwy"
#'       )
#' )
group_split_counts <- function(base_data, ref_group, split_on) {
  base_data %>%
  filter(as.integer(field) == min(as.integer(field))) %>%
  select(split_group_1, split_group_2, n_group_1, n_group_2) %>%
  pivot_longer(
    everything(),
    names_to = c(".value", "group"),
    names_pattern = "(.*)_group_(.*)",
    values_drop_na = TRUE
  ) %>%
  filter(n != 0) %>%
  count(group, split, wt = n, name = "n") %>%
  arrange(group) %>%
  mutate(
    shape = ifelse(ref_group == group, "bar", "point"),
    label = glue("{split_on} is {split}"),
    text = glue("Group {group} ({shape}): {label}, n = {n}")
  )
}

#' Prep data for group_split plotting
#' @param base_data data frame
#' @importFrom dplyr filter mutate coalesce arrange
#' @import forcats fct_inorder
#' @noRd
#' @examples
#' group_split_plot_data(
#'   base_data =
#'     group_split_prep(
#'       df = ggplot2::mpg,
#'       split_on = "year",
#'       type = "dv",
#'       dep_var = "hwy"
#'       )
#' )
group_split_plot_data <- function(base_data, threshold, ref_group, trunc_length) {
  base_data %>%
    filter(is.na(.data$delta) | .data$abs_delta > threshold) %>%
    mutate(
      ref_group_1 = ref_group == "1",
      bar = ifelse(ref_group_1, x_group_1, x_group_2),
      point = ifelse(ref_group_1, x_group_2, x_group_1),
      plot_bar = coalesce(as.numeric(bar), 0),
      plot_point = coalesce(as.numeric(point), 0)
    ) %>%
    arrange(plot_bar, plot_point) %>%
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
