#' debugonce(over_under_split)
#' over_under_split(refactor_columns(mpg, "1", split = "year"), "class", "dv")
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
      values_fill = list(x = 0, n = 0)
    ) %>%
    mutate(
      delta = x_group_2 - x_group_1,
      abs_delta = abs(delta),
      field_delta = sum(abs_delta),
      category =
        case_when(
          delta > 0 ~ "higher",
          delta < 0 ~ "lower",
          TRUE ~ "same"
        )
    )
}


group_split_prep(mpg, dep_var = "hwy", split = "year", type = "dv")

group_split_prep <- function(df,
                             split_on = NULL,
                             type,
                             dep_var = NULL,
                             ...
                             ) {

  calc_type <- type

  if (calc_type != "dv") {dep_var <- "1"}

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




group_split(mpg, split_on = "year", type = "dv", dep_var = "cty")
group_split(mpg, split_on = "year", type = "count", threshold = 10)
group_split(mpg, split_on = "year", type = "percent")

group_split <- function(df,
                        split_on = "",
                        type = c("dv", "percent", "count"),
                        ...,
                        trunc_length = 100,
                        threshold = 0.02,
                        sort_by = c("group_1", "group_2"),
                        group_1 = "Group 1",
                        group_2 = "Group 2",
                        return_data = FALSE,
                        n_field = 9,
                        color_over = "navyblue",
                        color_under = "red") {
  # to be used with scale_color... and scale_fill...
  fill_colors <- c(
    "over" = color_over,
    "under" = color_under
  )

  # dv, percent of pop, or count
  calc_type <- match.arg(type)

  # sort by actual or expected %
  sort_by <- paste0("x_", match.arg(sort_by))

  # get values/captions for threshold if not specified
  if (is.null(threshold)) {
    threshold_value <- 0
    threshold_caption <- ""
    threshold_astrisk <- ""
  } else {
    threshold_value <-
      ifelse(
        type == "percent",
        round(threshold * 100, 1),
        threshold
      )

    threshold_caption <-
      glue("* Values are excluded if difference is < {threshold_value}")
    threshold_astrisk <- "*"
  }

  x_axis <- case_when(
    calc_type == "dv" ~ dep_var,
    calc_type == "percent" ~ "Represenation %",
    TRUE ~ "# of Obs."
  )

  base_data <-
    group_split_prep(df, split_on, type = calc_type, dep_var)

  # return table or plot
  if (return_data) { # return data
    base_data
  } else { # return plot
    plot_data <-
      base_data %>%
      filter(.data$abs_delta > threshold) %>%
      mutate(
        value =
          str_trunc(.data$value, trunc_length) %>%
            fct_reorder(get(sort_by))
      )


    # filter # of facets if n_field specified
    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
    }

    group_counts <-
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
      group_by(group, split) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      arrange(group) %>%
      mutate(
        shape = ifelse(str_sub(sort_by, -1) == group, "line", "point"),
        text = glue("group {group} ({shape}): {split}, n = {n}")
      )


    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
    }

    ggplot(plot_data, aes(y = value, color = category)) +
      geom_col(aes(x = x_group_1, fill = category),
        alpha = 0.2, color = NA
      ) +
      geom_segment(aes(
        x = x_group_1,
        xend = x_group_2, yend = value, color = category,
        group = value
      )) +
      geom_point(aes(
        x = x_group_1,
        size = n_group_1,
      ), shape = "|") +
      geom_point(aes(
        x = x_group_2,
        size = n_group_2
      )) +
      # scale_fill_manual(values = fill_colors) +
      # scale_color_manual(values = fill_colors) +
      guides(color = FALSE) +
      facet_wrap(~field, scales = "free_y") +
      labs(
        title = glue("Population Change {x_axis}"),
        subtitle = paste(paste(group_counts$text, collapse = "; "), threshold_astrisk),
        caption = threshold_caption,
        x = x_axis,
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
