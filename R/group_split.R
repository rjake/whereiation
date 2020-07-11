#'
#'
#'
#' @param df
#' @param field
#' @param type
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


#'
#' @param df
#' @param split_on
#' @param type
#' @param dep_var
#' @param ...
#' @examples
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




#' Title
#'
#' @param df
#' @param split_on
#' @param type
#' @param ...
#' @param trunc_length
#' @param threshold
#' @param base_group
#' @param return_data
#' @param n_field
#' @param color_over
#' @param color_under
#' @param color_missing
#'
#' @return
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
#'   color_missing = "orange"
#' )
#'
#' #group_split(mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "1")
#' #debugonce(group_split)
#' #group_split(mpg, split_on = "year", type = "dv", dep_var = "cty", base_group = "2")
group_split <- function(df,
                        split_on,
                        type = c("dv", "percent", "count"),
                        dep_var = NULL,
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

  title <- case_when(
    calc_type == "dv" ~ glue("Change in outcome ({dep_var})"),
    calc_type == "percent" ~ "Change in volume",
    TRUE ~ "Change in # of Obs."
  )


  base_data <-
    group_split_prep(df, split_on, type = calc_type, dep_var)

#browser()


  plot_data <-
    base_data %>%
    filter(is.na(.data$delta) | .data$abs_delta > threshold) %>%
#      filter(field == "trans") %>% # DELETE DELETE DELETE
    mutate(
      ref_group_1 = ref_group == "1",
      bar = ifelse(ref_group_1, x_group_1, x_group_2),
      point = ifelse(ref_group_1, x_group_2, x_group_1),
      plot_bar = coalesce(as.numeric(bar), 0),
      plot_point = coalesce(as.numeric(point), 0)
    ) %>%
    arrange(plot_bar, plot_point) %>%
    mutate(
      value =
        str_trunc(.data$value, trunc_length) %>%
        fct_inorder()
    )

  # ggplot(plot_data, aes(y = value, color = category)) +
  #   geom_col(aes(x = bar, fill = category),alpha = 0.2, color = NA) +
  #   geom_point(aes(x = plot_point)) + facet_wrap(~field, scales = "free")
  #  #plot_data %>% select(1:5,6,13:17) %>% arrange(value) %>% mutate(i = as.integer(value))
  #
  # }
  # }
  # return table or plot
  if (return_data) { # return data
    plot_data %>%
      select(-c(abs_delta, ref_group_1, plot_bar, plot_point))
  } else { # return plot


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
        shape = ifelse(ref_group == as.character(group), "bar", "point"),
        label = glue("{split_on} is {split}"),
        text = glue("Group {group} ({shape}): {label}, n = {n}")
      )
# group_counts

    if (!is.null(n_field)) {
      plot_data <- filter(plot_data, as.integer(.data$field) <= n_field)
    }

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
