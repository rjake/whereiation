#' Summarize fields
#'
#' Pivots data and summarizes factor frequencies by field and generates stats
#' used for plotting
#'
#' @param return option to return a dataframe when TRUE and a list when FALSE.
#' The list option includes the original min/max of the data and the
#' grand average.
#'
#' @inheritDotParams variation_plot
#' @inheritParams variation_plot
#'
#' @importFrom dplyr select starts_with select_if mutate group_by summarise n ungroup row_number filter do rename
#' @importFrom broom glance
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
#' @importFrom stats sd lm
#' @importFrom rlang .data
#'
#' @export
#' @family manipulation functions
summarize_factors <- function(df,
                              ...,
                              avg_type = c("mean", "median"),
                              return = c("data", "list")
                              ) {
  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

  base_data <-
    refactor_columns(df, ...) %>%
    filter(!is.na(.data$y_outcome))

  avg <- eval(parse(text = avg_name))

  grand_avg <- avg(base_data$y_outcome)

  orig_min <- quantile(base_data$y_outcome, 0.02)
  orig_max <- quantile(base_data$y_outcome, 0.98)

  if (orig_min == orig_max && orig_min == 0) {
    orig_max <- max(base_data$y_outcome)
  }

  get_vars <-
    base_data %>%
    select(-starts_with("datascanr")) %>%
    select_if(function(x) n_distinct(x) > 1) %>%
    names()



  # map_dfr all fields
  get_fields <- map_dfr(seq_along(get_vars), agg_fields)
  field_stats <- map_dfr(seq_along(get_vars), get_stats)

  agg_data <-
    get_fields %>%
    filter(!is.na(.data$value)) %>%
    left_join(field_stats) %>%
    mutate(
      group_avg = change_range(.data$group_avg, orig_min, orig_max),
      grand_avg = grand_avg#,
      #group_dist = .data$group_avg - .data$grand_avg
      # value_diff = group_avg - grand_avg,
      # abs_value_diff = abs(value_diff)
    ) %>%
    group_by(.data$field) %>%
    filter(max(row_number()) > 1) %>%
    # mutate(
      # field_variance = var(group_avg),
      # extreme_group = max(abs(group_avg)),
      # field_range = max(.data$group_avg) - min(.data$group_avg)
    # ) %>%
    ungroup() %>%
    mutate(
      field_wt = abs(.data$field_r_sq_adj),
      field = fct_reorder(.data$field, .data$field_wt, .fun = max, .desc = TRUE)
      #field_wt_old = .data$field_range / max(.data$field_range),
    )

  if (missing(return)) {
    x_is <- "data"
  } else {
    x_is <- match.arg(return)
  }

  if (x_is == "data") {
    x <- agg_data
  } else {
    x <-
      list(
        data = agg_data,
        field_stats = field_stats,
        value_stats = get_fields,
        grand_avg = grand_avg,
        orig_min = orig_min,
        orig_max = orig_max
      )
  }

  x
}


agg_fields <- function(i) {
  # i = 1
  base_data %>%
    select(value = i, .data$y_outcome) %>%
    mutate(
      field = names(base_data)[i],
      value = as.character(.data$value)
    ) %>%
    group_by(.data$field, .data$value) %>%
    mutate(
      group_avg = avg(.data$y_outcome)#,
      #group_var = var(y_outcome),
      #group_sd = sd(.data$y_outcome)
    ) %>%
    group_by(
      .data$field, .data$value, .data$y_outcome,
      .data$group_avg#, .data$group_sd, .data$group_var
    ) %>%
    summarise(n = n()) %>%
    group_by(
      .data$field, .data$value,
      .data$group_avg#, .data$group_sd, .data$group_var
    ) %>%
    summarise(n = sum(.data$n)) %>%
    ungroup() %>%
    filter(.data$n > 5)
}


get_stats <- function(i) {
  base_data %>%
    select(value = get_vars[i], .data$y_outcome) %>%
    gather(field, value, -.data$y_outcome) %>%
    rename(y = .data$y_outcome) %>%
    group_by(.data$field) %>%
    do(glance(lm(.data$y ~ .data$value, data = .))) %>%
    ungroup() %>%
    mutate(field = get_vars[i]) %>%
    select(
      .data$field,
      field_r_sq = .data$r.squared,
      field_r_sq_adj = .data$adj.r.squared,
      field_p_value = .data$p.value
    )
}
