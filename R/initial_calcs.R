#' Convert data to a table of factors
#'
#' @inheritParams variation_plot
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter pull mutate select one_of row_number mutate_if bind_cols
#' @importFrom lubridate is.Date is.POSIXct
#'
#' @export
#' @family manipulation functions
refactor_columns <- function(df,
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

  avg <- eval(parse(text = avg_name))

  dv_name <-
    dep_var %>%
    gsub(pattern = " .*", replacement = "") %>%
    gsub(pattern = "\\(", replacement = "")


  suppressWarnings(
    keep_cols <-
      df %>%
      mutate(datascanr_outcome = eval(parse(text = dep_var))) %>%
      select(-one_of(c(ignore_cols, dv_name))) %>%
      mutate(datascanr_id = row_number())
  )

  keep_cols %>%
    select(-starts_with("datascanr")) %>%
    # mutate(outcome = ifelse(max(outcome) == 1, outcome*100, outcome))
    mutate_if(~(is.Date(.) | is.POSIXct(.)), as.numeric) %>%
    mutate_if(~check_cut_numeric(., n_quantile), cut_custom, n_quantile) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.character, collapse_cat, n = n_cat) %>%
    bind_cols(keep_cols %>% select(starts_with("datascanr"))) %>%
    mutate_if(is.logical, as.integer)
}


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
#' @importFrom dplyr select starts_with mutate group_by summarise n ungroup row_number filter
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
#' @importFrom stats sd
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
    df %>%
    filter(!is.na(.data$datascanr_outcome))

  avg <- eval(parse(text = avg_name))

  grand_avg <- avg(base_data$datascanr_outcome)

  orig_min <- boxplot.stats(base_data$datascanr_outcome)$stats[1]
  orig_max <- boxplot.stats(base_data$datascanr_outcome)$stats[5]

  if (orig_min == orig_max && orig_min == 0) {
    orig_max <- max(base_data$datascanr_outcome)
  }

  get_vars <-
    names(base_data %>% select(-starts_with("datascanr")))

  agg_fields <- function(i) {
    # i = 1
    base_data %>%
      select(value = i, .data$datascanr_outcome) %>%
      mutate(
        field = names(base_data)[i],
        value = as.character(.data$value)
      ) %>%
      group_by(.data$field, .data$value) %>%
      mutate(
        group_avg = avg(.data$datascanr_outcome)#,
        #group_var = var(datascanr_outcome),
        #group_sd = sd(.data$datascanr_outcome)
      ) %>%
      group_by(
        .data$field, .data$value, .data$datascanr_outcome,
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

  # map_dfr all fields
  get_fields <- map_dfr(seq_along(get_vars), agg_fields)

  agg_data <-
    get_fields %>%
    filter(!is.na(.data$value)) %>%
    mutate(
      group_avg = change_range(.data$group_avg, orig_min, orig_max),
      grand_avg = grand_avg#,
      #group_dist = .data$group_avg - .data$grand_avg
      # value_diff = group_avg - grand_avg,
      # abs_value_diff = abs(value_diff)
    ) %>%
    group_by(.data$field) %>%
    filter(max(row_number()) > 1) %>%
    mutate(
      # field_variance = var(group_avg),
      # extreme_group = max(abs(group_avg)),
      field_range = max(.data$group_avg) - min(.data$group_avg)
    ) %>%
    ungroup() %>%
    mutate(
      field = fct_reorder(.data$field, .data$field_range, .fun = max, .desc = TRUE),
      field_wt = .data$field_range / max(.data$field_range)
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
        grand_avg = grand_avg,
        orig_min = orig_min,
        orig_max = orig_max
      )
  }

  x
}


#' Generate stats for each observation at the factor level
#'
#' The dataset returned will be the length of the # of columns x # of rows
#' @param train_data training dataset generated from summarize_factors
#' @inheritDotParams variation_plot
#' @inheritParams variation_plot
#'
#' @importFrom tidyr gather drop_na
#' @importFrom dplyr mutate left_join filter arrange desc group_by ungroup
#' @importFrom grDevices boxplot.stats
#' @importFrom stats complete.cases weighted.mean
#' @importFrom rlang .data
#'
#' @export
#' @family manipulation functions
calculate_factor_stats <- function(df, train_data, dep_var, ...) {

  if (missing(train_data)) {
    base_data <- refactor_columns(df, dep_var = dep_var, ...)
    group_stats <- summarize_factors(base_data, ..., return = "list")
  } else {
    base_data <- df
    group_stats <- summarize_factors(train_data, ..., return = "list")
  }

  group_stats_data <- group_stats$data
  orig_min <- group_stats$orig_min
  orig_max <- group_stats$orig_max


  suppressWarnings(
    base_data %>%
      gather(
        key = field, value = value,
        -c(.data$datascanr_id, .data$datascanr_outcome)
      ) %>%
      mutate(value = as.character(.data$value)) %>%
      left_join(
        group_stats_data, by = c("field", "value")
      ) %>%
      group_by(.data$datascanr_id) %>%
      mutate(complete = sum(!is.na(.data$group_avg))) %>%
      ungroup() %>%
      drop_na(.data$group_avg:.data$field_wt) %>%
      #arrange(desc(.data$field_wt)) %>%
      mutate(group_avg_wt = .data$group_avg * .data$field_wt) %>%
      group_by(.data$datascanr_id) %>%
      mutate(estimate = weighted.mean(.data$group_avg, .data$field_wt)) %>%
      ungroup() %>%
      mutate(estimate = change_range(.data$estimate, orig_min, orig_max))
  )
}
