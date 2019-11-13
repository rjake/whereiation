#' Convert data to a table of factors
#'
#' @inheritParams variation_plot
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter pull mutate select one_of row_number mutate_if bind_cols
#' @importFrom lubridate is.Date is.POSIXct
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
    bind_cols(keep_cols %>% select(starts_with("datascanr")))
}
#refactor_columns(mpg, "hwy")


#' Summarize fields
#'
#' Pivots data and summarizes factor frequencies by field and generates stats
#' used for plotting
#'
#' @inheritDotParams variation_plot
#'
#' @importFrom dplyr select starts_with mutate group_by summarise n ungroup row_number filter
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
summarize_factors <- function(..., avg_type = c("mean", "median")) {
  if(missing(avg_type)) {
    avg_name <- "mean"
  } else {
    avg_name <- match.arg(avg_type)
  }

  base_data <-
    refactor_columns(..., avg_type = avg_name) %>%
    filter(!is.na(datascanr_outcome))

  avg <- eval(parse(text = avg_name))

  grand_avg <- avg(base_data$datascanr_outcome)

  orig_min <- boxplot.stats(base_data$datascanr_outcome)$stats[1]
  orig_max <- boxplot.stats(base_data$datascanr_outcome)$stats[5]

  get_vars <-
    names(base_data %>% select(-starts_with("datascanr")))

  agg_fields <- function(i) {
    # i = 1
    base_data %>%
      select(value = i, datascanr_outcome) %>%
      mutate(
        field = names(base_data)[i],
        value = as.character(value)
      ) %>%
      group_by(field, value) %>%
      mutate(
        group_avg = avg(datascanr_outcome),
        # group_var = var(datascanr_outcome),
        group_sd = sd(datascanr_outcome)
      ) %>%
      group_by(field, value, datascanr_outcome, group_avg, group_sd) %>%
      summarise(n = n()) %>%
      group_by(field, value, group_avg, group_sd) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      filter(n > 5)
  }

  # map_dfr all fields
  get_fields <- map_dfr(seq_along(get_vars), agg_fields)

  get_fields %>%
    filter(!is.na(value)) %>%
    mutate(
      group_avg = change_range(group_avg, orig_min, orig_max),
      grand_avg = grand_avg,
      group_dist = group_avg - grand_avg
      # value_diff = group_avg - grand_avg,
      # abs_value_diff = abs(value_diff)
    ) %>%
    group_by(field) %>%
    filter(max(row_number()) > 1) %>%
    mutate(
      # field_variance = var(group_avg),
      # extreme_group = max(abs(group_avg)),
      field_range = max(group_avg) - min(group_avg)
    ) %>%
    ungroup() %>%
    mutate(
      field = fct_reorder(field, field_range, .fun = max, .desc = T),
      field_wt = field_range / max(field_range)
    )
}


#' Generate stats for each observation at the factor level
#'
#' The dataset returned will be the length of the # of columns x # of rows
#'
#' @inheritDotParams variation_plot
#'
#' @importFrom tidyr gather
#' @importFrom dplyr mutate left_join filter arrange desc group_by ungroup
#' @importFrom grDevices boxplot.stats
calculate_factor_stats <- function(...) {
  base_data <-
    refactor_columns(...) %>%
    filter(!is.na(datascanr_outcome))

  group_stats <- summarize_factors(...)

  suppressWarnings(
    base_data %>%
      gather(field, value, -c(datascanr_id, datascanr_outcome)) %>%
      mutate(value = as.character(value)) %>%
      left_join(group_stats, by = c("field", "value")) %>%
      filter(complete.cases(.)) %>%
      arrange(desc(field_wt)) %>%
      mutate(group_dist_wt = group_dist * field_wt) %>%
      group_by(datascanr_id) %>%
      mutate(estimate = weighted.mean(group_avg, field_wt)) %>%
      ungroup()
  )
}
