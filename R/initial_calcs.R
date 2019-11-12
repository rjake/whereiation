#' Convert data to a table of factors
#'
#' @param df dataframe to evaluate
#' @param dep_var dependent variable to use (column name)
#' @param avg_type mean or median
#' @param ignore_cols columns to ignore from analysis. Good candidates are
#' fields that have have no duplicate values (primary keys) or fields with
#' a large proportion of null values
#' @param n_cat for categorical variables, the max number of unique values
#' to keep. This field feeds the \code{forcats::fct_lump(n = )} argument.
#' @param n_quantile for numeric/date fields, the number of quantiles used
#' to split the data into a factor. Fields that have less than this amount
#' will not be changed.
#'
#' @return dataframe
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter pull mutate select one_of row_number mutate_if bind_cols
refactor_columns <- function(df,
                             dep_var,
                             avg_type = mean,
                             ignore_cols = NA_character_,
                             n_cat = 15,
                             n_quantile = 10) {

  avg <- avg_type

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
      mutate_if(check_num_cat, cut_custom, n_quantile = n_quantile) %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, collapse_cat, n = n_cat) %>%
      bind_cols(keep_cols %>% select(starts_with("datascanr"))) %>%
      filter(!is.na(datascanr_outcome))
}


#' Title
#'
#' @param ...
#'
#' @return
#' @inheritDotParams refactor_columns
#' @importFrom dplyr select starts_with mutate group_by summarise n ungroup row_number filter
#' @importFrom purrr map_dfr
#' @importFrom forcats fct_reorder
summarize_factors <- function(...){

  base_data <- refactor_columns(...)
  avg <- avg_type

  grand_avg <- avg(base_data$datascanr_outcome)

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

  # map_dfr all fields ----
  get_fields <- map_dfr(seq_along(get_vars), agg_fields)


  get_fields %>%
    filter(!is.na(value)) %>%
    mutate(
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


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#' @inheritDotParams refactor_columns
#' @importFrom tidyr gather
#' @importFrom dplyr mutate left_join filter arrange desc group_by ungroup
#' @importFrom grDevices boxplot.stats
#' @examples
calculate_factor_stats <- function(...) {
  avg <- avg_type
  base_data <- refactor_columns(...)
  group_stats <- summarize_factors(...)

  suppressWarnings(
    #compare_values <-
      base_data %>%
      gather(field, value, -c(datascanr_id, datascanr_outcome)) %>%
      mutate(value = as.character(value)) %>%
      left_join(group_stats, by = c("field", "value")) %>%
      filter(complete.cases(.)) %>%
      arrange(desc(field_wt)) %>%
      mutate(group_dist_wt = group_dist * field_wt) %>%
      group_by(datascanr_id) %>%
      mutate(estimate =
               #sum(group_dist_wt)
               mean(group_dist_wt)
               #mean(group_dist_wt) + grand_avg
               ) %>%
      ungroup() %>%
      mutate(
        orig_min = boxplot.stats(df[[dep_var]])$stats[1],
        orig_max = boxplot.stats(df[[dep_var]])$stats[5],
        estimate_expand = change_range(estimate, orig_min, orig_max)
      )
  )
}


