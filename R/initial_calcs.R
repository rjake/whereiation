library(tidyverse)
library(ggrepel)

options(digits = 3)

change_range <- function(x, new_min, new_max) {
  (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
}


check_cut <- function(x) {
  (is.numeric(x) | is.integer(x)) & n_distinct(x) > 15
}


check_n_cat <- function(x) {
  n <- n_distinct(x)

  n != 1 &
    (is.character(x) | is.factor(x)) &
    n < 100 &
    n < length(x)
}

cut_custom <- function(x) {
  n <- 10
  cut(
    x,
    breaks = unique(quantile(x, probs = 0:n / n, na.rm = TRUE)),
    include.lowest = TRUE,
    dig.lab = 10
  )
}



refactor_columns <- function(df, dep_var, avg_type, ignore_cols = NA) {
  avg <- eval(parse(text = avg_type))
  
  dv_name <-
    dep_var %>%
    gsub(pattern = " .*", replacement = "") %>%
    gsub(pattern = "\\(", replacement = "")
  
  column_summary <-
    tibble(
      names = names(df),
      class = sapply(df, class),
      cut = sapply(df, check_cut),
      n_cat = sapply(df, check_n_cat)
    ) %>%
    filter(
      !names %in% ignore_cols,
      (cut == TRUE | n_cat == TRUE)
    )


  vars_cut <-
    column_summary %>%
    filter(names != dv_name, cut == TRUE) %>%
    pull(names)

  # vars_keep <-
  #   column_summary %>%
  #   filter(names %in% vars_cut) %>%
  #   pull(names)

  suppressWarnings(
    df %>%
      mutate(outcome = eval(parse(text = dep_var))) %>% # get(set_dv)
      filter(!is.na(outcome)) %>%
        select(-one_of(c(ignore_cols, dv_name))) %>%
      mutate_at(vars_cut, cut_custom) %>%
      # select(one_of(vars_keep), outcome) %>%
      # mutate(outcome = ifelse(max(outcome) == 1, outcome*100, outcome)) %>%
      mutate(unique_id = row_number())
  )
}


summarize_factors <- function(df, dep_var, avg_type, ignore_cols = NA){
  
  base_data <- refactor_columns(df, dep_var, avg_type, ignore_cols)
  avg <- eval(parse(text = avg_type))
  
  grand_avg <- avg(base_data$outcome)

  get_vars <-
    names(base_data %>% select(-c(unique_id, outcome)))

  agg_fields <- function(i) {
    # i = 1
    base_data %>%
      select(value = i, outcome) %>%
      mutate(
        field = names(base_data)[i],
        value = as.character(value)
      ) %>%
      group_by(field, value) %>%
      mutate(
        group_avg = avg(outcome),
        # group_var = var(outcome),
        group_sd = sd(outcome)
      ) %>%
      group_by(field, value, outcome, group_avg, group_sd) %>%
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

calculate_factor_stats <- function(df, dep_var, avg_type, ignore_cols = NA) {
  avg <- eval(parse(text = avg_type))
  base_data <- refactor_columns(df, dep_var, avg_type, ignore_cols)
  group_stats <- summarize_factors(df, dep_var, avg_type, ignore_cols)
  
  suppressWarnings(
    #compare_values <-
      base_data %>%
      gather(field, value, -c(unique_id, outcome)) %>%
      mutate(value = as.character(value)) %>%
      left_join(group_stats, by = c("field", "value")) %>%
      filter(complete.cases(.)) %>% 
      arrange(desc(field_wt)) %>% 
      mutate(group_dist_wt = group_dist * field_wt) %>%
      group_by(unique_id) %>% 
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


