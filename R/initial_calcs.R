# WORKSPACE ----
library(tidyverse)
library(ggrepel)

options(digits = 3)

dv_name <-
  set_dv %>% 
  gsub(" .*" , "",.) %>% 
  gsub("\\(" , "",.)

# CUSTOM FUNCTIONS ----
change_range <- function(x, new_min, new_max){
  (x - min(x))/(max(x)-min(x)) * (new_max - new_min) + new_min 
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

column_summary <-
  tibble(
    names = names(df),
    class = sapply(df, class),
    cut = sapply(df, check_cut),
    n_cat = sapply(df, check_n_cat)
  ) %>%
  filter(
    !names %in% set_ignore,
    (cut == TRUE | n_cat == TRUE)
  )

cut_custom <- function(x) {
  n <- 10
  cut(
    x,
    breaks = unique(quantile(x, probs = 0:n/n, na.rm = TRUE)),
    include.lowest = TRUE,
    dig.lab = 10
  )
}

vars_cut <-
  column_summary %>% 
  filter(names != dv_name, cut == TRUE) %>% 
  pull(names)

# vars_keep <-
#   column_summary %>% 
#   filter(names %in% vars_cut) %>% 
#   pull(names)

# DATASETS ----
# * base_data ----

base_data <-
  df %>%
  mutate(outcome = eval(parse(text = set_dv))) %>% #get(set_dv) 
  filter(!is.na(outcome)) %>% 
  #filter(complete.cases(.)) %>% # !is.na(outcome)
  select(-one_of(c(set_ignore, dv_name))) %>%
  mutate_at(vars_cut, cut_custom) %>% 
  #select(one_of(vars_keep), outcome) %>% 
  #mutate(outcome = ifelse(max(outcome) == 1, outcome*100, outcome)) %>% 
  mutate(unique_id = row_number())

# GLOBAL STATS ----
grand_mean <- mean(base_data$outcome) 
min_outcome <- min(base_data$outcome)
max_outcome <- max(base_data$outcome)
middle_outcome <- mean(c(min_outcome, max_outcome))


# AGG_FIELDS ----
get_vars <- 
  names(base_data %>% select(-c(unique_id, outcome)))

agg_fields <-
  function(i){
    #i = 1 
    #a <-
    base_data %>% 
      select(value = i, outcome) %>%
      mutate(field = names(base_data)[i],
             value = as.character(value)) %>%
      group_by(field, value) %>% 
      mutate(group_mean = mean(outcome),
             group_var = var(outcome),
             group_sd = sd(outcome)) %>%   
      group_by(field, value, outcome, group_mean, group_var, group_sd) %>% 
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(sum_outcome = n * outcome) %>% 
      group_by(field, value, group_mean, group_var, group_sd) %>% 
      summarise(n = sum(n),
                sum_outcome = sum(sum_outcome)) %>% 
      ungroup() %>% 
      filter(n > 5)
  }

# map_dfr all fields ----
get_fields <- map_dfr(seq_along(get_vars), agg_fields)


get_values <-
  get_fields %>% 
  filter(!is.na(value)) %>% 
  mutate(grand_mean = grand_mean,
         value_diff = group_mean - grand_mean,
         abs_value_diff = abs(value_diff)) %>% 
  group_by(field) %>% 
  filter(max(row_number()) > 1) %>% 
  mutate(field_variance = var(group_mean),
         extreme_group = max(abs(group_mean)), 
         field_range = max(group_mean) - min(group_mean)) %>% 
  ungroup() %>% 
  mutate(field = fct_reorder(field, value_diff, .fun = max, .desc = T),
         field_wt = field_range/max(field_range))
suppressWarnings(
  compare_values <-
    base_data %>% 
    gather(field, value, -c(unique_id, outcome)) %>%
    mutate(value = as.character(value)) %>% 
    left_join(get_values, by = c("field", "value")) %>%
    filter(complete.cases(.)) 
)

max_variance <- max(get_values$field_variance)
mean_variance <- mean(get_values$field_variance)

field_ranks <-
  compare_values %>% 
  distinct(field, field_variance, field_range, field_wt, extreme_group) %>% 
  arrange(desc(field_variance)) %>% 
  mutate(rank_var = factor(row_number())) %>% 
  arrange(desc(field_range)) %>% 
  mutate(rank_range = factor(row_number())) %>% 
  arrange(desc(extreme_group)) %>% 
  mutate(rank_extreme = factor(row_number()))

group_value_ranks <-
  get_values %>% 
  #left_join(field_ranks) %>% 
  mutate(
    field = fct_reorder(field, field_range, max),
    label = paste(field, ": \n",value)
  )

# PLOT ELEMENTS ----
plot_grand_mean <- 
  geom_vline(xintercept = grand_mean, color = "grey60", size = 1, linetype = "dotted")

plot_theme <-    
  theme(panel.background = element_rect(fill = "white", color = "grey60"))
