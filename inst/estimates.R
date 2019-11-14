use_measures <- c("outcome", "estimate", "rescale_estimate")

initial_estimates <-
  compare_values %>%
  group_by(unique_id, outcome) %>%
  summarise(estimate = avg(group_avg + value_diff)) %>%
  ungroup() %>%
  mutate(
    rescale_estimate = change_range(estimate, min_outcome, max_outcome),
    # rescale_binary = as.integer(rescale_estimate > middle_outcome)*100,
    error = rescale_estimate - outcome,
    abs_error = abs(error)
  ) %>%
  arrange(abs_error)

initial_estimates %>%
  select(use_measures) %>%
  gather() %>%
  mutate(key = factor(key,
    levels = use_measures,
    labels = use_measures
  )) %>%
  ggplot(aes(value, fill = key, color = key)) +
  facet_grid(key ~ ., scales = "free") +
  geom_histogram(color = "white", bins = 50) +
  plot_grand_avg + plot_theme +
  scale_fill_manual(values = c("grey40", "darkblue", "darkred")) +
  labs(title = "can we shift everything at the 50% line?")




all_obs <-
  compare_values %>%
  # filter(unique_id %in% c(get_id)) %>%
  mutate(x_n = value_diff * n) %>%
  group_by(unique_id, outcome) %>%
  mutate(
    obs_avg = avg(group_avg),
    group_dist = group_avg - obs_avg,
    group_dist_wt = group_dist * field_wt,
    obs_wt = obs_avg + group_dist_wt,
    obs_estimate = avg(obs_wt)
  ) %>%
  ungroup() %>%
  select(
    unique_id, field, value, n, group_avg, obs_avg,
    group_dist, field_wt, group_dist_wt, obs_wt, obs_estimate, outcome
  )

all_obs_estimates <-
  all_obs %>%
  group_by(unique_id, outcome, obs_estimate) %>%
  summarise(total_n = avg(n)) %>%
  ungroup() %>%
  mutate(
    predict_side = obs_estimate > grand_avg,
    actual_side = outcome > grand_avg,
    same_side = sign(obs_estimate - grand_avg) == sign(outcome - grand_avg)
  )

rm(all_obs)

hist(all_obs_estimates$obs_estimate, breaks = 100)

ggplot(all_obs_estimates, aes(obs_estimate, total_n, color = same_side)) +
  facet_grid("actual"+actual_side~"predict") +
  plot_theme + plot_grand_avg +
  geom_count(alpha = 0.5) +
  labs(
    x = "estimate",
    y = "avg group size",
    title = "Confusion Matrix"
  )

spec_sens <-
  all_obs_estimates %>%
  count(actual_side, predict_side) %>%
  mutate(
    actual_side = glue::glue("is_{actual_side}"),
    predict_side = glue::glue("guess_{predict_side}")
  )

# pctable <- as.table(matrix(spec_sens$n, nrow = 2, byrow = TRUE))
# fourfoldplot(ctable,
#             color = c("#CC6666", "#99CC99"),
#             conf.level = 0, margin = 1,
#             main = "Confusion Matrix")

spec <-
  spec_sens %>%
  spread(predict_side, n) %>%
  mutate(
    pct_neg = guess_FALSE / (guess_TRUE + guess_FALSE),
    pct_pos = guess_TRUE / (guess_TRUE + guess_FALSE)
  ) %>%
  mutate(
    measure = c("Spec", "Sens"),
    score = c(first(pct_neg), last(pct_pos))
  ) %>%
  select(1:3, measure, score, everything())


ppv <-
  spec_sens %>%
  spread(actual_side, n) %>%
  mutate(
    pct_neg = is_FALSE / (is_TRUE + is_FALSE),
    pct_pos = is_TRUE / (is_TRUE + is_FALSE)
  ) %>%
  mutate(
    measure = c("NNV", "PPV"),
    score = c(first(pct_neg), last(pct_pos))
  ) %>%
  select(1:3, measure, score, everything())

rbind(
  spec %>% select(measure, score),
  ppv %>% select(measure, score)
) %>%
  arrange(desc(score))

spec
ppv





# relative difference: takes value into account
# 15 would be 50% below 30 (15/30)
# 65 would be 50% above than 30 (30 + (100-30)/2)
rel_diff <-
  function(val, ref) {
    ifelse(val < ref,
      (val - ref) / ref,
      (val - ref) / (100 - ref)
    )
  }

slope <-
  function(y2, y1, x2, x1) {
    (y2 - y1) / (x2 - x1)
  }

distance <-
  function(x2, x1, y2, y1) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }

round_down <-
  function(x, accuracy, integer = F) {
    x_sign <- sign(x)
    x_int <- abs(as.integer(x))
    x_dec <- abs(x) - x_int

    if (integer == F) {
      get_round <- (x_dec %/% accuracy) * accuracy
      final <- (x_int + get_round) * x_sign
    } else {
      get_round <- (x_int %/% accuracy) * accuracy
      final <- (get_round) * x_sign
    }

    return(final)
  }



one_obs_weight <-
  one_obs_profile %>%
  mutate(
    avg_predict = avg(group_avg),
    avg_field_var = avg(field_variance),
    keep =
      case_when(
        group_avg == min(group_avg) ~ "min_x",
        group_avg == max(group_avg) ~ "max_x",
        field_variance == min(field_variance) ~ "min_y",
        field_variance == max(field_variance) ~ "max_y",
        TRUE ~ "remove"
      )
  ) %>%
  filter(keep != "remove") %>%
  # mutate(weight = field_variance/max_variance,
  #       est_x = prediction * weight)
  mutate(
    avg_x = grand_avg,
    avg_y = avg(field_variance)
  ) %>%
  mutate(
    diff_prediction = group_avg - avg_x,
    diff_field_variance = field_variance - avg_y,
    pull_x = field_variance / max(field_variance),
    pull_y = group_avg / max(group_avg)
  ) %>%
  # mutate(adj_x = pull_x*diff_prediction,
  #        adj_y = pull_y*diff_field_variance) %>%

  mutate(
    adj_x = grand_avg + (pull_x * diff_prediction),
    adj_y = avg(field_variance) + (pull_y * diff_field_variance)
  ) %>%
  group_by(unique_id) %>%
  summarise(
    est_x = avg(ifelse(str_detect(keep, "_x"), adj_x, NA_real_), na.rm = T),
    est_y = avg(ifelse(str_detect(keep, "_y"), adj_y, NA_real_), na.rm = T)
  ) %>%
  # est_x = avg_x + avg(adj_x),est_y = avg_y + avg(adj_y)) %>%
  mutate_if(is.numeric, function(x) round(x, 2))
