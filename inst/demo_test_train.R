df <- survival::flchain; dep_var <- "death"; ignore_cols <- "chapter";
df <- ggplot2::mpg; dep_var <- "cty"; ignore_cols <- NA_character_;
df <- iris; dep_var <- "Petal.Width"; ignore_cols <- NA_character_;
df <- iris; dep_var <- "Petal.Length"; ignore_cols <- NA_character_;

base_data <- refactor_columns(df, dep_var, ignore_cols = ignore_cols)

df_train <- sample_frac(base_data, 0.8)
df_test <- setdiff(base_data, df_train)

df_estimate <-
  calculate_factor_stats(df_test, df_train) %>%
  distinct(
    datascanr_id,
    complete,
    datascanr_outcome,
    estimate,
    grand_avg
  ) %>%
  mutate(
    off_by = estimate - datascanr_outcome,
    estimate_direction = ifelse(estimate > grand_avg, "above", "below"),
    original_direction = ifelse(datascanr_outcome > grand_avg, "above", "below"),
    side_correct = estimate_direction == original_direction
      # sign(datascanr_outcome - grand_avg) == sign(estimate - grand_avg)
  )


ggplot(df_estimate) +
  geom_count(aes(datascanr_outcome, estimate, color = side_correct), alpha = 0.3) +
  geom_vline(aes(xintercept = grand_avg), linetype = "dotted") +
  geom_abline() +
  theme_minimal() +
  coord_equal() +
  labs(x = "original", y = "estimate")


ggplot(df_estimate) +
  geom_count(aes(datascanr_outcome, off_by, color = side_correct), alpha = 0.3) +
  geom_vline(aes(xintercept = grand_avg)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  coord_equal() +
  labs(x = "original", y = "estimate")


spec_sens <-
  df_estimate %>%
  count(estimate_direction, original_direction) %>%
  mutate(
    estimate_direction = paste0("guess_", estimate_direction),
    original_direction = paste0("is_", original_direction)
  )


spec <-
  spec_sens %>%
  spread(estimate_direction, n, fill = 0) %>%
  mutate(
    pct_above = guess_above / (guess_above + guess_below),
    pct_below = guess_below / (guess_above + guess_below)
  ) %>%
  mutate(
    measure = c("Spec", "Sens"),
    score = c(first(pct_above), last(pct_below))
  ) %>%
  select(1:3, measure, score, everything())


ppv <-
  spec_sens %>%
  spread(original_direction, n, fill = 0) %>%
  mutate(
    pct_above = is_above / (is_above + is_below),
    pct_below = is_below / (is_above + is_below)
  ) %>%
  mutate(
    measure = c("PPV", "NNV"),
    score = c(first(pct_above), last(pct_below))
  ) %>%
  select(1:3, measure, score, everything())


rbind(
  spec %>% select(measure, score),
  ppv %>% select(measure, score)
) %>%
  arrange(desc(score))

spec
ppv



