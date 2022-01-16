df <- survival::flchain; dv <- "death"; ignore_cols <- "chapter";
df <- ggplot2::mpg; dv <- "cty"; ignore_cols <- NA_character_;
df <- iris; dv <- "Petal.Width"; ignore_cols <- NA_character_;
df <- iris; dv <- "Petal.Length"; ignore_cols <- NA_character_;
df <- read.csv("inst/extdata/kaggle_breast_cancer.csv"); dv <- "diagnosis == 'M'"; ignore_cols <- NA_character_;
df <- read.csv("inst/extdata/kaggle_housing.csv"); dv <- "SalePrice"; ignore_cols <- NA_character_;
df <- read.csv("inst/extdata/cognoma_samples.csv"); dv <- "dead"; ignore_cols <- "acronym";

base_data <- refactor_columns(df, dv, ignore_cols = ignore_cols)

set.seed(1234)
df_train <- base_data %>% sample_frac(base_data, 0.8)
df_test <- setdiff(base_data, df_train)

df_estimate <-
  generate_estimate_details(df, dv = dv) %>%
  distinct(
    unique_id,
    complete,
    y_outcome,
    estimate,
    grand_avg
  ) %>%
  filter(!is.na(y_outcome)) %>%
  mutate(
    off_by = estimate - y_outcome,
    estimate_direction = ifelse(estimate > grand_avg, "above", "below"),
    original_direction = ifelse(y_outcome > grand_avg, "above", "below"),
    side_correct = estimate_direction == original_direction
      # sign(whereiation_outcome - grand_avg) == sign(estimate - grand_avg)
  )


ggplot(df_estimate) +
  geom_count(aes(y_outcome, estimate, color = side_correct), alpha = 0.8) +
  geom_vline(aes(xintercept = grand_avg), linetype = "dotted") +
  geom_abline() +
  theme_minimal() +
  labs(x = "original", y = "estimate")


ggplot(df_estimate) +
  geom_count(aes(y_outcome, off_by, color = side_correct), alpha = 0.8) +
  geom_vline(aes(xintercept = grand_avg)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
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



