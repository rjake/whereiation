test_that("generate_estimate_details works", {
  df <- generate_estimate_details(df = iris, dep_var = "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(586, 16)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <-
    c("y_outcome", "unique_id", "field", "value",
      "factor_avg", "n",
      "field_r_sq", "field_r_sq_adj", "field_p_value",
      "rescale_factor_avg",
      "grand_avg",
      "field_wt", "complete",
      "factor_avg_wt",
      "estimate",
      "rescale_estimate")

  expect_equal(actual_names, expected_names)
})

