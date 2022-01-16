test_that("refactor_columns works", {
  df <- refactor_columns(iris, "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(150, 7)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <- c("y_outcome", "y_split", "unique_id", names(iris)[c(1:2, 4:5)])

  expect_equal(actual_names, expected_names)

})


test_that("summarize_factors_all_fields works", {
  df <- summarize_factors_all_fields(iris, "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(28, 10)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <-
    c("field", "value", "factor_avg",
      "n",
      "field_r_sq", "field_r_sq_adj", "field_p_value",
      "rescale_factor_avg", "grand_avg", "field_wt")

  expect_equal(actual_names, expected_names)
})


test_that("summarize_factors_all_fields returns list", {
  x <-
    summarize_factors_all_fields(
      iris,
      dv = "Petal.Length",
      return = "list"
    )

  expect_true(is.list(x))
})

