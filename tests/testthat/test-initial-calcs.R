test_that("refactor_columns works", {
  df <- refactor_columns(iris, "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(150, 6)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <- c("y_outcome", "y_id", names(iris)[c(1:2, 4:5)])

  expect_equal(actual_names, expected_names)

})


test_that("analyze_data works", {
  df <- analyze_data(iris, "Petal.Length")

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


test_that("analyze_data returns list", {
  x <-
    analyze_data(
      iris,
      dep_var = "Petal.Length",
      return = "list"
    )

  expect_true(is.list(x))
})

test_that("calculate_factor_stats works", {
  df <- calculate_factor_stats(df = iris, dep_var = "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(586, 14)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <-
    c("datascanr_outcome", "datascanr_id", "field", "value",
      "group_avg", "n",
      "field_r_sq", "field_r_sq_adj", "field_p_value",
      "grand_avg",
      "field_wt", "complete",
      "group_avg_wt",
      "estimate")

  expect_equal(actual_names, expected_names)
})

