test_that("refactor_columns works", {
  df <- refactor_columns(iris, "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(150, 6)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <-
    c(names(iris)[c(1:2, 4:5)], "datascanr_outcome", "datascanr_id")

  expect_equal(actual_names, expected_names)

})


test_that("summarize_factors works", {
  df <- summarize_factors(refactor_columns(iris, "Petal.Length"))

  actual_dim <- dim(df)
  expected_dim <- c(28, 7)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <-
    c("field", "value", "group_avg",
      #"group_sd",
      "n",
      "grand_avg", "field_range", "field_wt")

  expect_equal(actual_names, expected_names)
})


test_that("summarize_factors returns list", {
  x <-
    summarize_factors(
      refactor_columns(iris, "Petal.Length"),
      return = "list"
    )

  expect_true(is.list(x))
})

test_that("calculate_factor_stats works", {
  df <- calculate_factor_stats(df = iris, dep_var = "Petal.Length")

  actual_dim <- dim(df)
  expected_dim <- c(586, 12)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <-
    c("datascanr_outcome", "datascanr_id", "field", "value",
      "group_avg", "n", "grand_avg",
      "field_range", "field_wt", "complete",
      "group_avg_wt",
      "estimate")

  expect_equal(actual_names, expected_names)
})

