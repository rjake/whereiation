test_that("refactor_columns works", {
  df <- refactor_columns(iris, dv = Petal.Length)

  actual_dim <- dim(df)
  expected_dim <- c(150, 7)
  expect_equal(actual_dim, expected_dim)

  actual_names <- names(df)
  expected_names <- c("y_outcome", "y_split", "unique_id", names(iris)[c(1:2, 4:5)])

  expect_equal(actual_names, expected_names)

})


test_that("summarize_factors_all_fields works", {
  df <- summarize_factors_all_fields(iris, dv = Petal.Length)

  actual_dim <- dim(df)
  expected_dim <- c(28, 9)
  expect_equal(actual_dim, expected_dim)

})


test_that("summarize_factors_all_fields returns attributes", {
  x <-
    summarize_factors_all_fields(
      iris,
      dv = Petal.Length
    )

  expect_true(!is.null(attr(x, "about")))
})

