test_that("identify_factor_extremes works", {
  df <- identify_factor_extremes(df = iris, dep_var = "Petal.Length")
  expected_dim <- c(4, 9)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})


