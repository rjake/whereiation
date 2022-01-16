test_that("summarize_factor_extremes works", {
  df <- summarize_factor_extremes(df = iris, dv = "Petal.Length")
  expected_dim <- c(4, 9)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})


