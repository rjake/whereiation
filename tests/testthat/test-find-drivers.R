test_that("find_drivers works", {
  df <- find_drivers(df = iris, dep_var = "Petal.Length")
  expected_dim <- c(4, 9)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})


