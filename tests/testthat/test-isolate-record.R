test_that("isolate_record works", {
  df <- isolate_record(df = iris, dv = Petal.Length)

  actual_dim <- dim(df)
  expected_dim <- c(4, 11)
  expect_equal(actual_dim, expected_dim)
})

