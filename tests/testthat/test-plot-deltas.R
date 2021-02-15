test_that("plot_deltas works", {
  p <- plot_deltas(iris, "Petal.Length")

  actual_dim <- dim(p$data)
  expected_dim <- c(28, 12)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 3
  expect_equal(actual_layers, expected_layers)
})
