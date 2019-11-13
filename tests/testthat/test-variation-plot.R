test_that("variation_plot works", {
  p <- variation_plot(iris, "Petal.Length")

  actual_dim <- dim(p$data)
  expected_dim <- c(28, 10)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 3
  expect_equal(actual_layers, expected_layers)
})


test_that("variation_plot_single_obs works", {
  p <- variation_plot_single_obs(iris, "Petal.Length")

  actual_dim <- dim(p$data)
  expected_dim <- c(28, 10)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 6
  expect_equal(actual_layers, expected_layers)
})
