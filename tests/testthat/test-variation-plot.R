test_that("variation_plot works", {
  p <- variation_plot(iris, "Petal.Length")

  actual_dim <- dim(p$data)
  expected_dim <- c(28, 8)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 3
  expect_equal(actual_layers, expected_layers)
})


test_that("variation_plot_single_obs works", {
  p1 <- variation_plot_single_obs(df = iris, dep_var = "Petal.Length", avg_type = "mean")
  p2 <- variation_plot_single_obs(iris, "Petal.Length", labels = TRUE)
  actual_dim_p1 <- dim(p1$data)
  actual_dim_p2 <- dim(p2$data)
  expected_dim <- c(28, 10)
  expect_equal(actual_dim_p1, actual_dim_p2, expected_dim)

  actual_layers_p1 <- length(p1$layers)
  expected_layers_p1 <- 6
  expect_equal(actual_layers_p1, expected_layers_p1)

  actual_layers_p2 <- length(p2$layers)
  expected_layers_p2 <- 7
  expect_equal(actual_layers_p2, expected_layers_p2)
})
