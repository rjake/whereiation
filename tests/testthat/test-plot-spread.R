test_that("plot_spread works", {
  p <- plot_spread(iris, dv = Petal.Length)

  actual_dim <- dim(p$data)
  expected_dim <- c(28, 11)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 3
  expect_equal(actual_layers, expected_layers)
})


test_that("plot_spread uses weights", {
  p_dv <- plot_spread(iris, Petal.Length, n_cat = 4, collapse_by = "dv")
  p_n <- plot_spread(iris, Petal.Length, n_cat = 4, collapse_by = "n")

  # tables are different
  expect_false(identical(p_dv, p_n))

  # fields closest to grand mean are grouped
  min_dv <-
    p_dv$data %>%
    dplyr::filter(
      field == "Sepal.Length",
      abs(factor_avg - grand_avg) == min(abs(factor_avg - grand_avg))
    )
  expect_true(grepl("Other", min_dv$value))

  # the oter field has the least frequent values
  min_n <-
    p_n$data %>%
    dplyr::filter(
      field == "Sepal.Length",
      grepl("Other", value)
    )

  ref_n <-
    refactor_columns(iris, Petal.Length, n_cat = NULL) %>%
    dplyr::count(Sepal.Length, sort = TRUE) %>%
    dplyr::slice(-c(1:4))


  expect_equal(
    object = min_n$n[1],
    expected = sum(ref_n$n)
  )
})


test_that("plot_spread_single_obs works", {
  p1 <- plot_spread_single_obs(df = iris, dv = Petal.Length, avg_type = "mean")
  p2 <- plot_spread_single_obs(df = iris, dv = Petal.Length, labels = TRUE)
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
