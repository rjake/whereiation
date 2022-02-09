test_that("plot_deltas works", {
  p <- plot_deltas(iris, Petal.Length)

  actual_dim <- dim(p$data)
  expected_dim <- c(28, 12)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 3
  expect_equal(actual_layers, expected_layers)
})

test_that("plot_deltas accepts params", {
  p <-
    plot_deltas(
      df = ggplot2::mpg,
      dv = hwy,
      trunc_length = 15
    )

  df <- p$data

  expected_dim <- c(50, 12)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)

  df$value %>%
    clean_labels() %>%
    nchar() %>%
    max() %>%
    expect_equal(expected = 15)
})
