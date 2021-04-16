test_that("group_split works", {
  p <- plot_group_split(ggplot2::mpg, split_on = "year", dep_var = "hwy")

  actual_dim <- dim(p$data)
  expected_dim <- c(27, 16)
  expect_equal(actual_dim, expected_dim)

  actual_layers <- length(p$layers)
  expected_layers <- 4
  expect_equal(actual_layers, expected_layers)

  # no more than 1 pt per facet + y
  gg_build <- ggplot2::ggplot_build(p)$data[[3]]
  n_points <- ave(gg_build$y, paste(gg_build$PANEL, gg_build$y), FUN = length)
  expect_true(max(n_points) == 1)
})


test_that("group_split uses labels", {
  p1 <-
    plot_group_split(
      ggplot2::mpg,
      split_on = "year",
      dep_var = "hwy",
      n_field = 5,
      n_cat = 5,
      title = "test title",
      subtitle = "test subtitle",
      caption = 'test_caption'
    )

  p2 <- plot_group_split(ggplot2::mpg, split_on = "year", dep_var = "hwy", n_field = 5, n_cat = 5)

  p1_labs <- p1$labels
  p2_labs <- p2$labels


  expect_true(p1_labs$title != p2_labs$title)
  expect_true(p1_labs$subtitle != p2_labs$subtitle)
  expect_true(p1_labs$caption != p2_labs$caption)

  # subtitle becomes 3 lines
  expect_true(
    length(stringr::str_extract_all(p1_labs$subtitle, "\\n")[[1]]) == 2
  )

  # caption becomes 2 lines
  expect_true(
    length(stringr::str_extract_all(p1_labs$caption, "\\n")[[1]]) == 1
  )
})

