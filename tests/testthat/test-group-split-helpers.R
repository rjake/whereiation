test_that("summarize_over_under_split works", {
  df <-
    summarize_over_under_split(
      df = refactor_columns(ggplot2::mpg, "hwy", split_on = "year"),
      field = "cyl",
      n_cat = 5,
      type = "dv"
    )

  expected_dim <- c(4, 15)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})

test_that("summarize_over_under_split works for all types", {
  df <- refactor_columns(ggplot2::mpg, "hwy", split_on = "year")

  df_dv <-
    summarize_over_under_split(df = df, field = "cyl", n_cat = 5, type = "dv")

  expect_equal(round(weighted.mean(df_dv$x_bar, df_dv$n_bar), 1), 23.4)
  expect_equal(round(weighted.mean(df_dv$x_point, df_dv$n_point), 1), 23.5)

  df_count <-
    summarize_over_under_split(df = head(df, 100), field = "cyl", n_cat = 5, type = "count")

  expect_equal(sum(df_count$x_bar), 48)
  expect_equal(sum(df_count$x_point), 52)

  df_pct_field <-
    summarize_over_under_split(df = df, field = "cyl", n_cat = 5, type = "percent_field")

  expect_equal(sum(df_pct_field$x_bar), 100)
  expect_equal(sum(df_pct_field$x_point), 100)

  df_pct_factor <-
    summarize_over_under_split(df = df, field = "cyl", n_cat = 5, type = "percent_factor")

  expect_equal(df_pct_factor$x_bar + df_pct_factor$x_point, rep(100, 4))
})



test_that("map_over_under_split", {
  df <-
    map_over_under_split(
      ggplot2::mpg,
      dep_var = "hwy",
      split_on = "year",
      n_cat = 5,
      type = "dv"
    )

  expected_dim <- c(48, 15)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})


test_that("plot_group_split_prep", {
  df <-
    plot_group_split_prep(
      base_data =
        map_over_under_split(
          df = ggplot2::mpg,
          split_on = "year",
          type = "dv",
          n_cat = 5,
          dep_var = "hwy"
        ),
      threshold = 1,
      ref_group = "1",
      trunc_length = 20
    )

  expected_dim <- c(33, 15)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)

  expect_equal(
    object = max(nchar(as.character(df$value))),
    expected = 20
  )
})


test_that("summarize_group_split_metadata", {
  df <-
    summarize_group_split_metadata(
      base_data =
        map_over_under_split(
          df = ggplot2::mpg,
          split_on = "year",
          type = "dv",
          n_cat = 5,
          dep_var = "hwy"
        ),
      ref_group = "1",
      split_on = "year"
    )

  expected_dim <- c(2, 5)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})



test_that("check_dv_has_value", {
  expect_equal(
    object = check_dv_has_value(type = "count"),
    expected = "1"
  )

  expect_equal(
    object = check_dv_has_value(type = "percent"),
    expected = "1"
  )

  expect_equal(
    object = check_dv_has_value(type = "dv", FALSE, "x"),
    expected = "x"
  )

  expect_error(
    object = check_dv_has_value(type = "dv", TRUE),
    regexp = "required"
  )

})

