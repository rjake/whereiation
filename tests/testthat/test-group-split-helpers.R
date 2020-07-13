test_that("over_under_split", {
  df <-
    over_under_split(
      df = refactor_columns(ggplot2::mpg, "hwy", split = "year"),
      field = "class",
      n_cat = 5,
      type = "dv"
    )

  expected_dim <- c(6, 14)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})


test_that("group_split_prep", {
  df <-
    group_split_prep(
      ggplot2::mpg,
      dep_var = "hwy",
      split = "year",
      n_cat = 5,
      type = "dv"
    )

  expected_dim <- c(48, 14)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)
})


test_that("group_split_plot_data", {
  df <-
    group_split_plot_data(
      base_data =
        group_split_prep(
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

  expected_dim <- c(33, 19)
  actual_dim <- dim(df)
  expect_equal(expected_dim, actual_dim)

  expect_equal(
    object = max(nchar(as.character(df$value))),
    expected = 20
  )
})


test_that("group_split_counts", {
  df <-
    group_split_counts(
      base_data =
        group_split_prep(
          df = ggplot2::mpg,
          split_on = "year",
          type = "dv",
          n_cat = 5,
          dep_var = "hwy"
        ),
      ref_group = "1",
      split_on = "year"
    )

  expected_dim <- c(2, 6)
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

