test_that("check_cut_numeric works", {
  # x is numeric and the # of distinct vals > n_quantile
  expect_true(check_cut_numeric(x = 1:10, n_quantile = 3))
  expect_false(check_cut_numeric(x = 1:10, n_quantile = 30))
})


test_that("cut_custom works", {
  actual <- unique(cut_custom(10:20, 2, n_digits = 1))
  expected <- c("01 [9.9 to 15)", "02 [15 to 20.0]")
  expect_equal(actual, expected)
})


test_that("collapse_cat works", {
  df <-
    data.frame(
      x = letters[c(1, 2, 2, 2, 3, 4, 5, 5)],
      y = letters[1:8],
      wt = c(10, 1, 1, 1, 1, 30, 1, 1),
      stringsAsFactors = FALSE
    )

  wt <- df$wt

  df_count <- df %>% mutate(x2 = collapse_cat(x, n = 2, w = NULL))
  df_weight  <- df %>% mutate(x2 = collapse_cat(x, n = 2, w = wt))
  outside_weight <- df %>% mutate(x2 = collapse_cat(x, n = 2, w = wt))
  cond_weight <- df %>% mutate_if(is.character, collapse_cat, n = 2, w = wt)


  expect_equal(
    object = unique(df_count$x2),
    expected = c("Other (3)", "b", "e")
  )

  expect_equal(
    object = unique(df_weight$x2),
    expected = c("a", "Other (3)", "d")
  )

  expect_equal(
    object = unique(outside_weight$x2),
    expected = c("a", "Other (3)", "d")
  )

  expect_equal(
    object = unique(cond_weight$x),
    expected = c("a", "Other (3)", "d")
  )

  expect_equal(
    object = unique(cond_weight$y),
    expected = c("a", "Other (6)", "f")
  )

})


test_that("check_01_binary throws warnings/error", {
  # allowed
  expect_invisible(check_01_binary(c(0, 1, NA)))
  expect_invisible(check_01_binary(c(TRUE, TRUE, FALSE)))
  # not allowed
  # not 2 integers
  expect_warning(check_01_binary(c(1, NA)))
  expect_warning(check_01_binary(c(0, NA)))
  expect_warning(check_01_binary(c(1, NA)))
  # not integers
  expect_warning(check_01_binary(runif(10)))
  expect_warning(check_01_binary(as.Date("2020-01-01") + 1:10))
  # no
  expect_error(check_01_binary("a"))
})
