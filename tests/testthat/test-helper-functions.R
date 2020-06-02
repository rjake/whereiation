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
  actual <- unique(collapse_cat(letters, 3))
  expected <- c("a", "b", "c", "Other (23)")
  expect_equal(actual, expected)
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
