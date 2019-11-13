test_that("change_range works", {
  actual <- 1
  expected <- 1
  expect_equal(actual, expected)
})

test_that("check_num_cat works", {
  # x is numeric and the # of distinct vals > n_quantile
  expect_true(check_num_cat(x = 1:10, n_quantile = 3))
  expect_false(check_num_cat(x = 1:10, n_quantile = 30))
})

test_that("cut_custom works", {
  actual <- unique(cut_custom(10:20, 2))
  vals <- c("[9.99,15)", "[15,20.01]")
  expected <- factor(vals, levels = vals, ordered = TRUE)
  expect_equal(actual, expected)
})

test_that("cut_custom adds order", {
  actual <- unique(cut_custom(10:20, 2, order = TRUE))
  expected <- c("(01) [9.99,15)", "(02) [15,20.01]")
  expect_equal(actual, vals)
})

test_that("collapse_cat works", {
  actual <- unique(collapse_cat(letters, 3))
  expected <- c("a", "b", "c", "Other (23)")
  expect_equal(actual, expected)
})
