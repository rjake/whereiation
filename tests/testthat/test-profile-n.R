test_that("profile_n works", {
  top_n <- profile_n(df = iris, dep_var = "Petal.Length")
  bottom_n <- profile_n(df = iris, dep_var = "Petal.Length", position = "bottom")
  expected_n_cells <- 4 * 7
  top_n_cells <- sum(grepl("td ", top_n))
  bottom_n_cells <- sum(grepl("td ", bottom_n))
  expect_equal(length(top_n), length(bottom_n), expected_n_cells)
})


