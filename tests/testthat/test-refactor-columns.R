test_that("refactor_columns uses id", {
  df <-
    ggplot2::mpg %>%
    dplyr::mutate(id2 = 1:234) %>%
    mutate(id2 = paste(manufacturer, id2))

  no_id <- refactor_columns(df, dep_var = "cty")
  with_id <-  refactor_columns(df, dep_var = "cty", id = "id2")

  expect_equal(
    object = c(234, 13),
    expected = dim(no_id)
  )

  expect_equal(
    object = c(234, 12),
    expected = dim(with_id)
  )

})
