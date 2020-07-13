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


test_that("refactor_columns uses weights correctly", {
  df <-
    ggplot2::mpg %>%
    select(cty, manufacturer)

  use_dv <- refactor_columns(df, dep_var = "cty", n_cat = 3, collapse_by = "dv")
  use_n <-  refactor_columns(df, dep_var = "cty", n_cat = 3, collapse_by = "n")

  table(df$manufacturer) %>% sort()

  expect_equal(
    object = unique(use_dv$manufacturer),
    expected = c("Other (12)", "honda", "land rover", "lincoln")
  )

  expect_equal(
    object = unique(use_n$manufacturer),
    expected = c("Other (12)", "dodge", "toyota", "volkswagen")
  )
})
