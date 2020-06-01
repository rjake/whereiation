simple_p <-
  expected_proportions(
    df = mtcars,
    dep_var = "mpg > 15"
  )

complex_p <-
  expected_proportions(
    df = mtcars,
    dep_var = "mpg > 15",
    n_cat = 5,
    n_field = 3,
    threshold = NULL,
    color_over = "orange",
    color_under = "green",
    trunc_length = 10,
    sort_by = "actual"
  )

simple_p_build <- ggplot2::ggplot_build(simple_p)
complex_p_build <- ggplot2::ggplot_build(complex_p)

simple_p_data <- dplyr::filter(simple_p$data, field == "hp")
complex_p_data <- dplyr::filter(complex_p$data, field == "hp")


test_that("over_under_rep categorizes correctly", {
  df <-
    over_under_rep(
      df = refactor_columns(iris, dep_var = "Sepal.Length > 5"),
      "Species"
    )
  expect_equal(
    object = df$category,
    expected = c("under", "over", "over")
  )
})


test_that("expected_prop_prep creates new fields", {
  df <- expected_prop_prep(df = iris, dep_var = "Sepal.Length > 5")
  expect_true(all(c("expected", "actual", "category") %in% names(df)))
})


test_that("expected_prop_prep throws warning for non-binary DV", {
  # Sepal.Length is not binary
  expect_warning(expected_prop_prep(df = iris, dep_var = "Sepal.Length"))
})



test_that("returns data", {
  df <-
    expected_proportions(
      df = mtcars,
      dep_var = "mpg > 15",
      return_data = TRUE
    )

  expect_true("tbl" %in% class(df))
})


test_that("returns plot", {
  expect_true("gg" %in% class(simple_p))
})


test_that("has layers in right order", {
  # right layers
  geom_layers <-
    simple_p_build$plot$layers %>%
    purrr::map(pluck, "geom") %>%
    purrr::map(class) %>%
    purrr::map_chr(pluck, 1)

  expect_equal(
    object = geom_layers,
    expected = c("GeomCol", "GeomSegment", "GeomPoint", "GeomPoint")
  )
})




test_that("facets responsive", {
  expect_true(dplyr::n_distinct(simple_p_build$data[[1]]$PANEL) == 9)
  expect_true(dplyr::n_distinct(complex_p_build$data[[1]]$PANEL) == 3)
})


test_that("distinct values responsive to 6 (5 + other)", {
  expect_true(dplyr::n_distinct(simple_p_data$value) == 7)
  expect_true(dplyr::n_distinct(complex_p_data$value) == 6)
})


test_that("captions reflect threshold", {
  expect_true(stringr::str_detect(simple_p_build$plot$labels$caption, "2%"))
  expect_true(complex_p_build$plot$labels$caption == "")
})


test_that("colors change", {
  simple_colors <- unique(complex_p_build$data[[1]]$fill)
  complex_colors <- unique(simple_p_build$data[[1]]$fill)

  expect_true(all(simple_colors != complex_colors))
})


test_that("y-axis truncated", {
  complex_y <- as.character(unique(complex_p_data$value))
  simple_y <- as.character(unique(simple_p_data$value))

  expect_true(max(nchar(complex_y)) < max(nchar(simple_y)))
})


test_that("sorted", {
  by_expected <-
    expected_proportions(
      df = mtcars,
      dep_var = "mpg > 15",
      sort_by = "expected",
      n_field = 1,
      n_cat = 5
    ) %>%
    ggplot2::ggplot_build()

  by_actual <-
    expected_proportions(
      df = mtcars,
      dep_var = "mpg > 15",
      sort_by = "actual",
      n_field = 1,
      n_cat = 5
    ) %>%
    ggplot2::ggplot_build()

  expect_true(
    any(by_expected$data[[1]]$y != by_actual$data[[1]]$y)
  )
})


# rm(ls()[ls(pattern = "simple|complex")])
