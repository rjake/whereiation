simple_p <-
  plot_expected_proportions(
    df = mtcars,
    dv = mpg > 15
  )

complex_p <-
  plot_expected_proportions(
    df = mtcars,
    dv = mpg > 15,
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


test_that("summarize_over_under_proportions categorizes correctly", {
  df <-
    summarize_over_under_proportions(
      df = refactor_columns(iris, dv = Sepal.Length > 5),
      "Species"
    )
  expect_equal(
    object = df$category,
    expected = c("under", "over", "over")
  )
})


test_that("map_over_under_proportions creates new fields", {
  df <- map_over_under_proportions(df = iris, Sepal.Length > 5)
  expect_true(all(c("expected", "actual", "category") %in% names(df)))
})


test_that("map_over_under_proportions throws warning for non-binary DV", {
  # Sepal.Length is not binary
  expect_warning(map_over_under_proportions(df = iris, dv = Sepal.Length))
})

test_that("summarize_over_under_proportions omits NA values", {
  iris_na <- iris
  iris_na$missing <- c(1, 2, NA)
  df <-
    summarize_over_under_proportions(
      df = refactor_columns(iris_na, dv = Sepal.Length > 5),
      "missing"
    )
  expect_true(sum(is.na(df)) == 0)
})

test_that("returns data", {
  df <-
    plot_expected_proportions(
      df = mtcars,
      dv = mpg > 15,
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
    purrr::map(purrr::pluck, "geom") %>%
    purrr::map(class) %>%
    purrr::map_chr(purrr::pluck, 1)

  expect_equal(
    object = geom_layers,
    expected = c("GeomCol", "GeomVline","GeomSegment", "GeomPoint", "GeomPoint")
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
    plot_expected_proportions(
      df = employee_attrition |> select(gender, job_level),
      dv = gender == "Female",
      sort_by = "expected",
      threshold = 0
    )

  by_actual <-
    plot_expected_proportions(
      df = employee_attrition |> select(gender, job_level),
      dv = gender == "Female",
      sort_by = "actual",
      threshold = 0
    )

  expect_true(
    !identical(
      levels(by_expected$data$value),
      levels(by_actual$data$value)
    )
  )

})


# rm(ls()[ls(pattern = "simple|complex")])
