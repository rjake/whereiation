#' Profile examles at top / bottom of estimate
#'
#' @param ... variables passed on to \code{generate_estimate_details}
#' @param n number of observations to retrieve
#' @param position whether to return the highest ("top") or lowest ("bottom")
#'
#' @inheritDotParams variation_plot
#' @importFrom dplyr distinct arrange left_join select mutate rename_all desc
#' @importFrom scales percent
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_replace
#' @importFrom kableExtra cell_spec spec_color kable kable_styling
#' @importFrom tidyr spread
#'
#' @export
#' @family visualization functions

#' @examples
#' \dontrun {
#' profile_n(df = iris, dep_var = "Sepal.Length")
#' }
profile_n <- function(df,
                      dep_var,
                      ...,
                      n = 5,
                      position = c("top", "bottom")) {
  # df <- survival::flchain; dep_var <- "death"; ignore_cols <- "chapter";

  df_prep <- generate_estimate_details(df = df, dep_var = dep_var, ...)

  position <- match.arg(position)


  if (position == "top") {
    choose_end <- tail
  } else {
    choose_end <- head
  }


  slice_ids <-
    df_prep %>%
    distinct(.data$y_id, .data$estimate) %>%
    arrange(.data$estimate) %>%
    choose_end(n = n) %>%
    left_join(df_prep, by = c("y_id", "estimate")) %>%
    select(
      id = .data$y_id,
      .data$field, .data$field_wt,
      .data$value, .data$factor_avg, .data$estimate
    ) %>%
    mutate(
      pct_group = percent(.data$factor_avg, accuracy = 1),
      est_obs = round(.data$estimate, 4),
      cell = glue("{.data$value}\n{.data$pct_group}"),
      id = glue("ID {.data$id}<br/>est: {.data$est_obs}")
    ) %>%
    mutate(id = fct_reorder(.data$id, .data$estimate, .desc = TRUE))


  df_style <-
    slice_ids %>%
    mutate(
      cell = cell_spec(
        .data$cell,
        bold = TRUE,
        color = spec_color(
          .data$factor_avg, option = "C", end = 0.7, direction = -1
        )
      )
    ) %>%
    select(.data$id, .data$field, .data$field_wt, .data$cell) %>%
    spread(.data$id, .data$cell) %>%
    arrange(desc(.data$field_wt)) %>%
    mutate(field_wt = percent(.data$field_wt, 3))


  df_style %>%
    kable(
      escape = FALSE, align = "c",
      format = "html",
      caption = glue("{dep_var}: {position} {n} observations")
    ) %>%
    kable_styling(c("striped", "condensed"), full_width = TRUE)
}
