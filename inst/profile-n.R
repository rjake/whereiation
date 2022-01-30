#' Profile observations with the most extreme values
#'
#' @param ... variables passed on to \code{isolate_record}
#' @param n number of observations to retrieve
#' @param position whether to return the highest ("top") or lowest ("bottom")
#'
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#' @importFrom utils head tail
#' @importFrom dplyr distinct arrange left_join select mutate rename_all desc
#' @importFrom scales percent
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
#' @importFrom stringr str_replace
#' @importFrom kableExtra cell_spec spec_color kable kable_styling
#' @importFrom tidyr spread
#'
#' @export
#' @examples
#' \dontrun{
#' profile_n(df = iris, dv = Sepal.Length)
#' profile_n(df = iris, dv = Species == 'virginica')
#' }
#'
profile_n <- function(df,
                      dv,
                      ...,
                      n = 5,
                      position = c("top", "bottom")) {
  # df <- survival::flchain; dv <- "death"; ignore_cols <- "chapter";

  df_prep <- isolate_record(df = df, dv = {{dv}}, ...)

  position <- match.arg(position)
  dv_name <- deparse(substitute(dv))


  if (position == "top") {
    choose_end <- tail
  } else {
    choose_end <- head
  }


  slice_ids <-
    df_prep %>%
    distinct(.data$unique_id, .data$estimate) %>%
    arrange(.data$estimate) %>%
    choose_end(n = n) %>%
    left_join(df_prep, by = c("unique_id", "estimate")) %>%
    select(
      id = .data$unique_id,
      .data$field, .data$field_wt,
      .data$value, .data$factor_avg, .data$estimate
    ) %>%
    mutate(
      pct_group = round(.data$factor_avg, 3),
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
    spread(.data$id, .data$cell, fill = "small\nsample size") %>%
    arrange(desc(.data$field_wt)) %>%
    mutate(field_wt = percent(.data$field_wt, 3))


  df_style %>%
    kable(
      escape = FALSE,
      align = "c",
      format = "html",
      caption = glue("{dv_name}: {position} {n} observations")
    ) %>%
    kable_styling(
      c("striped", "condensed"),
      full_width = TRUE
    )
}


#' Generate stats for each observation at the factor level
#'
#' The dataset returned will be the length of the # of columns x # of rows
#' @param train_data training dataset generated from summarize_factors
#' @inheritDotParams refactor_columns
#' @inheritParams refactor_columns
#'
#' @importFrom tidyr gather drop_na
#' @importFrom dplyr mutate left_join filter arrange desc group_by ungroup
#' @importFrom grDevices boxplot.stats
#' @importFrom scales rescale_mid
#' @importFrom stats complete.cases weighted.mean
#' @importFrom rlang .data
#' @noRd
#' @examples
#' isolate_record(df = iris, dv = Sepal.Length)
isolate_record <- function(df, train_data, dv, ...) {

  if (missing(train_data)) {
    base_data <- refactor_columns(df, dv = {{dv}}, ...)
    group_stats <- summarize_factors_all_fields(df, dv = {{dv}}, ..., return = "list")
  } else {
    base_data <- df
    group_stats <- summarize_factors_all_fields(train_data, dv = {{dv}}, ..., return = "list")
  }

  group_stats_data <- group_stats$data
  orig_min <- group_stats$orig_min
  orig_max <- group_stats$orig_max


  base_data %>%
    gather(key = "field", value = "value", -c(1, 2, 3)) %>%
    mutate(value = as.character(.data$value)) %>%
    left_join(group_stats_data, by = c("field", "value")) %>%
    group_by(.data$unique_id) %>%
    mutate(complete = sum(!is.na(.data$factor_avg))) %>%
    ungroup() %>%
    drop_na(.data$factor_avg:.data$field_wt) %>%
    #arrange(desc(.data$field_wt)) %>%
    mutate(factor_avg_wt = .data$factor_avg * .data$field_wt) %>%
    group_by(.data$unique_id) %>%
    mutate(estimate = weighted.mean(.data$factor_avg, .data$field_wt)) %>%
    ungroup() %>%
    mutate(
      rescale_estimate = rescale_mid(
        x = .data$estimate,
        to = c(orig_min, orig_max),
        mid = group_stats$grand_avg
      )
    )
}
