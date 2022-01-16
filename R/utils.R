#' Confirm if numeric should be cut
#' @param x vector of data
#' @param n_quantile (max) number of quantiles to break data
#' @noRd
check_cut_numeric <- function(x, n_quantile) {
  (is.numeric(x) | is.integer(x)) &
    length(unique(x)) > n_quantile
}


#' Cut numeric and dates into 10 groups
#' @param x vector of data
#' @param n_quantile (max) number of quantiles to break data
#' @param order when TRUE, add rank to result ex: "(01) [1,3)", "(02) [4,10]"
#' @importFrom stringr str_pad str_replace_all
#' @noRd
cut_custom <- function(x, n_quantile, n_digits) {
  label <- # create cut labels ex: "[0-4)" "[5-9)"
    cut(
      x,
      breaks = n_quantile,
      include.lowest = TRUE,
      dig.lab = 5,
      right = FALSE,
      ordered_result = TRUE
    )

  # will create order ex: "(02)"
  ord <- str_pad(as.integer(label), 2, pad = "0")

  paste(ord, label) %>%
    str_replace_all("(\\,)(?=\\d)", " to ") %>%
    str_replace_all(paste0("(\\.\\d{", n_digits, "})\\d*"), "\\1")
}


#' Lump categorical data into groups
#' @param x vector of categorical data
#' @param n number of categories to keep
#'
#' @importFrom forcats fct_lump
#' @importFrom stringr str_replace
#' @importFrom stats ave
#' @noRd
#' @examples
#' collapse_cat(x = letters[c(1, 2, 2, 2, 3, 4, 5, 5)], n = 2)
#' collapse_cat(x = letters[c(1, 2, 2, 2, 3, 4, 5, 5)], n = NULL)
collapse_cat <- function(x,
                         n = NULL,
                         w = NULL) {
  if (is.null(n)) {
    n <- length(x)
  }

  if (is.null(w)) { # aggregate counts
    w <- as.integer(ave(x, x, FUN = length))
  }

  get_n <- length(unique(x)) - n

  if (get_n > 0 & class(x) == "character") {
    f_x <- fct_reorder(.f = x, .x = w, .fun = mean, .desc = TRUE)
    x[(as.integer(f_x) > n)] <- paste0("Other (", get_n, ")")
    x
  } else {
    x
  }
}


#' Test that x is a 0/1 binary variable
#' @param x vector
#' @importFrom glue glue
#'
#' @noRd
check_01_binary <- function(x) {
  if (!typeof(x) %in% c("integer", "double", "logical")) {
    stop(
      "the 'dv' specified is not binary (0/1) or logical",
      call. = FALSE
    )
  }

  eval_x <- as.numeric(unique(x[!is.na(x)]))
  is_binary <- all(range(eval_x) == c(0, 1))

  if (!is_binary) {
    rng <-
      range(eval_x, na.rm = TRUE) %>%
      paste(collapse = " to ")

    warning(
      glue(
        "Expected 'dv' to be a binary field 0/1 or TRUE/FALSE \\
        and data has values {rng}.
        The result may not be meaningful."
      ),
      call. = FALSE
    )
  }
}

#' Test that x is binary variable
#' @param x vector
#' @importFrom stringr str_trunc
#' @importFrom glue glue
#' @noRd
check_binary <- function(x) {
  unique_vals <- sort(unique(x[!is.na(x)]))
  is_binary <- length(unique_vals) == 2

  ex_inputs <-
    paste0(unique_vals, collapse = ", ") %>%
    str_trunc(80)

  if (!is_binary) {
    stop(
      glue(
        'Expecting a binary result.
        Found: {ex_inputs}
        Use a field with only two values or \\
        a logical test.
        Ex. \'date_field < "2020-01-01"\''
      ),
      call. = FALSE
    )
  }
}


#' Extract field name from expression, ex: "Year == 2008" -> "Year"
#' @noRd
extract_field_name <- function(x) {
  x %>%
    gsub(pattern = " .*", replacement = "") %>%
    gsub(pattern = "\\(", replacement = "")
}


#' @importFrom stringr str_remove_all
#' @noRd
#' @examples
#' clean_labels(x = "<class>suv")
#' clean_labels(x = "<cty>01 [8.97 to 11.6)")
clean_labels <- function(x) {
  str_remove_all(x, "^[^\\>]*(\\>)")
}


#' @importFrom stringr str_trunc str_detect
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
#' @importFrom dplyr mutate group_by arrange row_number ungroup
#' @noRd
#' @examples
#' reorder_within_field(
#'   df = summarize_factors_all_fields(iris, dv = Sepal.Length) |> filter(field == "Petal.Width"),
#'   sort_cols = field_avg,
#'   trunc_length = 30
#' )
reorder_within_field <- function(df, sort_cols, trunc_length) {
  df %>%
    mutate(
      value = glue(
        "<{field}>{x}",
        x = str_trunc(.data$value, trunc_length)
      )
    ) %>%
    group_by(.data$field) %>%
    arrange(
      ifelse(
        str_detect(.data$value, "\\d{2} \\["),
        .data$value,
        c(!!enquo(sort_cols))
      )
    ) |>
    mutate(
      value = fct_reorder(.data$value, row_number())
    ) %>%
    ungroup()
}
