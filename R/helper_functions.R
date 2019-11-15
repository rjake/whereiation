#' Expand the range to a new min and max
#' @param x numeric vector
#' @param new_min new minimum value
#' @param new_max new maxmim value
#' @noRd
change_range <- function(x, new_min, new_max) {
  (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
}


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
#' @importFrom stringr str_pad
#' @noRd
cut_custom <- function(x, n_quantile, order = FALSE) {
  label <- # create cut labels ex: "[0-4)" "[5-9)"
    cut(
      x,
      breaks = n_quantile,
      include.lowest = TRUE,
      dig.lab = 5,
      right = FALSE,
      ordered_result = TRUE
    )

  if (order) {
    ord <- # will create order ex: "(02)"
      paste0("(", str_pad(as.integer(label), 2, pad = "0"), ") ")

    # if cut returns brackets, add order ex: "(02) [5-9)"
    label <-
      ifelse(
        grepl("\\[", label),
        paste0(ord, label),
        as.character(label)
      )
  }

  label
}


#' Lump categorical data into groups
#' @param x vector of categorical data
#' @param n number of categories to keep
#'
#' @importFrom forcats fct_lump
#' @importFrom stringr str_replace
#' @noRd
collapse_cat <- function(x, n) {
  get_n <- length(unique(x)) - n

  if (get_n > 0 & class(x) == "character") {
    fct_lump(x, n, ties.method = "first") %>%
      str_replace("^Other$", paste0("Other (", get_n, ")"))
  } else {
    x
  }
}


