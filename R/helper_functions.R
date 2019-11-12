change_range <- function(x, new_min, new_max) {
  (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
}


check_cut <- function(x) {
  (is.numeric(x) | is.integer(x)) & n_distinct(x) > 15
}


check_n_cat <- function(x) {
  n <- n_distinct(x)

  n != 1 &
    (is.character(x) | is.factor(x)) &
    n < 100 &
    n < length(x)
}

# confirm if numeric/date fields should be cut
check_num_cat <- function(x, n_quantile) {
  (is.numeric(x) | is.integer(x) | lubridate::is.Date(x)) &
    n_distinct(x) > n_quantile
}

# cut numeric and dates into 10 groups
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
    label <- ifelse(str_detect(label, "\\["), paste0(ord, label), as.character(label))
  }

  label
}

# lump categorical data into 10 groups
collapse_cat <- function(x, n) {
  get_n <- n_distinct(x) - n

  if (get_n > 0 & class(x) == "character") {
    fct_lump(x, n, ties.method = "first") %>%
      str_replace("^Other$", paste0("Other (", get_n, ")"))
  } else {
    x
  }
}


