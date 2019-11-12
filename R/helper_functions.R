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

cut_custom <- function(x) {
  n <- 10
  cut(
    x,
    breaks = unique(quantile(x, probs = 0:n / n, na.rm = TRUE)),
    include.lowest = TRUE,
    dig.lab = 10
  )
}
