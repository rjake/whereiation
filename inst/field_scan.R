#' #' Title
#' #'
#' #' @param df
#' #' @param n_cat
#' #' @param n_quantile
#' #' @param ignore_cols
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' field_scan <- function(df, n_cat = 15, n_quantile = 10, ignore_cols = NA_character_) {
#'   # confirm if numeric/date fields should be cut
#'   check_cut_numeric <- function(x) {
#'     (is.numeric(x) | is.integer(x) | lubridate::is.Date(x)) &
#'       n_distinct(x) > n_quantile
#'   }
#'
#'   # cut numeric and dates into 10 groups
#'   cut_custom <- function(x) {
#'     label <- # create cut labels ex: "[0-4)" "[5-9)"
#'       cut(
#'         x,
#'         breaks = n_quantile,
#'         include.lowest = TRUE,
#'         dig.lab = 5,
#'         right = FALSE,
#'         ordered_result = TRUE
#'       )
#'
#'     ord <- # will create order ex: "(02)"
#'       paste0("(", str_pad(as.integer(label), 2, pad = "0"), ") ")
#'
#'     # if cut returns brackets, add order ex: "(02) [5-9)"
#'     ifelse(str_detect(label, "\\["), paste0(ord, label), as.character(label))
#'   }
#'
#'   # lump categorical data into 10 groups
#'   collapse_cat <- function(x, n = n_cat) {
#'     get_n <- n_distinct(x) - n
#'
#'     if (get_n > 0 & class(x) == "character") {
#'       fct_lump(x, n, ties.method = "first") %>%
#'         str_replace("^Other$", paste0("Other (", get_n, ")"))
#'     } else {
#'       x
#'     }
#'   }
#'
#'   # transform all columns: remove set_ignore columns, remove rows with NAs cut numeric data into categories, lump categorical data, add id
#'   df_as_categories <-
#'     df %>%
#'     select(-one_of(ignore_cols)) %>%
#'     # filter(complete.cases(.)) %>%
#'     mutate_if(check_cut_numeric, cut_custom) %>%
#'     mutate_if(is.character, collapse_cat) %>%
#'     mutate(id = row_number())
#'
#'   # table of class attributes
#'   column_class <-
#'     tibble(name = colnames(df)) %>%
#'     mutate(
#'       ord = row_number(),
#'       class = as.character(sapply(df, class)),
#'       n = as.integer(sapply(df, n_distinct)),
#'       class = ifelse(n <= 2, "logical", class)
#'     ) %>%
#'     filter(n != nrow(df))
#'
#'   # vector of column names
#'   get_cat_vars <- names(df_as_categories)
#'
#'   # function to create counts, first column selected by integer #, this is similar to a gather of all field & field values followed by count but resulting in fewer rows at each step
#'   agg_cat_fields <-
#'     function(i) {
#'       df_as_categories %>%
#'         select(value = i) %>%
#'         mutate(
#'           field = names(df_as_categories)[i],
#'           value = as.character(value)
#'         ) %>%
#'         group_by(field, value) %>%
#'         summarise(n = n()) %>%
#'         ungroup() %>%
#'         group_by(field, value) %>%
#'         summarise(n = sum(n)) %>%
#'         ungroup()
#'     }
#'
#'   # map_dfr table
#'   get_cat_fields <- map_dfr(seq_along(get_cat_vars), agg_cat_fields)
#'
#'
#'   # get names of all categorical variables
#'   all_cat_vars <-
#'     column_class %>%
#'     filter(str_detect(class, "logical|factor|character")) %>%
#'     pull(name)
#'
#'   # get names of all numeric & date variables
#'   all_num_vars <-
#'     column_class %>%
#'     filter(!name %in% all_cat_vars) %>%
#'     pull(name)
#'
#'   # plot of categorical variables
#'   get_cat_fields %>%
#'     filter(field %in% c(all_cat_vars, all_num_vars)) %>%
#'     mutate(
#'       discrete = ifelse(field %in% all_cat_vars, "discrete", "cont.") # ,
#'       # value = ifelse(rep(sort == FALSE, n()), value, fct_reorder(value, n))
#'     ) %>%
#'     ggplot(aes(x = (str_sub(value, 1, 15)), y = n)) + # fct_reorder, n
#'     geom_col(aes(fill = discrete), show.legend = FALSE) +
#'     facet_wrap(~ discrete + field, scales = "free") +
#'     coord_flip() +
#'     labs(
#'       title = "Frequency chart of top values and quantiles",
#'       x = ""
#'     )
#' }
