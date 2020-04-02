# test_that("calculate_factor_stats works", {
#   df <- calculate_factor_stats(df = iris, dep_var = "Petal.Length")
#
#   actual_dim <- dim(df)
#   expected_dim <- c(586, 14)
#   expect_equal(actual_dim, expected_dim)
#
#   actual_names <- names(df)
#   expected_names <-
#     c("datascanr_outcome", "datascanr_id", "field", "value",
#       "group_avg", "n",
#       "field_r_sq", "field_r_sq_adj", "field_p_value",
#       "grand_avg",
#       "field_wt", "complete",
#       "group_avg_wt",
#       "estimate")
#
#   expect_equal(actual_names, expected_names)
# })
#
