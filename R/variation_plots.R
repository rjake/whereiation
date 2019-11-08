variation_plot <- function(df, dep_var, avg_type, ignore_cols = NA) {
  factor_stats <- summarize_factors(df, dep_var, avg_type, ignore_cols)
  grand_avg <- factor_stats$grand_avg[1]
  group_value_ranks <-
    factor_stats %>%
    # left_join(field_ranks) %>%
    mutate(
      field = fct_reorder(field, field_range, max),
      label = paste(field, ": \n", value)
    )
  
  
  plot_grand_avg <-
    geom_vline(xintercept = grand_avg, color = "grey60", size = 1, linetype = "dotted")
  
  plot_theme <-
    theme(panel.background = element_rect(fill = "white", color = "grey60"))
  
  ggplot(
    group_value_ranks,
    aes(
      x = group_avg,
      y = field,
      color = field,
      fill = field
    )
  ) +
    plot_theme + plot_grand_avg + # plot_one_obs_avg +
    geom_line(size = 6, alpha = 0.2) +
    geom_point(aes(size = n), shape = 21, color = "black", stroke = 0.5) +
    # theme(axis.title.x = element_blank()) +
    guides(color = FALSE, fill = FALSE) +
    # xlim(0, NA) +
    # geom_point(data = one_obs_profile, aes(size = n), color = "black") +
    # plot_one_obs_avg_wt +
    labs(
      title = paste0(dep_var, " across all factors of all fields"),
      subtitle = paste0("each point represents a factor in the field along the y axis, factors with 5 or fewer observations have been excluded \nthe dotted line is the grand ", avg_type, " across all observations"),
      y = "Field",
      x = paste0("group ", avg_type, " of the dependent variable for each factor"),
      size = "# of \n observations"
    )
}

variation_plot_single_obs <- function(df, dep_var, avg_type, ignore_cols = NA, labels = FALSE, id = 1) {
  base_data <- refactor_columns(df, dep_var, avg_type, ignore_cols)
  compare_values <- calculate_factor_stats(df, dep_var, avg_type, ignore_cols)
  avg <- eval(parse(text = avg_type))
  
  get_id <- id
  
  field_ranks <-
    compare_values %>%
    distinct(field, field_range, field_wt) %>%
    arrange(desc(field_range)) %>%
    mutate(rank_range = factor(row_number()))
  
  one_obs_profile <-
    compare_values %>%
    filter(unique_id == get_id) %>%
    select(outcome:group_avg, grand_avg, n, field_range, field_wt, group_dist) %>%
    mutate(
      obs_avg = avg(group_avg),
      group_dist = group_avg - obs_avg,
      # group_dist_wt = group_dist * field_wt,
      group_dist_wt = group_dist * field_range,
      obs_wt = obs_avg + group_dist_wt,
      obs_estimate = avg(obs_wt)
    ) %>%
    select(
      unique_id, field, value, n, grand_avg, group_avg, obs_avg,
      group_dist, field_wt, group_dist_wt, obs_wt, obs_estimate
    ) %>%
    left_join(field_ranks)

  one_avg <- avg(one_obs_profile$obs_avg)
  one_avg_wt <- avg(one_obs_profile$obs_estimate)

  plot_one_obs_avg <- geom_vline(xintercept = one_avg, size = 1, alpha = .5)
  # plot_one_obs_avg_wt <- geom_vline(xintercept = one_avg_wt, color = "black", size = 2)

  plot_orig <-
    variation_plot(df, dep_var, avg_type, ignore_cols) +
    plot_one_obs_avg +
    geom_segment(
      data = one_obs_profile, xend = one_avg,
      aes(yend = field),
      color = "black",
      size = 2, alpha = 0.5
    ) +
    geom_point(
      data = one_obs_profile,
      color = "black", size = 5, shape = 21, stroke = 2
    ) +
    labs(
      title = paste0(dep_var, " across all factors of all fields", " - single observation")
    )

  if (labels == TRUE) {
    plot_orig <-
      plot_orig +
      geom_label_repel(
        data = one_obs_profile, size = 4,
        aes(label = paste0(field, ": ", value)),
        color = "black",
        segment.color = NA,
        hjust = -0.15
      )
  }

  plot_orig
}
