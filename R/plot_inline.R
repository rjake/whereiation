plot_inline <- function() {
    ggplot(group_value_ranks,
           aes(
             x = group_mean,
             y = field,
             color = field,
             fill = field
           )) +
    plot_theme + plot_grand_mean + #plot_one_obs_mean +
    geom_line(size = 6, alpha = 0.2) +
    geom_point(aes(size = n), shape = 21, color = "black", stroke = 0.5) +
    #theme(axis.title.x = element_blank()) +
    guides(color = FALSE, fill = FALSE) +
    xlim(0, NA) +
    #geom_point(data = one_obs_profile, aes(size = n), color = "black") +
    #plot_one_obs_mean_wt +
    labs(
      title = paste0(set_dv, " across all factors of all fields"),
      subtitle = "each point represents a factor in the field along the y axis, factors with 5 or fewer observations have been excluded \nthe dotted line is the grand mean across all observations",
      y = "Field",
      x = "group mean of the dependent variable for each factor",
      size = "# of \n observations"
    )
}

plot_inline_single_obs <- function(labels = FALSE) {
  
  get_id <- 
    base_data %>% 
    arrange(outcome) %>% 
    slice(1) %>% 
    pull(unique_id)
  
  one_obs_profile <- 
    compare_values %>% 
    filter(unique_id == get_id) %>% 
    select(outcome:group_mean, n, field_range, field_wt, value_diff, group_var) %>% 
    mutate(obs_mean = mean(group_mean),
           group_dist = group_mean - obs_mean,
           group_dist_wt = group_dist * field_wt,
           obs_wt = obs_mean + group_dist_wt,
           obs_estimate = mean(obs_wt)) %>% 
    select(unique_id, field, value, n, group_mean, obs_mean,
           group_dist, field_wt, group_dist_wt, obs_wt, obs_estimate, 
           group_var) %>% 
    left_join(field_ranks)
  
  one_mean <- mean(one_obs_profile$obs_mean)
  one_mean_wt <- mean(one_obs_profile$obs_estimate)
  
  plot_one_obs_mean <- geom_vline(xintercept = one_mean, size = 1, alpha = .5)
  plot_one_obs_mean_wt <- geom_vline(xintercept = one_mean_wt, color = "black", size = 2)
  
  plot_orig <-
    plot_inline() + 
    plot_one_obs_mean +
    geom_segment(data = one_obs_profile, xend = one_mean,
                 aes(yend = field),
                 color = "black", 
                 size = 2, alpha = 0.5) +
    geom_point(data = one_obs_profile, 
               color = "black", size = 5, shape = 21, stroke = 2) +
    labs(
      title = paste0(set_dv, " across all factors of all fields", " - single observation")
    )
  
  if (labels == TRUE) {
    plot_orig <-
      plot_orig + 
      geom_label_repel(data = one_obs_profile, size = 4, 
                       aes(label = paste0(field, ": ",value)),
                       color = "black",
                       segment.color = NA,
                       hjust = -0.15)
  } 
  
  plot_orig
  
}

