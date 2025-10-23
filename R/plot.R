# ---------------------------------------------------------------------
# plot_compare_influence: Create a horizontal bar plot showing the
# difference in influence of LHS items between two datasets (e.g., ADAS vs ADS).
#
# Args:
#   df : A dataframe containing LHS items and their influence values:
#        - influence.x : influence in dataset 1
#        - influence.y : influence in dataset 2
#        - LHS         : left-hand side items (rules)
#
# Returns:
#   A ggplot object of a horizontal bar chart showing influence differences.
# ---------------------------------------------------------------------
plot_compare_influence <- function(df) {
  
  # Compute influence difference and wrap long LHS labels for readability
  df <- df %>%
    mutate(
      influence_diff = influence.x - influence.y,  # difference between datasets
      LHS_wrapped = str_wrap(LHS, width = 30)     # wrap long item labels
    ) %>%
    arrange(influence_diff)  # sort items by influence difference
  
  # Set y-axis limits with padding for better visualization
  y_min <- min(df$influence_diff) * 1.2
  y_max <- max(df$influence_diff) * 1.2
  
  # Create the bar plot
  p <- ggplot(
    df,
    aes(
      x = reorder(LHS_wrapped, influence_diff),  # reorder bars by influence difference
      y = influence_diff,                        # height of bars
      fill = influence_diff                       # color reflects magnitude
    )
  ) +
    geom_col() +  # draw bars
    scale_fill_gradient2(
      low = "#1f78b4",   # blue for negative values (leans ADAS)
      mid = "white",     # neutral
      high = "#ff7f00",  # orange for positive values (leans ADS)
      midpoint = 0,
      name = "Influence",
      breaks = c(min(df$influence_diff), 0, max(df$influence_diff)),
      labels = c(
        paste0("Leaning ADAS (", round(min(df$influence_diff), 2), ")"),
        "Neutral (0)",
        paste0("Leaning ADS (", round(max(df$influence_diff), 2), ")")
      )
    ) +
    scale_y_continuous(
      limits = c(y_min, y_max),                  # apply padded limits
      expand = expansion(mult = c(0.05, 0.15))  # add extra spacing
    ) +
    coord_flip() +  # horizontal bars
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),             # hide y-axis labels
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = "Intersection Influential Items",
      x = "Item (LHS)",
      y = "Influence Difference"
    )
  
  return(p)
}


# ---------------------------------------------------------------------
# plot_rule_thresholds: Plot the number of association rules generated
# across varying support and confidence thresholds.
#
# Args:
#   df : Dataframe with columns:
#        - support      : support threshold
#        - rule_count   : number of rules generated
#        - confidence   : confidence threshold
#
# Returns:
#   A ggplot object of a line plot showing rules vs support, colored by confidence.
# ---------------------------------------------------------------------
plot_rule_thresholds <- function(df) {
  p <- ggplot(df, aes(x = support, y = rule_count, color = factor(confidence))) +
    geom_line(size = 1.2) +      # line showing trend of rule counts
    geom_point(size = 2) +       # highlight individual points
    scale_color_brewer(palette = "Set1", name = "Confidence") +  # color by confidence
    labs(
      title = "Number of Association Rules vs Support",
      x = "Support",
      y = "Number of Rules"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")  # position legend on the right
  
  p
}
