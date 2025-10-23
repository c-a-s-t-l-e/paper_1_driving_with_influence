plot_global_influence <- function(df, top_n = 10) {
  top_df <- df %>%
    slice_max(order_by = influence, n = top_n)

  p <- ggplot(
    top_df,
    aes(x = reorder(LHS, -influence), y = influence, fill = influence)
  ) +
    geom_col(width = 0.75) +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    labs(
      x = "LHS Item",
      y = "Influence Score",
      title = paste("Top LHS Items by Influence")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(
        angle = 45, # or try 60 if labels are very long
        hjust = 1, # right-justify so labels align under ticks
        vjust = 1
      ),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
  p
}

plot_local_influence <- function(df, top_n = 10) {
  top_df <- df %>%
    group_by(RHS) %>%
    slice_max(order_by = influence, n = top_n) %>%
    # create composite key so reorder works within facet
    mutate(
      LHS_facet = paste0(RHS, "_", LHS),
      LHS_facet = fct_reorder(LHS_facet, influence)
    ) %>%
    ungroup()

  p <- ggplot(top_df, aes(x = LHS_facet, y = influence, fill = influence)) +
    geom_col(width = 0.75) +
    coord_flip() +
    facet_wrap(~RHS, scales = "free_y") +
    scale_x_discrete(name = NULL, labels = function(x) {
      sub("^.*_(.*)$", "\\1", x)
    }) +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    labs(
      y = "Influence Score",
      title = "Influence of LHS Items by RHS Value"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      legend.position = "none"
    )
  p
}

# plot_compare_influence <- function(df) {
#   df <- df |>
#     mutate(
#       influence_diff = influence.x - influence.y
#     ) |>
#     arrange(-influence_diff)
# 
#   p <- ggplot(
#     df,
#     aes(
#       x = reorder(LHS, -influence_diff),
#       y = influence_diff,
#       fill = influence_diff
#     )
#   ) +
#     geom_col() +
#     scale_fill_gradient2(
#       low = "blue",
#       mid = "white",
#       high = "red",
#       midpoint = 0,
#       name = "",
#       breaks = c(
#         min(df$influence_diff),
#         0,
#         max(df$influence_diff)
#       ),
#       labels = c(
#         paste0(
#           "ADAS Leaning (",
#           round(min(df$influence_diff), 2),
#           ")"
#         ),
#         "Neutral (0)",
#         paste0("ADS Leaning (", round(max(df$influence_diff), 2), ")")
#       )
#     ) +
#     theme(
#       axis.text.x = element_text(
#         angle = 75,
#         hjust = 1,
#         vjust = 1,
#         face = "bold"
#       ),
#       axis.title = element_text(size = 14, face = "bold"),
#       legend.title = element_text(size = 12, face = "bold"),
#       legend.text = element_text(size = 10)
#     ) +
#     labs(
#       x = "Item (LHS)",
#       y = "Influence Difference"
#     )
#   p
# }

plot_compare_influence <- function(df) {
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(scales)
  
  df <- df %>%
    mutate(
      influence_diff = influence.x - influence.y,
      LHS_wrapped = str_wrap(LHS, width = 30)
    ) %>%
    arrange(influence_diff)
  
  # calculate limits with padding
  y_min <- min(df$influence_diff) * 1.2
  y_max <- max(df$influence_diff) * 1.2
  
  p <- ggplot(
    df,
    aes(
      x = reorder(LHS_wrapped, influence_diff),
      y = influence_diff,
      fill = influence_diff
    )
  ) +
    geom_col() +
    scale_fill_gradient2(
      low = "#1f78b4",   # blue
      mid = "white",     # neutral
      high = "#ff7f00",  # orange
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
      limits = c(y_min, y_max),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    coord_flip() +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),   # Always no y-axis labels
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






plot_rule_thresholds <- function(df) {
  p <- ggplot(df, aes(x = support, y = rule_count, color = factor(confidence))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_brewer(palette = "Set1", name = "Confidence") +
    labs(
      title = "Number of Association Rules vs Support",
      x = "Support",
      y = "Number of Rules"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")
  
  p
}