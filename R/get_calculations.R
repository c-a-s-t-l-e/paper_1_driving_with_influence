# ---------------------------------------------------------------------
# Calculation functions for single item influence.
# These functions compute how strongly items in the LHS of rules
# contribute globally or locally to association rule groups.
# ---------------------------------------------------------------------

# Compute global influence of LHS items across all rules
get_global_influence <- function(df, rule = "rule") {
  total_rules <- nrow(df)  # total number of rules for normalization
  
  df <- split_rules(df, rule)  # split rules into LHS and RHS columns
  
  influence_df <- df %>%
    # explode LHS into individual items
    mutate(LHS = strsplit(LHS, "\\s*,\\s*")) %>%
    unnest(LHS) %>%
    mutate(LHS = trimws(LHS)) %>%  # remove extra whitespace
    distinct(row_number(), LHS) %>% # ensure each LHS item counts only once per rule
    count(LHS, name = "count") %>%  # count occurrences of each item
    mutate(influence = count / total_rules) %>%  # normalize by total rules
    arrange(desc(influence))  # sort descending by influence
  
  influence_df
}

# Compute local influence of LHS items relative to each RHS
get_local_influence <- function(df, rule = "rule") {
  df <- split_rules(df, rule)  # split rules into LHS and RHS
  
  df %>%
    mutate(LHS = strsplit(LHS, "\\s*,\\s*")) %>%
    unnest(LHS) %>%
    mutate(LHS = trimws(LHS)) %>%  # clean whitespace
    distinct(RHS, row_number(), LHS) %>%  # count each LHS once per RHS
    group_by(RHS, LHS) %>%
    count(name = "count") %>%  # count occurrences per RHS
    group_by(RHS) %>%
    mutate(
      total_rules = sum(count),          # total rules for this RHS
      influence = count / total_rules     # calculate local influence
    ) %>%
    ungroup() %>%
    arrange(RHS, desc(influence))  # sort by RHS and influence descending
}