# Calculation functions

get_global_influence <- function(df, rule = "rule") {
  total_rules <- nrow(df)

  df <- split_rules(df, rule)

  influence_df <- df %>%
    # explode LHS into individual items
    mutate(LHS = strsplit(LHS, "\\s*,\\s*")) %>%
    unnest(LHS) %>%
    mutate(LHS = trimws(LHS)) %>%
    distinct(row_number(), LHS) %>% # ensure each item counts only once per rule
    count(LHS, name = "count") %>%
    mutate(influence = count / total_rules) %>%
    arrange(desc(influence))

  influence_df
}

get_local_influence <- function(df, rule = "rule") {
  # total rules per RHS
  df <- split_rules(df, rule)

  df %>%
    # explode LHS into individual items
    mutate(LHS = strsplit(LHS, "\\s*,\\s*")) %>%
    unnest(LHS) %>%
    mutate(LHS = trimws(LHS)) %>%
    # count each LHS item only once per rule
    distinct(RHS, row_number(), LHS) %>%
    group_by(RHS, LHS) %>%
    count(name = "count") %>%
    # calculate influence relative to total rules with that RHS
    group_by(RHS) %>%
    mutate(total_rules = sum(count), influence = count / total_rules) %>%
    ungroup() %>%
    arrange(RHS, desc(influence))
}

get_global_weighted_influence <- function(df, rule = "rule", weight_cols = c("confidence", "lift")) {
  total_rules <- nrow(df)
  
  df <- split_rules(df, rule)  # assuming this splits into columns LHS, RHS, confidence, lift, etc.
  
  influence_df <- df %>%
    # explode LHS into individual items
    mutate(LHS = strsplit(LHS, "\\s*,\\s*")) %>%
    unnest(LHS) %>%
    mutate(LHS = trimws(LHS)) %>%
    distinct(row_number(), LHS, .keep_all = TRUE) %>%
    
    # compute both types of weighted influence
    rowwise() %>%
    mutate(
      weight_multiply = prod(across(all_of(weight_cols))),
      weight_sum = sum(across(all_of(weight_cols)))
    ) %>%
    ungroup() %>%
    
    # aggregate by LHS item
    group_by(LHS) %>%
    summarise(
      influence_multiply = sum(weight_multiply) / total_rules,
      influence_sum = sum(weight_sum) / total_rules,
      .groups = "drop"
    ) %>%
    arrange(desc(influence_multiply))
  
  return(influence_df)
}