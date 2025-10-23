# ---------------------------------------------------------------------
# Dataframe filtering functions
# ---------------------------------------------------------------------

# Keep only rows where lift > 1
get_positive_lift <- function(df) {
  df <- df |> 
    filter(lift > 1)  # filter for positive association rules
}

# Keep only rows where lift < 1
get_negative_lift <- function(df) {
  df <- df |> filter(lift < 1) # filter for negative association rules
}

# Filter by lift, optionally keeping only positive lift rules
filter_by_lift <- function(df, positive_lift = TRUE) {
  if (positive_lift) {
    get_positive_lift(df)  # keep positive lift rules
  } else {
    get_negative_lift(df)  # keep negative lift rules (function must exist)
  }
}


# ---------------------------------------------------------------------
# Dataframe manipulation helpers
# ---------------------------------------------------------------------

# Split association rules into LHS and RHS columns
split_rules <- function(df, col) {
  # Ensure rules are character strings
  rules <- as.character(df[[col]])
  
  # Extract left-hand side (everything before "=>")
  lhs <- sub("\\s*=>.*", "", rules)
  
  # Extract right-hand side (everything after "=>")
  rhs <- sub(".*=>\\s*", "", rules)
  
  # Create new columns LHS and RHS without curly braces
  df <- df |>
    mutate(
      LHS = gsub("[{}]", "", lhs),  # clean curly braces from LHS
      RHS = gsub("[{}]", "", rhs)   # clean curly braces from RHS
    )
}