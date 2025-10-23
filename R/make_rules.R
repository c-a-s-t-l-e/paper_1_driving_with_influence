# ---------------------------------------------------------------------
# make_transactions: Convert a dataframe into transaction format
# for association rule mining using the arules package.
#
# Args:
#   df : A dataframe with categorical variables
#
# Returns:
#   An object of class "transactions" suitable for apriori().
# ---------------------------------------------------------------------
make_transactions <- function(df) {
  # Convert all columns to factors
  df <- df |>
    mutate(across(everything(), as.factor))
  
  # Convert dataframe to transactions
  trans <- as(df, "transactions")
  return(trans)
}

# ---------------------------------------------------------------------
# make_rules: Generate association rules from a dataframe
# using the apriori algorithm.
#
# Args:
#   df            : Input dataframe
#   sup_thresh    : Minimum support threshold (default = 0.05)
#   conf_thresh   : Minimum confidence threshold (default = 0.7)
#   minlen_thresh : Minimum number of items in a rule (default = 2)
#   global_transactions : Flag for using global transaction set (currently not used)
#
# Returns:
#   A set of association rules (arules object)
# ---------------------------------------------------------------------
make_rules <- function(
    df,
    sup_thresh = 0.05,
    conf_thresh = 0.7,
    minlen_thresh = 2,
    global_transactions = TRUE
) {
  # Convert dataframe to transactions
  trans <- make_transactions(df)
  
  # Generate rules using apriori
  rules <- apriori(
    trans,
    parameter = list(
      support = sup_thresh,
      confidence = conf_thresh,
      minlen = minlen_thresh
    )
  )
  
  rules
}

# ---------------------------------------------------------------------
# make_rules_df: Convert association rules into a tidy dataframe
# and filter for positive lift.
#
# Args:
#   df         : Input dataframe
#   sup_thresh : Minimum support threshold for rule generation
#   conf_thresh: Minimum confidence threshold for rule generation
#
# Returns:
#   A dataframe with columns: rule, support, confidence, lift
# ---------------------------------------------------------------------
make_rules_df <- function(df, sup_thresh = 0.05, conf_thresh = 0.7) {
  # Generate rules
  rules <- make_rules(df, sup_thresh, conf_thresh)
  
  # Convert rules object to dataframe and rename columns
  rules_df <- as(rules, "data.frame") %>%
    dplyr::rename(
      rule = rules,
      support = support,
      confidence = confidence,
      lift = lift
    )
  
  # Filter to keep only rules with positive lift
  rules_df <- rules_df |> filter_by_lift()
  
  rules_df
}
