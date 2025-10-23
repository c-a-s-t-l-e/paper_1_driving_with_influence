# ---------------------------------------------------------------------
# get_rules_per_threshold: Compute the number of association rules generated
# across a grid of support and confidence thresholds.
#
# Args:
#   transactions     : A transaction dataset (arules format)
#   support_values   : Vector of minimum support thresholds to test
#   confidence_values: Vector of minimum confidence thresholds to test
#   maxlen           : Maximum number of items per rule (default = 10)
#
# Returns:
#   A data frame with columns:
#     - support      : support threshold used
#     - confidence   : confidence threshold used
#     - rule_count   : number of rules generated for that threshold pair
# ---------------------------------------------------------------------
get_rules_per_threshold <- function(
    transactions,
    support_values,
    confidence_values,
    maxlen = 10
) {
  # Create a grid of all support-confidence combinations
  results <- expand.grid(
    support = support_values,
    confidence = confidence_values
  )
  results$rule_count <- NA  # placeholder for rule counts
  
  # Loop through each threshold combination
  for (i in 1:nrow(results)) {
    s <- results$support[i]
    c <- results$confidence[i]
    
    # Generate association rules for the current thresholds
    rules <- apriori(
      transactions,
      parameter = list(supp = s, conf = c, maxlen = maxlen, target = "rules")
    )
    
    # Record the number of rules generated
    results$rule_count[i] <- length(rules)
  }
  
  return(results)
}
