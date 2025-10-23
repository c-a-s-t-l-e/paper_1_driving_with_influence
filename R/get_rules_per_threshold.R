library(arules)

# Function to calculate number of rules for a grid of support & confidence thresholds
get_rules_per_threshold <- function(
  transactions,
  support_values,
  confidence_values,
  maxlen = 10
) {
  results <- expand.grid(
    support = support_values,
    confidence = confidence_values
  )
  results$rule_count <- NA

  for (i in 1:nrow(results)) {
    s <- results$support[i]
    c <- results$confidence[i]

    rules <- apriori(
      transactions,
      parameter = list(supp = s, conf = c, maxlen = maxlen, target = "rules")
    )

    results$rule_count[i] <- length(rules)
  }

  return(results)
}

# Example usage:
# Suppose 'Groceries' is your transaction dataset
# data(Groceries)  # comes with arules
# supports <- seq(0.01, 0.05, by = 0.01)
# confidences <- seq(0.1, 0.9, by = 0.2)
# rule_counts <- rule_count_grid(Groceries, supports, confidences)
# print(rule_counts)
