# Make transactions to turn into rules
make_transactions <- function(df) {
  df <- df |>
    mutate(across(everything(), as.factor))

  trans <- as(df, "transactions")
  return(trans)
}


# Make rules using apriori algorithm
make_rules <- function(
  df,
  sup_thresh = 0.05,
  conf_thresh = 0.7,
  minlen_thresh = 2,
  global_transactions = TRUE
) {
  trans <- make_transactions(df)

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
