# Helper to convert rules to data frame
make_rules_df <- function(df, sup_thresh = 0.05, conf_thresh = 0.7) {
  rules <- make_rules(df, sup_thresh, conf_thresh)

  rules_df <- as(rules, "data.frame") %>%
    dplyr::rename(
      rule = rules,
      support = support,
      confidence = confidence,
      lift = lift
    )

  rules_df <- rules_df |> filter_by_lift()

  rules_df
}
