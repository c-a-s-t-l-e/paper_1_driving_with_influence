# Dataframe filtering functions

get_positive_lift <- function(df) {
  df <- df |> filter(lift > 1)
}

get_negative_lift <- function(df) {
  df <- df |> filter(lift < 1)
}

filter_by_lift <- function(df, positive_lift = TRUE) {
  if (positive_lift) get_positive_lift(df) else get_negative_lift(df)
}

# Dataframe manipulation helpers

split_rules <- function(df, col) {
  # Ensure rules are characters
  rules <- as.character(df[[col]])

  # Use regex to split at the " => "
  lhs <- sub("\\s*=>.*", "", rules) # everything before =>
  rhs <- sub(".*=>\\s*", "", rules) # everything after =>

  df <- df |>
    mutate(
      LHS = gsub("[{}]", "", lhs), # remove curly braces
      RHS = gsub("[{}]", "", rhs), # remove curly braces
    )
}
