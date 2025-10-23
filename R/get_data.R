get_intersection_data <- function(df) {
  df <- df |>
    filter(
      roadway_type == "Intersection"
    ) |>
    select(-roadway_type)
}

get_highway_data <- function(df) {
  df <- df |>
    filter(
      roadway_type == "Highway / Freeway"
    ) |>
    select(-roadway_type)
}
