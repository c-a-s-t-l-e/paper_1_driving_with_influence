# ---------------------------------------------------------------------
# get_intersection_data: Filter dataset to only include intersection crashes
#   - Keeps rows where roadway_type is "Intersection"
#   - Removes the roadway_type column since it is now redundant
# ---------------------------------------------------------------------
get_intersection_data <- function(df) {
  df <- df |>
    filter(
      roadway_type == "Intersection"  # keep only intersection crashes
    ) |>
    select(-roadway_type)  # drop the column since all rows are now intersections
}

# ---------------------------------------------------------------------
# get_highway_data: Filter dataset to only include highway/freeway crashes
#   - Keeps rows where roadway_type is "Highway / Freeway"
#   - Removes the roadway_type column since it is now redundant
# ---------------------------------------------------------------------
get_highway_data <- function(df) {
  df <- df |>
    filter(
      roadway_type == "Highway / Freeway"  # keep only highway/freeway crashes
    ) |>
    select(-roadway_type)  # drop the column since all rows are now highways
}
