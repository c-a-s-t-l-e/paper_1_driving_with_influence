# ---------- Clean ADAS Dataset ----------
clean_data <- function(df) {
  df %>%
    #mutate(dataset_type = dataset_name) |>

    janitor::clean_names() %>%

    # Derived columns
    add_collision_type() %>%
    add_weather() %>%
    add_mileage_category() %>%
    add_time_columns() %>%
    add_time_of_day() %>%
    mutate(vehicle_age = year - model_year) %>%

    # Remove unknown / NA pre-crash movement
    drop_na(cp_pre_crash_movement) %>%
    filter(cp_pre_crash_movement != "Unknown") %>%

    # Clean categorical variables
    replace_with_unknown(c(
      "city",
      "cp_pre_crash_movement",
      "sv_pre_crash_movement",
      "posted_speed_limit_mph",
      "month",
      "hour"
    )) %>%
    title_case_columns("collision_type") %>%
    replace_str_in_char_columns("Other, see Narrative", "Other") %>%

    # Get analysis columns
    get_analysis_columns(c(
      #"dataset_type",
      "weather",
      "roadway_type",
      "roadway_surface",
      "posted_speed_limit_mph",
      "lighting",
      "crash_with",
      "cp_pre_crash_movement",
      "sv_pre_crash_movement",
      "collision_type",
      "mileage_category",
      "vehicle_age",
      "time_of_day"
    ))
}
