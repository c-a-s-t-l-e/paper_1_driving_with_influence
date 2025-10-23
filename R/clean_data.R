# ---------------------------------------------------------------------
# clean_data: Preprocesses raw crash datasets for analysis
# Steps include:
#   - Standardizing column names
#   - Creating derived columns (collision type, weather, mileage, time info)
#   - Calculating vehicle age
#   - Removing unknown or missing pre-crash movement data
#   - Cleaning and standardizing categorical variables
#   - Selecting and preparing analysis-specific columns
# ---------------------------------------------------------------------
clean_data <- function(df) {
  df %>%
    
    # Standardize column names to snake_case
    janitor::clean_names() %>%
    
    # Create derived columns for analysis
    add_collision_type() %>%
    add_weather() %>%
    add_mileage_category() %>%
    add_time_columns() %>%
    add_time_of_day() %>%
    
    # Calculate vehicle age
    mutate(vehicle_age = year - model_year) %>%
    
    # Remove rows with missing or unknown pre-crash movement
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
    
    # Select and prepare columns for analysis
    get_analysis_columns(c(
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
