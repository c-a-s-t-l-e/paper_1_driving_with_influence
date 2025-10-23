# ---------- Helper Functions ----------

# Check if any columns have "Y"
any_y <- function(df, cols) {
  rowSums(df[cols] == "Y", na.rm = TRUE) > 0
}

# Collision type
add_collision_type <- function(df) {
  df %>%
    mutate(
      collision_type = case_when(

        # --- Rear-end collisions ---
        any_y(
          df,
          c("sv_contact_area_rear", "sv_contact_area_rear_left", "sv_contact_area_rear_right")
        ) &
          any_y(
            df,
            c("cp_contact_area_front", "cp_contact_area_front_left", "cp_contact_area_front_right")
          ) ~ "SV rear-ended by CP",

        any_y(
          df,
          c("cp_contact_area_rear", "cp_contact_area_rear_left", "cp_contact_area_rear_right")
        ) &
          any_y(
            df,
            c("sv_contact_area_front", "sv_contact_area_front_left", "sv_contact_area_front_right")
          ) ~ "CP rear-ended by SV",

        # --- Head-on collisions ---
        any_y(
          df,
          c("sv_contact_area_front", "sv_contact_area_front_left", "sv_contact_area_front_right")
        ) &
          any_y(
            df,
            c("cp_contact_area_front", "cp_contact_area_front_left", "cp_contact_area_front_right")
          ) ~ "Head-on collision",

        # --- T-bone (side-impact) collisions ---
        any_y(
          df,
          c("sv_contact_area_left", "sv_contact_area_right")
        ) &
          any_y(
            df,
            c("cp_contact_area_front", "cp_contact_area_rear")
          ) ~ "T-bone collision (SV side impacted)",

        any_y(
          df,
          c("cp_contact_area_left", "cp_contact_area_right")
        ) &
          any_y(
            df,
            c("sv_contact_area_front", "sv_contact_area_rear")
          ) ~ "T-bone collision (CP side impacted)",

        # --- Sideswipe collisions (both have side contact) ---
        any_y(
          df,
          c("sv_contact_area_left", "sv_contact_area_right")
        ) &
          any_y(
            df,
            c("cp_contact_area_left", "cp_contact_area_right")
          ) ~ "Sideswipe collision",

        # --- Fallback category ---
        TRUE ~ "Other / unknown"
      )
    )
}

# Weather
add_weather <- function(df) {
  df %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(
      cols = starts_with("weather_"),
      names_to = "weather",
      values_to = "occurred"
    ) %>%
    filter(occurred == "Y") %>%
    mutate(weather = str_remove(weather, "^weather_")) %>%
    group_by(row_id) %>%
    summarise(
      weather = str_to_title(paste(weather, collapse = ", ")),
      .groups = "drop"
    ) %>%
    right_join(df %>% mutate(row_id = row_number()), by = "row_id") %>%
    select(-row_id)
}

# Mileage category
add_mileage_category <- function(df) {
  df %>%
    filter(!is.na(mileage)) %>%
    mutate(
      mileage_category = cut(
        mileage,
        breaks = c(-Inf, 30000, 60000, 100000, 150000, Inf),
        labels = c("Low", "Low Medium", "High Medium", "High", "Very High")
      )
    )
}

# Time columns
add_time_columns <- function(df) {
  df %>%
    mutate(
      date = lubridate::my(incident_date),
      month = month(date),
      year = year(date),
      hour = hour(round_date(
        as.POSIXct(incident_time_24_00, format = "%H:%M:%S"),
        unit = "hour"
      ))
    ) %>%
    select(-c(incident_date, incident_time_24_00, date))
}

# Time of day
add_time_of_day <- function(df) {
  df %>%
    mutate(
      time_of_day = case_when(
        hour >= 5 & hour < 12 ~ "Morning",
        hour >= 12 & hour < 17 ~ "Afternoon",
        hour >= 17 & hour < 21 ~ "Evening",
        TRUE ~ "Night"
      )
    )
}

# Replace missing with "Unknown"
replace_with_unknown <- function(df, columns) {
  existing_cols <- intersect(columns, names(df))
  df %>%
    mutate(across(
      all_of(existing_cols),
      ~ replace_na(as.character(.x), "Unknown")
    ))
}

# Title case
title_case_columns <- function(df, columns, exceptions = c("SV", "CP")) {
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- map_chr(
        df[[col]],
        ~ {
          words <- str_split(.x, " ")[[1]]
          words <- map_chr(
            words,
            ~ ifelse(toupper(.x) %in% exceptions, toupper(.x), str_to_title(.x))
          )
          paste(words, collapse = " ")
        }
      )
    }
  }
  df
}

# Replace string in all character columns
replace_str_in_char_columns <- function(df, pattern, replacement) {
  df %>%
    mutate(across(where(is.character), ~ str_replace(.x, pattern, replacement)))
}

# Filter analysis columns
get_analysis_columns <- function(df, columns) {
  df %>%
    filter(
      report_version == 1,
      roadway_type %in% c("Intersection", "Highway / Freeway")
    ) %>%
    select(all_of(columns))
}
