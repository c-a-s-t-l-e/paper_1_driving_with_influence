# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(arules)
library(forcats)
library(ggplot2)

library(quarto)

# Set target options:
tar_option_set(
  packages = c(
    # Packages that your targets need for their tasks.
    "readr",
    "dplyr",
    "tidyr",
    "stringr",
    "lubridate",
    "purrr",
    "arules",
    "forcats",
    "ggplot2"
  )
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
# Make a target list
list(
  
  ### READ IN THE DATA
  tar_target(
    paths,
    c("data/raw/ADS.csv", "data/raw/ADAS.csv")
  ),
  tar_target(files, paths, format = "file", pattern = map(paths)),
  tar_target(data, 
             read_csv(files), 
             pattern = map(files), 
             iteration = "list"),
  
  ### CLEAN DATA
  tar_target(
    cleaned_data,
    clean_data(data),
    pattern = map(data),
    iteration = "list" # to access the two individual datasets
  ),
  
  ### SEPARATE DATA BY ROADWAY TYPE
  tar_target(
    intersection_data,
    get_intersection_data(cleaned_data),
    pattern = map(cleaned_data),
    iteration = "list"
  ),
  tar_target(
    highway_data,
    get_highway_data(cleaned_data),
    pattern = map(cleaned_data),
    iteration = "list"
  ),
  
  ### GET THRESHOLD COUNTS
  tar_target(
    global_rule_threshold_count,
    get_rules_per_threshold(make_transactions(cleaned_data), support_values = seq(0.01, 0.07, by = 0.01), confidence_values = seq(0.6, 0.9, by = 0.1)),
    pattern = map(cleaned_data),
    iteration = "list"
  ),
  tar_target(
    highway_rule_threshold_count,
    get_rules_per_threshold(make_transactions(highway_data), support_values = seq(0.01, 0.07, by = 0.01), confidence_values = seq(0.6, 0.9, by = 0.1)),
    pattern = map(highway_data),
    iteration = "list"
  ),
  tar_target(
    intersection_rule_threshold_count,
    get_rules_per_threshold(make_transactions(intersection_data), support_values = seq(0.01, 0.07, by = 0.01), confidence_values = seq(0.6, 0.9, by = 0.1)),
    pattern = map(intersection_data),
    iteration = "list"
  ),
  
  ### PLOT THRESHOLDS
  tar_target(
    plot_global_rule_threshold_count,
    plot_rule_thresholds(global_rule_threshold_count),
    pattern = map(global_rule_threshold_count),
    iteration = "list"
  ),
  tar_target(
    plot_highway_rule_threshold_count,
    plot_rule_thresholds(highway_rule_threshold_count),
    pattern = map(highway_rule_threshold_count),
    iteration = "list"
  ),
  tar_target(
    plot_intersection_rule_threshold_count,
    plot_rule_thresholds(intersection_rule_threshold_count),
    pattern = map(intersection_rule_threshold_count),
    iteration = "list"
  ),
  
  ### MAKE RULES
  tar_target(
    global_rules_data,
    make_rules_df(cleaned_data, sup_thresh = 0.03, conf_thresh = 0.7),
    pattern = map(cleaned_data),
    iteration = "list"
  ),
  tar_target(
    intersection_rules_data,
    make_rules_df(intersection_data, sup_thresh = 0.03, conf_thresh = 0.7),
    pattern = map(intersection_data),
    iteration = "list"
  ),
  tar_target(
    highway_rules_data,
    make_rules_df(highway_data, sup_thresh = 0.03, conf_thresh = 0.7),
    pattern = map(highway_data),
    iteration = "list"
  ),
  
  ### GET INFLUENCE VALUES
  tar_target(
    global_items_influence,
    get_global_influence(global_rules_data),
    pattern = map(global_rules_data),
    iteration = "list"
  ),
  tar_target(
    highway_items_influence,
    get_global_influence(highway_rules_data),
    pattern = map(highway_rules_data),
    iteration = "list"
  ),
  tar_target(
    intersection_items_influence,
    get_global_influence(intersection_rules_data),
    pattern = map(intersection_rules_data),
    iteration = "list"
  ),
  
  ### MAKE INFLUENCE COMPARISON DATAFRAMES
  tar_target(
    global_items_compare,
    full_join(
      global_items_influence[[1]],
      global_items_influence[[2]],
      by = "LHS"
    ) %>%
      mutate(across(everything(), ~ tidyr::replace_na(., 0)))
  ),
  tar_target(
    highway_items_compare,
    full_join(
      highway_items_influence[[1]],
      highway_items_influence[[2]],
      by = "LHS"
    ) %>%
      mutate(across(everything(), ~ tidyr::replace_na(., 0)))
  ),
  tar_target(
    intersection_items_compare,
    full_join(
      intersection_items_influence[[1]],
      intersection_items_influence[[2]],
      by = "LHS"
    ) %>%
      mutate(across(everything(), ~ tidyr::replace_na(., 0)))
  ),
  
  ### PLOT INFLUENCE COMPARISON PLOTS
  tar_target(
    global_compare_plot,
    plot_compare_influence(global_items_compare)
  ),
  tar_target(
    highway_compare_plot,
    plot_compare_influence(highway_items_compare)
  ),
  tar_target(
    intersection_compare_plot,
    plot_compare_influence(intersection_items_compare)
  )
)
