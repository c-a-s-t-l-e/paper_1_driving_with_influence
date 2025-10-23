decode_partial_vin <- function(df, vin_col, year_col) {
  base_url <- "https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVin/"
  vin_col <- rlang::ensym(vin_col)
  year_col <- rlang::ensym(year_col)
  
  # A cache to avoid duplicate requests
  vin_cache <- list()
  
  # Function to decode a single VIN
  decode_vin <- function(vin, year) {
    if (is.na(vin) || is.na(year)) return(NULL)
    cache_key <- paste0(vin, "_", year)
    
    if (!is.null(vin_cache[[cache_key]])) {
      return(vin_cache[[cache_key]])
    }
    
    full_url <- paste0(base_url, vin, "?format=xml&modelyear=", year)
    resp <- GET(full_url)
    
    if (status_code(resp) != 200) {
      warning(paste("Failed to fetch for VIN:", vin))
      return(NULL)
    }
    
    xml <- read_xml(content(resp, "text"))
    results <- xml_find_all(xml, ".//Results/DecodedVariable")
    
    if (length(results) == 0) {
      warning(paste("No results for VIN:", vin))
      return(NULL)
    }
    
    data <- lapply(results, function(node) {
      name <- xml_text(xml_find_first(node, "Variable"))
      value <- xml_text(xml_find_first(node, "Value"))
      if (is.na(value) || value == "") value <- NA
      setNames(list(value), name)
    })
    
    decoded <- bind_cols(data)
    vin_cache[[cache_key]] <<- decoded
    return(decoded)
  }
  
  # Apply decoding row-by-row
  decoded_data <- df %>%
    rowwise() %>%
    mutate(decoded = list(decode_vin(!!vin_col, !!year_col))) %>%
    unnest_wider(decoded, names_sep = "_decoded")
  
  return(decoded_data)
}