library(plumber)
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(stringr)

# --- Operational Database Connection Helper ---
get_db_con <- function() {
  tryCatch({
    dbConnect(
      RPostgres::Postgres(),
      dbname   = "rv_db",
      host     = "dpg-cplf398cmk4c739ne890-a.oregon-postgres.render.com",
      port     = 5432,
      user     = "rv_db_user",
      password = "ZxW5RZjZH0WX872yX8DfgQG1ugb3Y5Nl"
    )
  }, error = function(e) { NULL })
}

#* Enable CORS for cross-domain WordPress and agency requests
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @apiTitle RV Valuation & Comp API
#* @apiDescription Calculates active market median, 25th/75th percentiles, and 4-week market trends based on Canadian dealership inventory.

#* @param year Model Year (e.g., 2021)
#* @param rv_type Type of RV (e.g., "Travel Trailer")
#* @param manufacturer Manufacturer name (Optional)
#* @param brand Brand name (Optional)
#* @param model_name Specific Model (Optional)
#* @param province Filter by Province code (Optional)
#* @param keyword Optional search keyword or floorplan
#* @get /valuation
#* @post /valuation
function(year = "", rv_type = "", manufacturer = "", brand = "", 
         model_name = "", province = "", keyword = "") {
  
  con <- get_db_con()
  if (is.null(con)) {
    return(list(status = "error", message = "Database connection failed"))
  }
  
  # Fetch rolling 35 days of active inventory
  query <- "
    SELECT * 
    FROM model.canada_available 
    WHERE \"CollectionDate\" >= (SELECT MAX(\"CollectionDate\") FROM model.canada_available) - INTERVAL '34 days'
  "
  rv_data <- dbGetQuery(con, query) %>%
    mutate(across(any_of(c("ManufacturerNew", "Brand", "Model", "State")), ~ifelse(is.na(.) | . == "", "Unknown", .)))
  
  # Apply User Filters
  res_df <- rv_data %>% filter(Price < 999000)
  
  if (year != "") res_df <- res_df %>% filter(Year == as.numeric(year))
  if (rv_type != "") res_df <- res_df %>% filter(RV_Type_1 %in% rv_type)
  if (manufacturer != "") res_df <- res_df %>% filter(ManufacturerNew %in% manufacturer)
  if (brand != "") res_df <- res_df %>% filter(Brand %in% brand)
  if (model_name != "") res_df <- res_df %>% filter(Model %in% model_name)
  if (province != "") res_df <- res_df %>% filter(State %in% province)
  if (keyword != "") {
    res_df <- res_df %>% filter(str_detect(ProductName, pattern = fixed(keyword, ignore_case = TRUE)))
  }
  
  # Return early if no matches
  if (nrow(res_df) == 0) {
    dbDisconnect(con)
    return(list(
      status = "no_comps_found",
      message = "No active listings match this criteria.",
      units_available = 0
    ))
  }
  
  # Current Week Comp Calculations
  latest_week <- max(floor_date(as.Date(res_df$CollectionDate), unit = "week", week_start = 3), na.rm = TRUE)
  current_data <- res_df %>% filter(floor_date(as.Date(CollectionDate), unit = "week", week_start = 3) == latest_week)
  valid_prices <- current_data$Price[current_data$Price > 200]
  
  units_count <- nrow(current_data)
  
  if (length(valid_prices) > 0) {
    median_val <- unname(median(valid_prices, na.rm = TRUE))
    quantiles  <- unname(quantile(valid_prices, probs = c(0.25, 0.75), na.rm = TRUE))
    low_25th   <- quantiles[1]
    high_75th  <- quantiles[2]
  } else {
    median_val <- NA; low_25th <- NA; high_75th <- NA
  }
  
  # 4-Week Trend Analysis Logic
  weekly_trend <- res_df %>%
    mutate(Week = floor_date(as.Date(CollectionDate), unit = "week", week_start = 3)) %>%
    group_by(Week) %>%
    summarize(MedianPrice = median(Price[Price > 200], na.rm = TRUE), Units = n(), .groups = "drop") %>%
    arrange(desc(Week)) %>%
    slice(1:4)
  
  avg_4wk_price <- unname(mean(weekly_trend$MedianPrice, na.rm = TRUE))
  avg_4wk_units <- unname(round(mean(weekly_trend$Units, na.rm = TRUE), 0))
  
  # Active Telemetry DB Logging
  telemetry_entry <- data.frame(
    username = "api_client",
    session_id = "api_request",
    search_time = format(Sys.time(), "%Y-%m-%d %I:%M %p"),
    search_text = paste0("Year: ", year, " | Type: ", rv_type, " | Mfr: ", manufacturer, " | Brand: ", brand),
    results_count = units_count,
    min_price = ifelse(is.na(low_25th), NA_real_, low_25th),
    avg_price = ifelse(is.na(median_val), NA_real_, median_val),
    max_price = ifelse(is.na(high_75th), NA_real_, high_75th),
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    dbWriteTable(con, Id(schema = "model", table = "telemetry_searches"), telemetry_entry, append = TRUE, row.names = FALSE)
  }, error = function(e) NULL)
  
  dbDisconnect(con)
  
  # JSON Output Payload
  response_payload <- list(
    status = "success",
    units_available = units_count,
    pricing = list(
      low_25th_percentile = low_25th,
      median_price = median_val,
      high_75th_percentile = high_75th
    ),
    trends_4_week = list(
      avg_4wk_median_price = avg_4wk_price,
      avg_4wk_units = avg_4wk_units
    )
  )
  
  # Safely attach the warning if necessary
  if (length(valid_prices) < 3) {
    response_payload$warning <- "Fewer than 3 comps found. Widen search criteria."
  }
  
  # ... existing valuation code above ...
  
  return(response_payload)
}

# ==========================================
# CASCADING LOOKUP ENDPOINTS (ADD BELOW HERE)
# ==========================================

#* Fetch distinct active manufacturers
#* @get /manufacturers
function() {
  con <- get_db_con()
  if (is.null(con)) return(list())
  
  query <- "
    SELECT DISTINCT \"ManufacturerNew\" AS mfr 
    FROM model.canada_available 
    WHERE \"ManufacturerNew\" IS NOT NULL AND \"ManufacturerNew\" != '' AND \"ManufacturerNew\" != 'Unknown'
    ORDER BY mfr
  "
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(res$mfr)
}

#* Fetch distinct brands for a selected manufacturer
#* @param manufacturer Selected manufacturer name
#* @get /brands
function(manufacturer = "") {
  con <- get_db_con()
  if (is.null(con)) return(list())
  
  query <- sprintf("
    SELECT DISTINCT \"Brand\" AS brand 
    FROM model.canada_available 
    WHERE \"ManufacturerNew\" = %s AND \"Brand\" IS NOT NULL AND \"Brand\" != '' AND \"Brand\" != 'Unknown'
    ORDER BY brand
  ", dbQuoteString(con, manufacturer))
  
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(res$brand)
}
