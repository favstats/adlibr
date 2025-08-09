# Utility Functions for adlibr
#
# This file contains utility functions used throughout the adlibr package.

#' Create User Agent String
#' 
#' @description
#' Creates a standardized User-Agent string for HTTP requests.
#' 
#' @return Character. User-Agent string
#' @keywords internal
create_user_agent <- function() {
  version <- utils::packageVersion("adlibr")
  glue::glue("adlibr/{version} (+https://github.com/favstats/adlibr)")
}

#' Check Internet Connection
#' 
#' @description
#' Checks if an internet connection is available.
#' 
#' @param timeout Numeric. Timeout in seconds for the connection test
#' 
#' @return Logical. TRUE if internet is available
#' @keywords internal
check_internet <- function(timeout = 5) {
  tryCatch({
    con <- url("https://www.google.com", timeout = timeout)
    on.exit(close(con))
    readLines(con, n = 1, warn = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Sanitize String for URLs
#' 
#' @description
#' Sanitizes strings for safe inclusion in URLs.
#' 
#' @param x Character. String to sanitize
#' @param encode_spaces Logical. Whether to URL encode spaces
#' 
#' @return Character. Sanitized string
#' @keywords internal
#' 
#' @importFrom stringi stri_enc_toutf8 stri_trim_both
sanitize_string <- function(x, encode_spaces = TRUE) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  
  # Convert to UTF-8 and trim whitespace
  x <- stringi::stri_enc_toutf8(x)
  x <- stringi::stri_trim_both(x)
  
  # Remove or replace problematic characters
  x <- gsub("[\\r\\n\\t]", " ", x)  # Replace line breaks with spaces
  x <- gsub("\\s+", " ", x)         # Collapse multiple spaces
  
  # URL encode if requested
  if (encode_spaces) {
    x <- gsub(" ", "%20", x)
  }
  
  return(x)
}

#' Convert Date to API Format
#' 
#' @description
#' Converts dates to the appropriate format for different platforms.
#' 
#' @param date Date, POSIXct, or character. Date to convert
#' @param format Character. Target format ("iso", "epoch", "custom")
#' @param platform Character. Platform name for platform-specific formatting
#' 
#' @return Character or numeric. Formatted date
#' @keywords internal
#' 
#' @importFrom clock as_date date_format
format_date_for_api <- function(date, format = "iso", platform = NULL) {
  if (is.null(date)) {
    return(NULL)
  }
  
  # Convert to date object
  date_obj <- tryCatch({
    clock::as_date(date)
  }, error = function(e) {
    cli::cli_abort("Invalid date format: {.val {date}}")
  })
  
  switch(format,
    "iso" = clock::date_format(date_obj, format = "%Y-%m-%d"),
    "epoch" = as.numeric(as.POSIXct(date_obj)) * 1000,  # Milliseconds
    "custom" = {
      if (is.null(platform)) {
        return(clock::date_format(date_obj, format = "%Y-%m-%d"))
      }
      
      # Platform-specific date formatting
      switch(platform,
        "meta" = clock::date_format(date_obj, format = "%Y-%m-%d"),
        "tiktok" = clock::date_format(date_obj, format = "%Y-%m-%d"),
        "microsoft" = clock::date_format(date_obj, format = "%Y-%m-%d"),
        "apple" = "LAST_90_DAYS",  # Apple uses presets
        clock::date_format(date_obj, format = "%Y-%m-%d")
      )
    },
    clock::date_format(date_obj, format = "%Y-%m-%d")
  )
}

#' Parse Date from API Response
#' 
#' @description
#' Parses dates from API responses, handling various formats.
#' 
#' @param date_string Character or numeric. Date string or timestamp from API
#' @param format Character. Expected input format ("iso", "epoch", "auto")
#' 
#' @return POSIXct. Parsed datetime in UTC
#' @keywords internal
#' 
#' @importFrom clock as_date_time
parse_api_date <- function(date_string, format = "auto") {
  if (is.null(date_string) || is.na(date_string) || date_string == "") {
    return(as.POSIXct(NA))
  }
  
  tryCatch({
    switch(format,
      "epoch" = {
        # Handle both seconds and milliseconds timestamps
        timestamp <- as.numeric(date_string)
        if (timestamp > 1e10) {  # Likely milliseconds
          timestamp <- timestamp / 1000
        }
        as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
      },
      "iso" = {
        clock::as_date_time(date_string)
      },
      "auto" = {
        # Try to detect format automatically
        if (grepl("^\\d{10,13}$", as.character(date_string))) {
          # Looks like a timestamp
          parse_api_date(date_string, "epoch")
        } else {
          # Try as ISO date
          parse_api_date(date_string, "iso")
        }
      },
      as.POSIXct(NA)
    )
  }, error = function(e) {
    cli::cli_warn("Failed to parse date: {.val {date_string}}")
    return(as.POSIXct(NA))
  })
}

#' Normalize Country Codes
#' 
#' @description
#' Normalizes country codes to uppercase ISO 3166-1 alpha-2 format.
#' 
#' @param countries Character vector. Country codes to normalize
#' 
#' @return Character vector. Normalized country codes
#' @keywords internal
normalize_country_codes <- function(countries) {
  if (is.null(countries) || length(countries) == 0) {
    return(character(0))
  }
  
  # Convert to uppercase and trim
  countries <- toupper(stringi::stri_trim_both(countries))
  
  # Remove empty strings
  countries <- countries[countries != ""]
  
  # Validate format (2 letters)
  valid_countries <- countries[grepl("^[A-Z]{2}$", countries)]
  
  if (length(valid_countries) != length(countries)) {
    invalid <- setdiff(countries, valid_countries)
    cli::cli_warn("Invalid country codes removed: {.val {invalid}}")
  }
  
  return(valid_countries)
}

#' Create Empty Normalized Tibble
#' 
#' @description
#' Creates an empty tibble with the normalized schema columns.
#' 
#' @return tibble. Empty tibble with proper column types
#' @keywords internal
#' 
#' @importFrom tibble tibble
#' @importFrom clock sys_time_now
create_empty_normalized_tibble <- function() {
  tibble::tibble(
    ad_id = character(0),
    platform = character(0),
    advertiser_id = character(0),
    advertiser_name = character(0),
    payer = character(0),
    beneficiary = character(0),
    status = character(0),
    first_seen = as.Date(integer(0), origin = "1970-01-01"),
    last_seen = as.Date(integer(0), origin = "1970-01-01"),
    ad_text = character(0),
    media_type = character(0),
    media_urls = list(),
    landing_url = character(0),
    countries = list(),
    languages = list(),
    spend_range = character(0),
    impressions_range = character(0),
    eu_reach = character(0),
    targeting_age = list(),
    targeting_gender = list(),
    targeting_locations = list(),
    targeting_other = list(),
    extra_json = list(),
    fetched_at = as.POSIXct(integer(0), origin = "1970-01-01")
  )
}

#' Add Fetched Timestamp
#' 
#' @description
#' Adds a fetched_at timestamp to data.
#' 
#' @param data Data frame or tibble
#' 
#' @return Data frame with fetched_at column
#' @keywords internal
#' 
#' @importFrom clock sys_time_now
add_fetched_timestamp <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  
  data$fetched_at <- clock::sys_time_now()
  return(data)
}

#' Collapse List Column to String
#' 
#' @description
#' Safely collapses list columns to character strings for display.
#' 
#' @param x List or vector
#' @param sep Character. Separator string
#' @param max_items Integer. Maximum items to include
#' 
#' @return Character. Collapsed string
#' @keywords internal
collapse_list_column <- function(x, sep = ", ", max_items = 5) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  if (!is.list(x)) {
    x <- as.list(x)
  }
  
  # Flatten nested lists
  x <- unlist(x, recursive = TRUE)
  
  # Remove NA and empty values
  x <- x[!is.na(x) & x != ""]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  # Limit number of items
  if (length(x) > max_items) {
    x <- c(x[1:max_items], "...")
  }
  
  paste(x, collapse = sep)
}

#' Retry with Exponential Backoff
#' 
#' @description
#' Executes a function with exponential backoff retry logic.
#' 
#' @param expr Expression to execute
#' @param max_attempts Integer. Maximum number of attempts
#' @param initial_wait Numeric. Initial wait time in seconds
#' @param backoff_factor Numeric. Multiplier for wait time
#' @param jitter Logical. Whether to add random jitter
#' 
#' @return Result of successful execution
#' @keywords internal
retry_with_backoff <- function(expr, max_attempts = 3, initial_wait = 1, 
                              backoff_factor = 2, jitter = TRUE) {
  
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      expr
    }, error = function(e) {
      if (attempt == max_attempts) {
        # Re-throw error on final attempt
        stop(e)
      }
      
      # Calculate wait time
      wait_time <- initial_wait * (backoff_factor ^ (attempt - 1))
      
      # Add jitter if requested
      if (jitter) {
        wait_time <- wait_time * (0.5 + runif(1) * 0.5)
      }
      
      cli::cli_inform("Attempt {attempt} failed, retrying in {round(wait_time, 1)} seconds...")
      Sys.sleep(wait_time)
      
      return("retry")
    })
    
    if (!identical(result, "retry")) {
      return(result)
    }
  }
}

#' Check Required Environment Variables
#' 
#' @description
#' Checks if required environment variables are set for a platform.
#' 
#' @param platform Character. Platform name
#' @param config List. Platform configuration
#' 
#' @return Logical. TRUE if all required env vars are set
#' @keywords internal
check_env_vars <- function(platform, config = NULL) {
  if (is.null(config)) {
    config <- get_platform_config(platform)
  }
  
  # Check if authentication is public or optional
  auth_type <- config$auth %||% "required"
  if (auth_type %in% c("public", "public_https", "graphql_public", "optional_developer_token")) {
    return(TRUE)  # Public APIs or optional auth don't require env vars
  }
  
  if (!"env_vars" %in% names(config) || length(config$env_vars) == 0) {
    return(TRUE)  # No env vars required
  }
  
  missing_vars <- character(0)
  
  for (var in config$env_vars) {
    if (Sys.getenv(var) == "") {
      missing_vars <- c(missing_vars, var)
    }
  }
  
  if (length(missing_vars) > 0) {
    cli::cli_warn(
      "Missing environment variables for {.val {platform}}: {.val {missing_vars}}",
      "i" = "Use {.fun adlibr_auth_setup} to configure authentication"
    )
    return(FALSE)
  }
  
  return(TRUE)
}
