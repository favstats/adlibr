# Amazon Ads (EU) Ad Library API
#
# This file contains platform-specific functions for accessing
# Amazon's EU Ad Library API.

#' Build Amazon-Specific Request
#' 
#' @description
#' Builds a request specifically for Amazon's EU Ad Library API.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_amazon_request <- function(operation, params, config) {
  
  # Amazon EU Ad Library is currently a public API without authentication
  base_url <- config$base
  
  # Create request
  req <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_headers(
      Accept = "application/json",
      `Content-Type` = "application/json"
    ) |>
    httr2::req_timeout(config$default_timeout %||% 30)
  
  # Add query parameters
  query_params <- build_amazon_params(params, operation, config)
  if (length(query_params) > 0) {
    req <- httr2::req_url_query(req, !!!query_params)
  }
  
  return(req)
}

#' Build Amazon-Specific Parameters
#' 
#' @description
#' Converts unified parameters to Amazon's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. Amazon-formatted parameters
#' @keywords internal
build_amazon_params <- function(params, operation, config) {
  
  amazon_params <- list()
  
  if (operation == "search_ads") {
    
    # Required: advertiser
    if (!is.null(params$advertisers)) {
      amazon_params$advertiser <- sanitize_string(params$advertisers[1])
    } else {
      cli::cli_warn("Amazon ad search typically requires advertiser name")
    }
    
    # Optional: country (EU countries only)
    if (!is.null(params$countries)) {
      # Amazon EU only supports EU countries
      eu_countries <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", 
                       "FR", "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", 
                       "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE")
      
      countries <- normalize_country_codes(params$countries)
      valid_countries <- intersect(countries, eu_countries)
      
      if (length(valid_countries) > 0) {
        amazon_params$country <- valid_countries[1]  # Take first valid EU country
      } else {
        cli::cli_warn("Amazon EU Ad Library only supports EU countries")
        amazon_params$country <- "DE"  # Default to Germany
      }
    } else {
      amazon_params$country <- "DE"  # Default
    }
    
    # Date range (if supported)
    if (!is.null(params$date_from)) {
      amazon_params$start_date <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      amazon_params$end_date <- format_date_for_api(params$date_to, "iso")
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      amazon_params$limit <- min(as.integer(params$limit), 100)
    } else {
      amazon_params$limit <- 100
    }
    
    if (!is.null(params$page_token)) {
      amazon_params$offset <- as.integer(params$page_token)
    } else {
      amazon_params$offset <- 0
    }
    
  } else if (operation == "advertisers") {
    
    # Search by advertiser name
    if (!is.null(params$keyword)) {
      amazon_params$name <- sanitize_string(params$keyword)
    }
    
    # Country filter
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      amazon_params$country <- countries[1]
    } else {
      amazon_params$country <- "DE"
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      amazon_params$limit <- min(as.integer(params$limit), 100)
    } else {
      amazon_params$limit <- 100
    }
    
    if (!is.null(params$page_token)) {
      amazon_params$offset <- as.integer(params$page_token)
    } else {
      amazon_params$offset <- 0
    }
  }
  
  return(amazon_params)
}

#' Parse Amazon Response
#' 
#' @description
#' Parses Amazon API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_amazon_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # Amazon returns ads in data array
    ads <- content$data %||% content$ads %||% list()
    
    result <- list(
      data = ads,
      pagination = content$pagination %||% list(),
      total_count = content$total %||% length(ads)
    )
    
  } else if (operation == "advertisers") {
    # Amazon returns advertisers
    advertisers <- content$data %||% content$advertisers %||% list()
    
    result <- list(
      data = advertisers,
      pagination = content$pagination %||% list(),
      total_count = content$total %||% length(advertisers)
    )
  }
  
  return(result)
}

#' Normalize Amazon Data to Common Schema
#' 
#' @description
#' Converts Amazon EU Ad Library data to adlibr normalized schema.
#' 
#' @param raw_data List. Raw Amazon API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_amazon_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$data) || length(raw_data$data) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$data
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "id") %||% safe_extract(ad, "ad_id")
    advertiser_id <- safe_extract(ad, "advertiser_id")
    advertiser_name <- safe_extract(ad, "advertiser_name") %||% safe_extract(ad, "advertiser")
    
    # Dates
    first_seen <- parse_api_date(safe_extract(ad, "first_shown") %||% safe_extract(ad, "start_date"))
    last_seen <- parse_api_date(safe_extract(ad, "last_shown") %||% safe_extract(ad, "end_date"))
    
    # Ad content
    ad_text <- safe_extract(ad, "ad_text") %||% safe_extract(ad, "creative_text")
    
    # Media information
    media_type <- determine_media_type(safe_extract(ad, "creative_type"), safe_extract(ad, "media_type"))
    
    # Geographic data
    country <- safe_extract(ad, "country")
    countries <- if (!is.null(country)) {
      list(country)
    } else {
      list(character(0))
    }
    
    # Targeting (limited in Amazon data)
    targeting_age <- list(safe_extract(ad, "age_targeting", character(0)))
    targeting_gender <- list(safe_extract(ad, "gender_targeting", character(0)))
    targeting_locations <- list(safe_extract(ad, "location_targeting", character(0)))
    
    # Spend information (if available)
    spend_range <- safe_extract(ad, "spend_range") %||% safe_extract(ad, "spend")
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "amazon",
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      advertiser_verified = NA,
      advertiser_type = NA_character_,
      payer = NA_character_,
      beneficiary = NA_character_,
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
      ad_creative_urls = list(character(0)),
      ad_library_url = NA_character_,
      media_type = as.character(media_type),
      media_urls = list(character(0)),
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_range = as.character(spend_range),
      impressions_range = NA_character_,
      eu_reach = NA_character_,
      targeting_age = targeting_age,
      targeting_gender = targeting_gender,
      targeting_locations = targeting_locations,
      targeting_interests = list(character(0)),
      targeting_behaviors = list(character(0)),
      targeting_custom_audiences = list(character(0)),
      targeting_exclusions = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Validate Amazon Credentials
#' 
#' @description
#' Validates Amazon API access (public API, so mainly tests connectivity).
#' 
#' @return List with validation results
#' @keywords internal
validate_amazon_credentials <- function() {
  
  # Test basic connectivity
  tryCatch({
    config <- get_platform_config("amazon")
    
    req <- httr2::request(config$base) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(
        valid = TRUE,
        message = "Amazon EU Ad Library accessible",
        authenticated = FALSE  # Public API
      ))
    } else {
      return(list(
        valid = FALSE,
        message = paste("Amazon API returned status", httr2::resp_status(response)),
        error = "api_error"
      ))
    }
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = paste("Amazon API connection failed:", e$message),
      error = "connection_error"
    ))
  })
}
