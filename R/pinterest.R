# Pinterest Ad Library API
#
# This file contains platform-specific functions for accessing
# Pinterest's Ad Library API.

#' Build Pinterest-Specific Request
#' 
#' @description
#' Builds a request specifically for Pinterest's Ad Library API.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_pinterest_request <- function(operation, params, config) {
  
  # Get Pinterest access token
  access_token <- get_credential("PINTEREST_TOKEN")
  if (is.null(access_token)) {
    cli::cli_abort("PINTEREST_TOKEN environment variable required for Pinterest API access")
  }
  
  # Build endpoint URL based on operation
  endpoint <- switch(operation,
    "search_ads" = "ads/search",
    "advertisers" = "advertisers",
    cli::cli_abort("Unknown Pinterest operation: {operation}")
  )
  
  base_url <- paste0(config$base, "/", endpoint)
  
  # Create request
  req <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_headers(
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_timeout(config$default_timeout %||% 30)
  
  # Add query parameters
  query_params <- build_pinterest_params(params, operation, config)
  if (length(query_params) > 0) {
    req <- httr2::req_url_query(req, !!!query_params)
  }
  
  return(req)
}

#' Build Pinterest-Specific Parameters
#' 
#' @description
#' Converts unified parameters to Pinterest's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. Pinterest-formatted parameters
#' @keywords internal
build_pinterest_params <- function(params, operation, config) {
  
  pinterest_params <- list()
  
  if (operation == "search_ads") {
    
    # Country filter
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      pinterest_params$country <- countries[1]  # Pinterest typically takes single country
    } else {
      pinterest_params$country <- "US"  # Default
    }
    
    # Keyword search
    if (!is.null(params$keyword)) {
      pinterest_params$search_terms <- sanitize_string(params$keyword)
    }
    
    # Advertiser filter
    if (!is.null(params$advertisers)) {
      pinterest_params$advertiser_name <- sanitize_string(params$advertisers[1])
    }
    
    # Date range
    if (!is.null(params$date_from)) {
      pinterest_params$start_date <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      pinterest_params$end_date <- format_date_for_api(params$date_to, "iso")
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      pinterest_params$page_size <- min(as.integer(params$limit), 100)
    } else {
      pinterest_params$page_size <- 100
    }
    
    if (!is.null(params$page_token)) {
      pinterest_params$bookmark <- params$page_token  # Pinterest uses bookmark pagination
    }
    
  } else if (operation == "advertisers") {
    
    # Search by advertiser name
    if (!is.null(params$keyword)) {
      pinterest_params$name <- sanitize_string(params$keyword)
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      pinterest_params$page_size <- min(as.integer(params$limit), 100)
    } else {
      pinterest_params$page_size <- 100
    }
    
    if (!is.null(params$page_token)) {
      pinterest_params$bookmark <- params$page_token
    }
  }
  
  return(pinterest_params)
}

#' Parse Pinterest Response
#' 
#' @description
#' Parses Pinterest API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_pinterest_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # Pinterest returns ads in items array
    ads <- content$items %||% content$data %||% list()
    bookmark <- content$bookmark
    
    result <- list(
      data = ads,
      pagination = list(bookmark = bookmark),
      total_count = length(ads)
    )
    
  } else if (operation == "advertisers") {
    # Pinterest returns advertisers
    advertisers <- content$items %||% content$data %||% list()
    bookmark <- content$bookmark
    
    result <- list(
      data = advertisers,
      pagination = list(bookmark = bookmark),
      total_count = length(advertisers)
    )
  }
  
  return(result)
}

#' Normalize Pinterest Data to Common Schema
#' 
#' @description
#' Converts Pinterest Ad Library data to adlibr normalized schema.
#' 
#' @param raw_data List. Raw Pinterest API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_pinterest_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$data) || length(raw_data$data) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$data
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "id") %||% safe_extract(ad, "pin_id")
    advertiser_id <- safe_extract(ad, "advertiser_id")
    advertiser_name <- safe_extract(ad, "advertiser_name")
    
    # Dates
    first_seen <- parse_api_date(safe_extract(ad, "created_time") %||% safe_extract(ad, "start_time"))
    last_seen <- parse_api_date(safe_extract(ad, "updated_time") %||% safe_extract(ad, "end_time"))
    
    # Ad content - Pinterest is primarily visual
    ad_text <- safe_extract(ad, "pin_description") %||% safe_extract(ad, "title")
    
    # Media - Pinterest is image-focused
    media_type <- "image"  # Pinterest is primarily image-based
    
    # Pin/media URLs
    pin_url <- safe_extract(ad, "pin_url") %||% safe_extract(ad, "image_url")
    media_urls <- if (!is.null(pin_url)) list(pin_url) else list(character(0))
    
    # Landing page
    landing_url <- safe_extract(ad, "destination_url") %||% safe_extract(ad, "link")
    
    # Geographic targeting
    geo_targeting <- safe_extract(ad, "targeting", list())
    countries <- list(safe_extract(geo_targeting, "countries", character(0)))
    
    # Pinterest-specific targeting
    targeting_interests <- list(safe_extract(geo_targeting, "interests", character(0)))
    targeting_age <- list(safe_extract(geo_targeting, "age_bucket", character(0)))
    targeting_gender <- list(safe_extract(geo_targeting, "genders", character(0)))
    targeting_locations <- list(safe_extract(geo_targeting, "geo", character(0)))
    
    # Keywords as behaviors
    keywords <- safe_extract(ad, "keywords", character(0))
    targeting_behaviors <- if (length(keywords) > 0) list(keywords) else list(character(0))
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "pinterest",
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
      media_urls = media_urls,
      landing_url = as.character(landing_url),
      countries = countries,
      languages = list(character(0)),
      spend_range = NA_character_,
      impressions_range = NA_character_,
      eu_reach = NA_character_,
      targeting_age = targeting_age,
      targeting_gender = targeting_gender,
      targeting_locations = targeting_locations,
      targeting_interests = targeting_interests,
      targeting_behaviors = targeting_behaviors,
      targeting_custom_audiences = list(character(0)),
      targeting_exclusions = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Validate Pinterest Credentials
#' 
#' @description
#' Validates Pinterest API credentials by making a test request.
#' 
#' @return List with validation results
#' @keywords internal
validate_pinterest_credentials <- function() {
  
  access_token <- get_credential("PINTEREST_TOKEN")
  
  if (is.null(access_token)) {
    return(list(
      valid = FALSE,
      message = "PINTEREST_TOKEN not found",
      error = "missing_token"
    ))
  }
  
  # Test with minimal request
  tryCatch({
    config <- get_platform_config("pinterest")
    
    req <- httr2::request(paste0(config$base, "/user_account")) |>
      httr2::req_headers(Authorization = paste("Bearer", access_token)) |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(
        valid = TRUE,
        message = "Pinterest credentials valid"
      ))
    } else {
      return(list(
        valid = FALSE,
        message = paste("API returned status", httr2::resp_status(response)),
        error = "api_error"
      ))
    }
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = paste("Connection failed:", e$message),
      error = "connection_error"
    ))
  })
}
