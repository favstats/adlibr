# TikTok Commercial Content Library API
#
# This file contains platform-specific functions for accessing
# TikTok's Commercial Content Library API, leveraging existing
# tiktokcomm implementation patterns.

#' Build TikTok-Specific Request
#' 
#' @description
#' Builds a request specifically for TikTok's Commercial Content Library API.
#' Integrates with existing tiktokcomm API patterns where applicable.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers", "details")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_tiktok_request <- function(operation, params, config) {
  
  # Get TikTok access token
  access_token <- get_credential("TIKTOK_TOKEN")
  if (is.null(access_token)) {
    cli::cli_abort("TIKTOK_TOKEN environment variable required for TikTok API access")
  }
  
  # Build endpoint URL based on operation
  endpoint <- switch(operation,
    "search_ads" = "ads/search",
    "advertisers" = "advertisers/search",
    "details" = "ads/details",
    cli::cli_abort("Unknown TikTok operation: {operation}")
  )
  
  base_url <- paste0(config$base, "/", endpoint)
  
  # Create request
  req <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_headers(
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json",
      Accept = "application/json"
    ) |>
    httr2::req_timeout(config$default_timeout %||% 30)
  
  # TikTok often uses POST requests with JSON body
  if (operation %in% c("search_ads", "advertisers")) {
    query_params <- build_tiktok_params(params, operation, config)
    if (length(query_params) > 0) {
      req <- httr2::req_body_json(req, query_params)
      req <- httr2::req_method(req, "POST")
    }
  } else {
    # For details, use GET with query params
    query_params <- build_tiktok_params(params, operation, config)
    if (length(query_params) > 0) {
      req <- httr2::req_url_query(req, !!!query_params)
    }
  }
  
  return(req)
}

#' Build TikTok-Specific Parameters
#' 
#' @description
#' Converts unified parameters to TikTok's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. TikTok-formatted parameters
#' @keywords internal
build_tiktok_params <- function(params, operation, config) {
  
  tiktok_params <- list()
  
  if (operation == "search_ads") {
    
    # Required: country_code
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      tiktok_params$country_code <- countries[1]  # TikTok takes single country
    } else {
      tiktok_params$country_code <- "US"  # Default
    }
    
    # Optional: keyword search
    if (!is.null(params$keyword)) {
      tiktok_params$search_term <- sanitize_string(params$keyword)
    }
    
    # Optional: advertiser ID filter
    if (!is.null(params$advertisers)) {
      # If advertiser looks like an ID, use it directly, otherwise search
      if (grepl("^[0-9]+$", params$advertisers[1])) {
        tiktok_params$advertiser_id <- as.numeric(params$advertisers[1])
      } else {
        tiktok_params$advertiser_name <- sanitize_string(params$advertisers[1])
      }
    }
    
    # Date range
    if (!is.null(params$date_from)) {
      tiktok_params$start_date <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      tiktok_params$end_date <- format_date_for_api(params$date_to, "iso")
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      tiktok_params$page_size <- min(as.integer(params$limit), 100)  # TikTok max
    } else {
      tiktok_params$page_size <- 100
    }
    
    if (!is.null(params$page_token)) {
      tiktok_params$page_token <- params$page_token
    }
    
    # Filters
    tiktok_params$filters <- list()
    
    # Ad type filter
    if (!is.null(params$ad_type)) {
      tiktok_params$filters$ad_type <- params$ad_type
    }
    
    # Industry filter
    if (!is.null(params$industry)) {
      tiktok_params$filters$industry <- params$industry
    }
    
  } else if (operation == "advertisers") {
    
    # Country
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      tiktok_params$country_code <- countries[1]
    } else {
      tiktok_params$country_code <- "US"
    }
    
    # Search by advertiser name
    if (!is.null(params$keyword)) {
      tiktok_params$advertiser_name <- sanitize_string(params$keyword)
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      tiktok_params$page_size <- min(as.integer(params$limit), 100)
    } else {
      tiktok_params$page_size <- 100
    }
    
    if (!is.null(params$page_token)) {
      tiktok_params$page_token <- params$page_token
    }
    
  } else if (operation == "details") {
    
    # Ad ID for details
    if (!is.null(params$ad_id)) {
      tiktok_params$ad_id <- params$ad_id
    }
    
    # Country context
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      tiktok_params$country_code <- countries[1]
    } else {
      tiktok_params$country_code <- "US"
    }
  }
  
  return(tiktok_params)
}

#' Parse TikTok Response
#' 
#' @description
#' Parses TikTok API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_tiktok_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  # Check for API errors
  if (!is.null(content$error)) {
    cli::cli_abort("TikTok API error: {content$error$message}")
  }
  
  if (operation == "search_ads") {
    # TikTok returns ads in data array
    ads <- content$data$ads %||% content$data %||% list()
    
    result <- list(
      data = ads,
      pagination = content$data$pagination %||% content$pagination,
      total_count = content$data$total_count %||% length(ads)
    )
    
  } else if (operation == "advertisers") {
    # TikTok returns advertisers
    advertisers <- content$data$advertisers %||% content$data %||% list()
    
    result <- list(
      data = advertisers,
      pagination = content$data$pagination %||% content$pagination,
      total_count = content$data$total_count %||% length(advertisers)
    )
    
  } else if (operation == "details") {
    # Single ad details
    ad_details <- content$data %||% list()
    
    result <- list(
      data = if (length(ad_details) > 0) list(ad_details) else list(),
      total_count = if (length(ad_details) > 0) 1 else 0
    )
  }
  
  return(result)
}

#' Normalize TikTok Data to Common Schema
#' 
#' @description
#' Converts TikTok Commercial Content Library data to adlibr normalized schema.
#' 
#' @param raw_data List. Raw TikTok API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_tiktok_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$data) || length(raw_data$data) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$data
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "ad_id") %||% safe_extract(ad, "id")
    advertiser_id <- safe_extract(ad, "advertiser_id")
    advertiser_name <- safe_extract(ad, "advertiser_name") %||% safe_extract(ad, "brand_name")
    
    # Advertiser verification (TikTok has verification badges)
    advertiser_verified <- safe_extract(ad, "advertiser_verified", FALSE)
    
    # Dates
    first_seen <- parse_api_date(safe_extract(ad, "first_shown_date") %||% safe_extract(ad, "start_date"))
    last_seen <- parse_api_date(safe_extract(ad, "last_shown_date") %||% safe_extract(ad, "end_date"))
    
    # Ad content
    ad_text <- safe_extract(ad, "ad_text") %||% safe_extract(ad, "caption")
    
    # Media information - TikTok is primarily video
    media_type <- "video"  # TikTok ads are predominantly video
    media_urls <- list(safe_extract(ad, "video_url", character(0)))
    
    # Landing page
    landing_url <- safe_extract(ad, "landing_url") %||% safe_extract(ad, "click_url")
    
    # Geographic data
    countries_data <- safe_extract(ad, "reach_countries") %||% safe_extract(ad, "countries")
    countries <- if (!is.null(countries_data)) {
      list(as.character(countries_data))
    } else {
      list(character(0))
    }
    
    # Targeting information
    targeting_data <- safe_extract(ad, "targeting", list())
    
    targeting_age <- list(safe_extract(targeting_data, "age_groups", character(0)))
    targeting_gender <- list(safe_extract(targeting_data, "gender", character(0)))
    targeting_locations <- list(safe_extract(targeting_data, "locations", character(0)))
    targeting_interests <- list(safe_extract(targeting_data, "interests", character(0)))
    targeting_behaviors <- list(safe_extract(targeting_data, "behaviors", character(0)))
    
    # Industry/category as interest
    industry <- safe_extract(ad, "industry") %||% safe_extract(ad, "category")
    if (!is.null(industry)) {
      targeting_interests <- list(c(targeting_interests[[1]], industry))
    }
    
    # TikTok specific fields
    ad_format <- safe_extract(ad, "ad_format")
    objective <- safe_extract(ad, "objective")
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "tiktok",
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      advertiser_verified = as.logical(advertiser_verified),
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

#' Search TikTok Advertisers
#' 
#' @description
#' Search for advertisers on TikTok platform.
#' 
#' @param search_term Character. Search term for advertiser names
#' @param limit Integer. Maximum number of results
#' 
#' @return tibble. Advertiser information
#' @keywords internal
search_tiktok_advertisers <- function(search_term, limit = 100) {
  
  config <- get_platform_config("tiktok")
  
  params <- list(
    keyword = search_term,
    limit = limit
  )
  
  req <- build_tiktok_request("advertisers", params, config)
  response <- httr2::req_perform(req)
  handle_response_errors(response, "tiktok", "advertisers")
  
  parsed <- parse_tiktok_response(response, "advertisers")
  
  # Convert to tibble
  if (length(parsed$data) > 0) {
    advertisers_df <- purrr::map_df(parsed$data, function(advertiser) {
      tibble::tibble(
        advertiser_id = safe_extract(advertiser, "advertiser_id"),
        advertiser_name = safe_extract(advertiser, "advertiser_name"),
        verified = safe_extract(advertiser, "verified", FALSE),
        industry = safe_extract(advertiser, "industry"),
        platform = "tiktok"
      )
    })
    return(advertisers_df)
  } else {
    return(tibble::tibble(
      advertiser_id = character(0),
      advertiser_name = character(0),
      verified = logical(0),
      industry = character(0),
      platform = character(0)
    ))
  }
}

#' Validate TikTok Credentials
#' 
#' @description
#' Validates TikTok API credentials by making a test request.
#' 
#' @return List with validation results
#' @keywords internal
validate_tiktok_credentials <- function() {
  
  access_token <- get_credential("TIKTOK_TOKEN")
  
  if (is.null(access_token)) {
    return(list(
      valid = FALSE,
      message = "TIKTOK_TOKEN not found",
      error = "missing_token"
    ))
  }
  
  # Test with minimal request
  tryCatch({
    config <- get_platform_config("tiktok")
    
    # Try a simple advertisers search
    test_params <- list(
      country_code = "US",
      page_size = 1
    )
    
    req <- httr2::request(paste0(config$base, "/advertisers/search")) |>
      httr2::req_headers(
        Authorization = paste("Bearer", access_token),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(test_params) |>
      httr2::req_method("POST") |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      content <- httr2::resp_body_json(response)
      return(list(
        valid = TRUE,
        message = "TikTok credentials valid",
        test_results = length(content$data %||% list())
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