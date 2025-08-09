# Snapchat Political and Advocacy Ads API
#
# This file contains platform-specific functions for accessing
# Snapchat's Political and Advocacy Ads API.

#' Build Snapchat-Specific Request
#' 
#' @description
#' Builds a request specifically for Snapchat's Political Ads API.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_snapchat_request <- function(operation, params, config) {
  
  if (operation == "search_ads") {
    # POST endpoint for ads search
    base_url <- "https://adsapi.snapchat.com/v1/ads_library/ads/search"
    
    # Build JSON body according to Snapchat API docs
    body <- list()
    
    # Map adlibr parameters to Snapchat parameters
    if (!is.null(params$advertisers)) {
      body$paying_advertiser_name <- params$advertisers
    }
    
    if (!is.null(params$countries)) {
      body$countries <- params$countries
    }
    
    if (!is.null(params$date_from)) {
      body$start_date <- paste0(params$date_from, "T00:00:00.000Z")
    }
    
    if (!is.null(params$date_to)) {
      body$end_date <- paste0(params$date_to, "T23:59:59.000Z")
    }
    
    # Snapchat status filter (default to ACTIVE)
    body$status <- "ACTIVE"
    
    # Create POST request
    req <- httr2::request(base_url) |>
      httr2::req_headers(
        Accept = "application/json",
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_method("POST") |>
      httr2::req_timeout(30)
    
    return(req)
    
  } else if (operation == "details") {
    # GET endpoint for ad details
    ad_id <- params$ad_id
    if (is.null(ad_id)) {
      cli::cli_abort("ad_id required for Snapchat details operation")
    }
    
    base_url <- paste0("https://adsapi.snapchat.com/v1/ads_library/ads/", ad_id)
    
    req <- httr2::request(base_url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(30)
    
    return(req)
    
  } else {
    cli::cli_abort("Unknown Snapchat operation: {operation}")
  }
}

#' Build Snapchat-Specific Parameters
#' 
#' @description
#' Converts unified parameters to Snapchat's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. Snapchat-formatted parameters
#' @keywords internal
build_snapchat_params <- function(params, operation, config) {
  
  snapchat_params <- list()
  
  if (operation == "search_ads") {
    
    # Country filter (US, CA, UK typically supported)
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      # Snapchat political ads primarily in US, CA, UK
      supported_countries <- c("US", "CA", "GB", "UK")
      valid_countries <- intersect(countries, supported_countries)
      
      if (length(valid_countries) > 0) {
        snapchat_params$CountryCode <- valid_countries[1]
      } else {
        cli::cli_warn("Snapchat political ads primarily available in US, CA, UK")
        snapchat_params$CountryCode <- "US"
      }
    } else {
      snapchat_params$CountryCode <- "US"
    }
    
    # Advertiser search
    if (!is.null(params$advertisers)) {
      snapchat_params$AdvertiserName <- sanitize_string(params$advertisers[1])
    }
    
    # Organization search (Snapchat uses this term)
    if (!is.null(params$keyword)) {
      snapchat_params$OrganizationName <- sanitize_string(params$keyword)
    }
    
    # Date range
    if (!is.null(params$date_from)) {
      snapchat_params$StartDate <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      snapchat_params$EndDate <- format_date_for_api(params$date_to, "iso")
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      snapchat_params$limit <- min(as.integer(params$limit), 100)
    } else {
      snapchat_params$limit <- 100
    }
    
    if (!is.null(params$page_token)) {
      snapchat_params$offset <- as.integer(params$page_token)
    } else {
      snapchat_params$offset <- 0
    }
    
  } else if (operation == "advertisers") {
    
    # Search by organization name
    if (!is.null(params$keyword)) {
      snapchat_params$OrganizationName <- sanitize_string(params$keyword)
    }
    
    # Country
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      snapchat_params$CountryCode <- countries[1]
    } else {
      snapchat_params$CountryCode <- "US"
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      snapchat_params$limit <- min(as.integer(params$limit), 100)
    } else {
      snapchat_params$limit <- 100
    }
    
    if (!is.null(params$page_token)) {
      snapchat_params$offset <- as.integer(params$page_token)
    } else {
      snapchat_params$offset <- 0
    }
  }
  
  return(snapchat_params)
}

#' Parse Snapchat Response
#' 
#' @description
#' Parses Snapchat API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_snapchat_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # According to Snapchat API docs, search returns ad_previews array
    ad_previews <- content$ad_previews %||% list()
    
    # Extract the actual ad data from each preview
    ads <- purrr::map(ad_previews, function(preview) {
      if (preview$sub_request_status == "SUCCESS") {
        return(preview$ad_preview)
      }
      return(NULL)
    })
    
    # Remove NULL entries
    ads <- purrr::compact(ads)
    
    # Extract pagination info
    pagination <- list()
    if (!is.null(content$paging$next_link)) {
      # Extract cursor from next_link
      next_url <- content$paging$next_link
      cursor_match <- stringr::str_extract(next_url, "cursor=([^&]+)")
      if (!is.na(cursor_match)) {
        pagination$next_cursor <- stringr::str_replace(cursor_match, "cursor=", "")
      }
    }
    
    result <- list(
      elements = ads,
      paging = pagination,
      total_count = length(ads)
    )
    
  } else if (operation == "details") {
    # Details returns single ad_preview
    ad_data <- content$ad_preview %||% list()
    
    result <- list(
      elements = list(ad_data),
      total_count = 1
    )
    
  } else {
    result <- list(
      elements = list(),
      total_count = 0
    )
  }
  
  return(result)
}

#' Normalize Snapchat Data to Common Schema
#' 
#' @description
#' Converts Snapchat Political Ads data to adlibr normalized schema.
#' 
#' @param raw_data List. Raw Snapchat API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_snapchat_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$elements) || length(raw_data$elements) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$elements
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "AdID") %||% safe_extract(ad, "Id")
    advertiser_name <- safe_extract(ad, "OrganizationName") %||% safe_extract(ad, "AdvertiserName")
    
    # Snapchat specific fields
    candidate_ballot_info <- safe_extract(ad, "CandidateBallotInformation")
    
    # Dates
    first_seen <- parse_api_date(safe_extract(ad, "StartDate") %||% safe_extract(ad, "CreatedDate"))
    last_seen <- parse_api_date(safe_extract(ad, "EndDate") %||% safe_extract(ad, "LastServedDate"))
    
    # Spend information
    spend_range <- safe_extract(ad, "Spend") %||% safe_extract(ad, "SpendUSD")
    
    # Impressions
    impressions_range <- safe_extract(ad, "Impressions")
    
    # Geographic targeting
    countries <- list(safe_extract(ad, "CountryCode", character(0)))
    
    # Targeting information (limited in Snapchat political ads)
    targeting_age <- list(safe_extract(ad, "AgeTargeting", character(0)))
    targeting_gender <- list(safe_extract(ad, "GenderTargeting", character(0)))
    targeting_locations <- list(safe_extract(ad, "GeoTargeting", character(0)))
    
    # Political ad specific info
    advertiser_type <- if (!is.null(candidate_ballot_info)) {
      "political"
    } else {
      "advocacy"
    }
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "snapchat",
      advertiser_id = NA_character_,
      advertiser_name = as.character(advertiser_name),
      advertiser_verified = NA,
      advertiser_type = as.character(advertiser_type),
      payer = NA_character_,
      beneficiary = as.character(safe_extract(candidate_ballot_info, "CandidateName")),
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = NA_character_,  # Snapchat doesn't expose ad creative text
      ad_creative_urls = list(character(0)),
      ad_library_url = NA_character_,
      media_type = "video",  # Snapchat is primarily video
      media_urls = list(character(0)),
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_range = as.character(spend_range),
      impressions_range = as.character(impressions_range),
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

#' Validate Snapchat Credentials
#' 
#' @description
#' Validates Snapchat API access (public API, so mainly tests connectivity).
#' 
#' @return List with validation results
#' @keywords internal
validate_snapchat_credentials <- function() {
  
  # Test basic connectivity
  tryCatch({
    config <- get_platform_config("snapchat")
    
    req <- httr2::request(config$base) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(
        valid = TRUE,
        message = "Snapchat Political Ads API accessible",
        authenticated = FALSE  # Public API
      ))
    } else {
      return(list(
        valid = FALSE,
        message = paste("Snapchat API returned status", httr2::resp_status(response)),
        error = "api_error"
      ))
    }
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = paste("Snapchat API connection failed:", e$message),
      error = "connection_error"
    ))
  })
}
