# X (formerly Twitter) Ads Transparency API
#
# This file contains platform-specific functions for accessing
# X's Ads Transparency Center API.

#' Build X-Specific Request
#' 
#' @description
#' Builds a request specifically for X's Ads Transparency API.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_x_request <- function(operation, params, config) {
  
  # X Ads Transparency may require API key or be public
  api_key <- get_credential("X_API_KEY")
  
  base_url <- config$base
  
  # Create request
  req <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_headers(
      Accept = "application/json",
      `Content-Type` = "application/json"
    ) |>
    httr2::req_timeout(config$default_timeout %||% 30)
  
  # Add authentication if available
  if (!is.null(api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
  }
  
  # Add query parameters
  query_params <- build_x_params(params, operation, config)
  if (length(query_params) > 0) {
    req <- httr2::req_url_query(req, !!!query_params)
  }
  
  return(req)
}

#' Build X-Specific Parameters
#' 
#' @description
#' Converts unified parameters to X's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. X-formatted parameters
#' @keywords internal
build_x_params <- function(params, operation, config) {
  
  x_params <- list()
  
  if (operation == "search_ads") {
    
    # Country filter
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      x_params$country <- countries[1]  # X typically takes single country
    } else {
      x_params$country <- "US"  # Default
    }
    
    # Advertiser search
    if (!is.null(params$advertisers)) {
      x_params$advertiser_name <- sanitize_string(params$advertisers[1])
    }
    
    # Keyword search in ad content
    if (!is.null(params$keyword)) {
      x_params$search_term <- sanitize_string(params$keyword)
    }
    
    # Date range
    if (!is.null(params$date_from)) {
      x_params$start_date <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      x_params$end_date <- format_date_for_api(params$date_to, "iso")
    }
    
    # Ad type filter (promoted tweets, etc.)
    if (!is.null(params$ad_type)) {
      x_params$ad_type <- params$ad_type
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      x_params$count <- min(as.integer(params$limit), 100)
    } else {
      x_params$count <- 100
    }
    
    if (!is.null(params$page_token)) {
      x_params$next_token <- params$page_token  # X uses next_token pagination
    }
    
  } else if (operation == "advertisers") {
    
    # Search by advertiser name
    if (!is.null(params$keyword)) {
      x_params$name <- sanitize_string(params$keyword)
    }
    
    # Country filter
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      x_params$country <- countries[1]
    } else {
      x_params$country <- "US"
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      x_params$count <- min(as.integer(params$limit), 100)
    } else {
      x_params$count <- 100
    }
    
    if (!is.null(params$page_token)) {
      x_params$next_token <- params$page_token
    }
  }
  
  return(x_params)
}

#' Parse X Response
#' 
#' @description
#' Parses X API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_x_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # X returns ads in data array
    ads <- content$data %||% list()
    meta <- content$meta %||% list()
    
    result <- list(
      data = ads,
      pagination = list(next_token = meta$next_token),
      total_count = meta$result_count %||% length(ads)
    )
    
  } else if (operation == "advertisers") {
    # X returns advertisers
    advertisers <- content$data %||% list()
    meta <- content$meta %||% list()
    
    result <- list(
      data = advertisers,
      pagination = list(next_token = meta$next_token),
      total_count = meta$result_count %||% length(advertisers)
    )
  }
  
  return(result)
}

#' Normalize X Data to Common Schema
#' 
#' @description
#' Converts X Ads Transparency data to adlibr normalized schema.
#' 
#' @param raw_data List. Raw X API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_x_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$data) || length(raw_data$data) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$data
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "id") %||% safe_extract(ad, "tweet_id")
    advertiser_id <- safe_extract(ad, "advertiser_id")
    advertiser_name <- safe_extract(ad, "advertiser_name") %||% safe_extract(ad, "account_name")
    
    # X verification
    advertiser_verified <- safe_extract(ad, "verified", FALSE)
    
    # Dates
    first_seen <- parse_api_date(safe_extract(ad, "created_at") %||% safe_extract(ad, "start_time"))
    last_seen <- parse_api_date(safe_extract(ad, "updated_at") %||% safe_extract(ad, "end_time"))
    
    # Tweet/ad content
    ad_text <- safe_extract(ad, "text") %||% safe_extract(ad, "tweet_text")
    
    # Media information
    media_data <- safe_extract(ad, "media", list())
    
    # Determine media type
    media_type <- if (length(media_data) > 0) {
      media_types <- purrr::map_chr(media_data, ~ safe_extract(.x, "type", "unknown"))
      if ("video" %in% media_types) "video" else if ("photo" %in% media_types) "image" else "text"
    } else {
      "text"
    }
    
    # Extract media URLs
    media_urls <- if (length(media_data) > 0) {
      urls <- purrr::map_chr(media_data, ~ safe_extract(.x, "url", ""))
      list(urls[urls != ""])
    } else {
      list(character(0))
    }
    
    # Landing URL from expanded URLs
    urls_data <- safe_extract(ad, "entities", list())$urls
    landing_url <- if (length(urls_data) > 0) {
      safe_extract(urls_data[[1]], "expanded_url")
    } else {
      NA_character_
    }
    
    # Geographic data
    geo_targeting <- safe_extract(ad, "geo_targeting", list())
    countries <- list(safe_extract(geo_targeting, "countries", character(0)))
    
    # Targeting information (limited in X transparency)
    targeting_age <- list(safe_extract(ad, "age_targeting", character(0)))
    targeting_gender <- list(safe_extract(ad, "gender_targeting", character(0)))
    targeting_locations <- list(safe_extract(geo_targeting, "locations", character(0)))
    
    # Keywords/hashtags as interests
    hashtags <- safe_extract(ad, "entities", list())$hashtags
    targeting_interests <- if (length(hashtags) > 0) {
      hashtag_texts <- purrr::map_chr(hashtags, ~ safe_extract(.x, "text", ""))
      list(hashtag_texts[hashtag_texts != ""])
    } else {
      list(character(0))
    }
    
    # Spend information (if available)
    spend_range <- safe_extract(ad, "spend") %||% safe_extract(ad, "promotion_spend")
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "x",
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
      spend_range = as.character(spend_range),
      impressions_range = NA_character_,
      eu_reach = NA_character_,
      targeting_age = targeting_age,
      targeting_gender = targeting_gender,
      targeting_locations = targeting_locations,
      targeting_interests = targeting_interests,
      targeting_behaviors = list(character(0)),
      targeting_custom_audiences = list(character(0)),
      targeting_exclusions = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Validate X Credentials
#' 
#' @description
#' Validates X API credentials if available, or tests public access.
#' 
#' @return List with validation results
#' @keywords internal
validate_x_credentials <- function() {
  
  api_key <- get_credential("X_API_KEY")
  
  # Test basic connectivity
  tryCatch({
    config <- get_platform_config("x")
    
    req <- httr2::request(config$base) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(10)
    
    # Add auth if available
    if (!is.null(api_key)) {
      req <- httr2::req_headers(req, Authorization = paste("Bearer", api_key))
    }
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(
        valid = TRUE,
        message = "X Ads Transparency API accessible",
        authenticated = !is.null(api_key)
      ))
    } else {
      return(list(
        valid = FALSE,
        message = paste("X API returned status", httr2::resp_status(response)),
        error = "api_error"
      ))
    }
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = paste("X API connection failed:", e$message),
      error = "connection_error"
    ))
  })
}
