# Microsoft (Bing) Ad Library API
#
# This file contains platform-specific functions for accessing
# Microsoft's Bing Ad Library API.

#' Build Microsoft-Specific Request
#' 
#' @description
#' Builds a request specifically for Microsoft's Bing Ad Library API.
#' 
#' @param operation Character. Operation type ("search_ads", "details", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_microsoft_request <- function(operation, params, config) {
  
  if (operation == "search_ads") {
    # Microsoft Bing Ad Library API - confirmed working
    base_url <- "https://adlibrary.api.bingads.microsoft.com/api/v1/Ads"
    
    # Build query parameters for Microsoft API
    query_params <- list()
    
    # Map adlibr parameters to Microsoft parameters
    if (!is.null(params$q)) {
      query_params$searchText <- params$q
    }
    
    if (!is.null(params$advertisers)) {
      query_params$advertiserId <- params$advertisers
    }
    
    if (!is.null(params$countries)) {
      # Microsoft expects country codes as comma-separated list
      query_params$countryCodes <- paste(params$countries, collapse = ",")
    }
    
    if (!is.null(params$date_from)) {
      query_params$startDate <- params$date_from
    }
    
    if (!is.null(params$date_to)) {
      query_params$endDate <- params$date_to
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      query_params$`$top` <- min(as.integer(params$limit), 1000)  # Microsoft limit
    } else {
      query_params$`$top` <- 100  # Default
    }
    
    if (!is.null(params$page_token)) {
      query_params$`$skip` <- as.integer(params$page_token)
    }
    
    # Always include count for pagination
    query_params$`$count` <- "true"
    
    # Optional authentication (for higher limits)
    dev_token <- get_credential("MS_DEV_TOKEN")
    auth_token <- get_credential("MS_AUTH_TOKEN")
    
    headers <- list(Accept = "application/json")
    if (!is.null(dev_token)) {
      headers$DeveloperToken <- dev_token
    }
    if (!is.null(auth_token)) {
      headers$AuthenticationToken <- auth_token
    }
    
    # Create GET request
    req <- httr2::request(base_url) |>
      httr2::req_headers(!!!headers) |>
      httr2::req_url_query(!!!query_params) |>
      httr2::req_timeout(30)
    
    return(req)
    
  } else if (operation == "details") {
    # Details endpoint
    ad_id <- params$ad_id
    if (is.null(ad_id)) {
      cli::cli_abort("ad_id required for Microsoft details operation")
    }
    
    base_url <- paste0("https://adlibrary.api.bingads.microsoft.com/api/v1/Ads/", ad_id)
    
    req <- httr2::request(base_url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_timeout(30)
    
    return(req)
    
  } else {
    cli::cli_abort("Unknown Microsoft operation: {operation}")
  }
}

#' Build Microsoft-Specific Parameters
#' 
#' @description
#' Converts unified parameters to Microsoft's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. Microsoft-formatted parameters
#' @keywords internal
build_microsoft_params <- function(params, operation, config) {
  
  ms_params <- list()
  
  # Common parameters
  if (operation %in% c("search_ads", "advertisers")) {
    
    # Optional: search text
    if (!is.null(params$keyword)) {
      ms_params$searchText <- sanitize_string(params$keyword)
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      ms_params$top <- min(as.integer(params$limit), 100)  # Microsoft max per page
    } else {
      ms_params$top <- 100
    }
    
    if (!is.null(params$page_token)) {
      # Microsoft uses skip for pagination
      ms_params$skip <- as.integer(params$page_token)
    } else {
      ms_params$skip <- 0
    }
  }
  
  # Ad search specific parameters
  if (operation == "search_ads") {
    
    # Optional: advertiser ID filter
    if (!is.null(params$advertisers)) {
      # Take first advertiser if multiple provided
      ms_params$advertiserId <- params$advertisers[1]
    }
    
    # Optional: date range
    if (!is.null(params$date_from)) {
      ms_params$startDate <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      ms_params$endDate <- format_date_for_api(params$date_to, "iso")
    }
    
    # Optional: countries (Microsoft uses numeric codes)
    if (!is.null(params$countries)) {
      country_codes <- convert_countries_to_microsoft_codes(params$countries)
      if (length(country_codes) > 0) {
        ms_params$countryCodes <- paste(country_codes, collapse = ",")
      }
    }
  }
  
  return(ms_params)
}

#' Convert Country Codes to Microsoft Format
#' 
#' @description
#' Converts ISO country codes to Microsoft's numeric format.
#' Note: This is a simplified mapping. In production, you'd want
#' a complete mapping table.
#' 
#' @param countries Character vector. ISO country codes
#' 
#' @return Character vector. Microsoft numeric country codes
#' @keywords internal
convert_countries_to_microsoft_codes <- function(countries) {
  
  # Simplified mapping (EU/EEA focus for Microsoft)
  country_mapping <- list(
    "US" = "84",    # United States  
    "GB" = "77",    # United Kingdom
    "DE" = "31",    # Germany
    "FR" = "30",    # France
    "IT" = "40",    # Italy
    "ES" = "65",    # Spain
    "NL" = "54",    # Netherlands
    "BE" = "14",    # Belgium
    "AT" = "11",    # Austria
    "SE" = "67",    # Sweden
    "DK" = "25",    # Denmark
    "NO" = "55",    # Norway
    "FI" = "29",    # Finland
    "PL" = "58",    # Poland
    "CZ" = "24",    # Czech Republic
    "HU" = "38",    # Hungary
    "RO" = "60",    # Romania
    "BG" = "17",    # Bulgaria
    "HR" = "37",    # Croatia
    "SI" = "64",    # Slovenia
    "SK" = "63",    # Slovakia
    "LT" = "45",    # Lithuania
    "LV" = "44",    # Latvia
    "EE" = "27",    # Estonia
    "IE" = "39",    # Ireland
    "PT" = "59",    # Portugal
    "GR" = "36",    # Greece
    "CY" = "23",    # Cyprus
    "MT" = "51",    # Malta
    "LU" = "46"     # Luxembourg
  )
  
  countries <- normalize_country_codes(countries)
  
  # Convert to Microsoft codes
  ms_codes <- character(0)
  for (country in countries) {
    if (country %in% names(country_mapping)) {
      ms_codes <- c(ms_codes, country_mapping[[country]])
    } else {
      cli::cli_warn("Country code {country} not supported by Microsoft Ad Library")
    }
  }
  
  return(ms_codes)
}

#' Parse Microsoft Response
#' 
#' @description
#' Parses Microsoft API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_microsoft_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # Microsoft OData format: {"@odata.context": "...", "@odata.count": 1000, "value": [...]}
    ads <- content$value %||% list()
    
    # Extract pagination info from OData
    pagination <- list()
    total_count <- content$`@odata.count` %||% length(ads)
    
    # Calculate next page offset if we have more results
    if (!is.null(content$`@odata.nextLink`)) {
      # Extract skip parameter from next link
      next_link <- content$`@odata.nextLink`
      skip_match <- stringr::str_extract(next_link, "\\$skip=([0-9]+)")
      if (!is.na(skip_match)) {
        pagination$next_skip <- as.integer(stringr::str_replace(skip_match, "\\$skip=", ""))
      }
    }
    
    result <- list(
      elements = ads,
      paging = pagination,
      total_count = total_count
    )
    
  } else if (operation == "details") {
    # Details returns single ad
    result <- list(
      elements = list(content),
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

#' Normalize Microsoft Data to Common Schema
#' 
#' @description
#' Enhanced Microsoft data normalization with improved field mapping.
#' 
#' @param raw_data List. Raw Microsoft API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_microsoft_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$elements) || length(raw_data$elements) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$elements
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers - from actual API response structure
    ad_id <- as.character(safe_extract(ad, "AdId"))
    advertiser_id <- as.character(safe_extract(ad, "AdvertiserId"))
    advertiser_name <- safe_extract(ad, "AdvertiserName")
    
    # Ad content - from actual API response
    ad_text <- paste(
      safe_extract(ad, "Title", ""),
      safe_extract(ad, "Description", ""),
      sep = " "
    ) %>% trimws()
    
    # URLs
    display_url <- safe_extract(ad, "DisplayUrl")
    landing_url <- safe_extract(ad, "DestinationUrl")
    
    # Media handling - Microsoft includes AssetJson
    asset_json <- safe_extract(ad, "AssetJson", "")
    media_urls <- list(character(0))
    media_type <- "unknown"
    
    if (!is.null(asset_json) && asset_json != "") {
      # Try to parse AssetJson for media URLs
      tryCatch({
        assets <- jsonlite::fromJSON(asset_json)
        if (length(assets) > 0) {
          asset_urls <- purrr::map_chr(assets, ~ safe_extract(.x, "AssetUrl", ""))
          media_urls <- list(asset_urls[asset_urls != ""])
          
          # Determine media type from assets
          asset_types <- purrr::map_chr(assets, ~ safe_extract(.x, "AssetType", ""))
          if (any(grepl("Image", asset_types, ignore.case = TRUE))) {
            media_type <- "image"
          } else if (any(grepl("Video", asset_types, ignore.case = TRUE))) {
            media_type <- "video"
          }
        }
      }, error = function(e) {
        # AssetJson parsing failed, keep defaults
      })
    }
    
    # Placeholder values for data not available in Microsoft API
    first_seen <- NA
    last_seen <- NA
    countries <- list(character(0))
    impressions_low <- NA_integer_
    impressions_high <- NA_integer_
    
    # Targeting information
    target_types <- safe_extract(ad, "TargetTypes", list())
    targeting_other <- if (length(target_types) > 0) {
      list(as.character(target_types))
    } else {
      list(character(0))
    }
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "microsoft",
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      advertiser_url = as.character(display_url),
      advertiser_verified = NA,
      advertiser_type = NA_character_,
      payer = NA_character_,  # Not available in Microsoft API
      beneficiary = NA_character_,
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
      ad_creative_urls = media_urls,
      ad_library_url = NA_character_,  # Microsoft doesn't provide direct ad library URLs
      media_type = as.character(media_type),
      media_urls = media_urls,
      landing_url = as.character(landing_url),
      countries = countries,
      languages = list(character(0)),
      spend_low = NA_integer_,
      spend_high = NA_integer_,
      spend_range = NA_character_,
      impressions_low = impressions_low,
      impressions_high = impressions_high,
      impressions_range = NA_character_,
      eu_reach = NA_character_,
      targeting_age = list(character(0)),
      targeting_gender = list(character(0)),
      targeting_locations = list(character(0)),
      targeting_interests = list(character(0)),
      targeting_behaviors = list(character(0)),
      targeting_custom_audiences = list(character(0)),
      targeting_exclusions = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Convert Microsoft Code to ISO Country Code
#' 
#' @description
#' Converts Microsoft's numeric country codes back to ISO codes.
#' 
#' @param ms_code Character. Microsoft country code
#' 
#' @return Character. ISO country code or original if not found
#' @keywords internal
convert_microsoft_code_to_iso <- function(ms_code) {
  
  # Reverse mapping from Microsoft codes to ISO
  reverse_mapping <- list(
    "84" = "US", "77" = "GB", "31" = "DE", "30" = "FR", "40" = "IT",
    "65" = "ES", "54" = "NL", "14" = "BE", "11" = "AT", "67" = "SE",
    "25" = "DK", "55" = "NO", "29" = "FI", "58" = "PL", "24" = "CZ",
    "38" = "HU", "60" = "RO", "17" = "BG", "37" = "HR", "64" = "SI",
    "63" = "SK", "45" = "LT", "44" = "LV", "27" = "EE", "39" = "IE",
    "59" = "PT", "36" = "GR", "23" = "CY", "51" = "MT", "46" = "LU"
  )
  
  if (ms_code %in% names(reverse_mapping)) {
    return(reverse_mapping[[ms_code]])
  } else {
    return(as.character(ms_code))  # Return as-is if not found
  }
}

#' Search Microsoft Advertisers
#' 
#' @description
#' Search for advertisers on Microsoft platform.
#' 
#' @param search_term Character. Search term for advertiser names
#' @param limit Integer. Maximum number of results
#' 
#' @return tibble. Advertiser information
#' @keywords internal
search_microsoft_advertisers <- function(search_term, limit = 100) {
  
  config <- get_platform_config("microsoft")
  
  params <- list(
    keyword = search_term,
    limit = limit
  )
  
  req <- build_microsoft_request("advertisers", params, config)
  response <- httr2::req_perform(req)
  handle_response_errors(response, "microsoft", "advertisers")
  
  parsed <- parse_microsoft_response(response, "advertisers")
  
  # Convert to tibble
  if (length(parsed$value) > 0) {
    advertisers_df <- purrr::map_df(parsed$value, function(adv) {
      tibble::tibble(
        advertiser_id = safe_extract(adv, "AdvertiserId"),
        advertiser_name = safe_extract(adv, "AdvertiserName"),
        verified_advertiser_id = safe_extract(adv, "VerifiedAdvertiserId"),
        platform = "microsoft"
      )
    })
    return(advertisers_df)
  } else {
    return(tibble::tibble(
      advertiser_id = character(0),
      advertiser_name = character(0),
      verified_advertiser_id = character(0),
      platform = character(0)
    ))
  }
}

#' Validate Microsoft Credentials
#' 
#' @description
#' Validates Microsoft API credentials by making a test request.
#' Since Microsoft API works without authentication, this mainly
#' tests connectivity and optional token validity.
#' 
#' @return List with validation results
#' @keywords internal
validate_microsoft_credentials <- function() {
  
  # Test basic connectivity (no auth required)
  tryCatch({
    req <- httr2::request("https://adlibrary.api.bingads.microsoft.com/api/v1/Ads") |>
      httr2::req_url_query(top = 1) |>
      httr2::req_timeout(10)
    
    # Add optional auth headers if available
    dev_token <- get_credential("MS_DEV_TOKEN")
    auth_token <- get_credential("MS_AUTH_TOKEN")
    
    headers <- list()
    if (!is.null(dev_token)) headers$DeveloperToken <- dev_token
    if (!is.null(auth_token)) headers$AuthenticationToken <- auth_token
    
    if (length(headers) > 0) {
      req <- httr2::req_headers(req, !!!headers)
    }
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(
        valid = TRUE,
        message = "Microsoft API accessible",
        authenticated = !is.null(dev_token) || !is.null(auth_token),
        rate_limit_status = "unknown"
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
