# Apple App Store Ad Repository API
#
# This file contains platform-specific functions for accessing
# Apple's App Store Ad Repository API.

#' Build Apple-Specific Request
#' 
#' @description
#' Builds a request specifically for Apple's App Store Ad Repository API.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_apple_request <- function(operation, params, config) {
  
  if (operation == "search_ads") {
    # Apple Ad Repository API - requires RSQL format and entity lookup
    base_url <- "https://adrepository.apple.com/api/v1/ad-repository-ads"
    
    # Apple requires type, id, and countryOrRegion - all mandatory
    # First, we need to search for entities to get app/developer IDs
    
    # For now, let's build a basic query with available info
    # In a real implementation, we'd first call the entities endpoint
    query_parts <- character(0)
    
    # Default to APP type if not specified
    query_parts <- c(query_parts, "type==APP")
    
    # Use provided ID or default to a known developer with active ads
    if (!is.null(params$app_id)) {
      query_parts <- c(query_parts, paste0("id==", params$app_id))
    } else if (!is.null(params$developer_id)) {
      query_parts <- c(query_parts, "type==DEVELOPER") 
      query_parts[1] <- "type==DEVELOPER"  # Override APP type
      query_parts <- c(query_parts, paste0("id==", params$developer_id))
    } else {
      # Use GAMEHAUS LIMITED developer ID - known to have active ads
      query_parts[1] <- "type==DEVELOPER"  # Override APP type
      query_parts <- c(query_parts, "id==1562307552")  # GAMEHAUS LIMITED
    }
    
    # Countries (required) - default to DE,FR if not provided
    if (!is.null(params$countries)) {
      countries_str <- paste(toupper(params$countries), collapse = ",")
      query_parts <- c(query_parts, paste0("countryOrRegion=in=(", countries_str, ")"))
    } else {
      query_parts <- c(query_parts, "countryOrRegion=in=(DE,FR)")
    }
    
    # Date preset
    date_preset <- "LAST_90_DAYS"
    if (!is.null(params$date_range)) {
      date_preset <- params$date_range
    }
    query_parts <- c(query_parts, paste0("datePreset==", date_preset))
    
    # Build RSQL query
    ql_query <- paste(query_parts, collapse = ";")
    
    # Pagination
    limit <- if (!is.null(params$limit)) min(as.integer(params$limit), 50) else 50
    offset <- if (!is.null(params$page_token)) as.integer(params$page_token) else 0
    
    # Create GET request with RSQL query
    req <- httr2::request(base_url) |>
      httr2::req_headers(
        Accept = "application/json"  # Apple specifically rejects Accept: */*
      ) |>
      httr2::req_url_query(
        ql = ql_query,
        limit = limit,
        offset = offset
      ) |>
      httr2::req_timeout(30)
    
    return(req)
    
  } else if (operation == "advertisers") {
    # Entity search endpoint
    base_url <- "https://adrepository.apple.com/api/v1/ad-repository-entities"
    
    # Apple entities require 'name' parameter with min 2 chars
    name_query <- params$q %||% params$advertisers %||% "apple"
    if (nchar(name_query) < 2) {
      name_query <- "app"  # Default fallback
    }
    
    # Optional type filter
    types <- if (!is.null(params$types)) params$types else NULL
    
    # Pagination
    limit <- if (!is.null(params$limit)) min(as.integer(params$limit), 50) else 50
    offset <- if (!is.null(params$page_token)) as.integer(params$page_token) else 0
    
    query_params <- list(
      name = name_query,
      limit = limit,
      offset = offset
    )
    
    if (!is.null(types)) {
      query_params$types <- types
    }
    
    req <- httr2::request(base_url) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_url_query(!!!query_params) |>
      httr2::req_timeout(30)
    
    return(req)
    
  } else {
    cli::cli_abort("Unknown Apple operation: {operation}")
  }
}

#' Build Apple-Specific Parameters
#' 
#' @description
#' Converts unified parameters to Apple's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. Apple-formatted parameters
#' @keywords internal
build_apple_params <- function(params, operation, config) {
  
  apple_params <- list()
  
  if (operation == "search_ads") {
    
    # Required: type and id (advertiser lookup first)
    if (!is.null(params$advertisers)) {
      # For ad search, we need the advertiser ID
      apple_params$type <- "DEVELOPER"
      apple_params$id <- params$advertisers[1]  # Take first advertiser
    } else {
      # If no advertiser specified, we can't search ads directly in Apple
      cli::cli_warn("Apple ad search requires advertiser ID. Use advertisers parameter.")
      return(list())
    }
    
    # Optional: country/region
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      apple_params$countryOrRegion <- countries[1]  # Apple takes single country
    }
    
    # Date preset (Apple uses predefined ranges)
    if (!is.null(params$date_from) || !is.null(params$date_to)) {
      # Convert date range to Apple's preset format
      apple_params$datePreset <- determine_apple_date_preset(params$date_from, params$date_to)
    } else {
      apple_params$datePreset <- "LAST_90_DAYS"  # Default
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      apple_params$limit <- min(as.integer(params$limit), 100)  # Apple max per page
    } else {
      apple_params$limit <- 100
    }
    
    if (!is.null(params$page_token)) {
      apple_params$offset <- as.integer(params$page_token)
    } else {
      apple_params$offset <- 0
    }
    
  } else if (operation == "advertisers") {
    
    # Optional: search by name
    if (!is.null(params$keyword)) {
      apple_params$name <- sanitize_string(params$keyword)
    }
    
    # Type filter
    apple_params$types <- "APP,DEVELOPER"  # Search both apps and developers
    
    # Pagination
    if (!is.null(params$limit)) {
      apple_params$limit <- min(as.integer(params$limit), 100)
    } else {
      apple_params$limit <- 100
    }
    
    if (!is.null(params$page_token)) {
      apple_params$offset <- as.integer(params$page_token)
    } else {
      apple_params$offset <- 0
    }
  }
  
  return(apple_params)
}

#' Determine Apple Date Preset
#' 
#' @description
#' Converts date range to Apple's preset format.
#' 
#' @param date_from Date or character. Start date
#' @param date_to Date or character. End date
#' 
#' @return Character. Apple date preset
#' @keywords internal
determine_apple_date_preset <- function(date_from, date_to) {
  
  if (is.null(date_from) && is.null(date_to)) {
    return("LAST_90_DAYS")
  }
  
  # Convert to dates
  if (!is.null(date_from)) {
    date_from <- clock::as_date(date_from)
  }
  if (!is.null(date_to)) {
    date_to <- clock::as_date(date_to)
  } else {
    date_to <- Sys.Date()
  }
  
  # Calculate days difference
  if (!is.null(date_from)) {
    days_diff <- as.numeric(date_to - date_from)
    
    if (days_diff <= 90) {
      return("LAST_90_DAYS")
    } else if (days_diff <= 180) {
      return("LAST_180_DAYS")
    } else {
      return("LAST_YEAR")
    }
  }
  
  return("LAST_90_DAYS")
}

#' Parse Apple Response
#' 
#' @description
#' Parses Apple API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_apple_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # Apple returns ads in 'data' array
    ads <- content$data %||% list()
    
    result <- list(
      elements = ads,  # Change to elements for consistency
      pagination = content$pagination,
      total_count = length(ads)
    )
    
  } else if (operation == "advertisers") {
    # Apple returns entities in 'data' array
    entities <- content$data %||% list()
    
    result <- list(
      data = entities,
      pagination = content$pagination,
      total_count = length(entities)
    )
    
  } else if (operation == "countries") {
    countries <- content$data %||% list()
    
    result <- list(
      data = countries,
      total_count = length(countries)
    )
  }
  
  return(result)
}

#' Normalize Apple Data to Common Schema
#' 
#' @description
#' Enhanced Apple data normalization with improved field mapping.
#' 
#' @param raw_data List. Raw Apple API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_apple_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$elements) || length(raw_data$elements) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$elements
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "adId")
    advertiser_id <- safe_extract(ad, "developerId")
    advertiser_name <- safe_extract(ad, "developerName")
    
    # App information
    app_name <- safe_extract(ad, "appName")
    
    # Ad content - Apple stores text in adBanner object
    ad_banner <- safe_extract(ad, "adBanner", list())
    short_desc <- safe_extract(ad_banner, "shortDescription", "")
    promo_text <- safe_extract(ad_banner, "promotionalText", "")
    subtitle <- safe_extract(ad_banner, "subtitle", "")
    
    # Combine all text fields
    ad_text <- paste(c(subtitle, short_desc, promo_text), collapse = " ")
    ad_text <- if (nchar(trimws(ad_text)) > 0) trimws(ad_text) else NA_character_
    
    # Media assets - Apple stores in adAssets array
    ad_assets <- safe_extract(ad, "adAssets", list())
    
    # Extract URLs from asset list
    picture_urls <- character(0)
    video_urls <- character(0)
    
    if (length(ad_assets) > 0) {
      for (asset in ad_assets) {
        pic_url <- safe_extract(asset, "pictureUrl")
        vid_url <- safe_extract(asset, "videoUrl")
        
        if (!is.null(pic_url) && pic_url != "") {
          picture_urls <- c(picture_urls, pic_url)
        }
        if (!is.null(vid_url) && vid_url != "") {
          video_urls <- c(video_urls, vid_url)
        }
      }
    }
    
    # Combine all media URLs
    all_media_urls <- c(picture_urls, video_urls)
    
    media_type <- if (length(video_urls) > 0) {
      "video"
    } else if (length(picture_urls) > 0) {
      "image"
    } else {
      "unknown"
    }
    
    # Dates - Apple format: "2025-07-31 00:00:00.0"
    first_impression <- safe_extract(ad, "firstImpressionDate")
    last_impression <- safe_extract(ad, "lastImpressionDate")
    
    first_seen <- if (!is.null(first_impression)) {
      tryCatch({
        as.Date(substr(first_impression, 1, 10))  # Extract just "2025-07-31" part
      }, error = function(e) NA)
    } else NA
    
    last_seen <- if (!is.null(last_impression)) {
      tryCatch({
        as.Date(substr(last_impression, 1, 10))  # Extract just "2025-08-04" part
      }, error = function(e) NA)
    } else NA
    
    # Geographic data
    country_region <- safe_extract(ad, "countryOrRegion")
    countries <- if (!is.null(country_region)) {
      list(country_region)
    } else {
      list(character(0))
    }
    
    # Targeting information
    audience_refinement <- safe_extract(ad, "audienceRefinement", list())
    
    targeting_age <- list(safe_extract(audience_refinement, "ageTarget", character(0)))
    targeting_gender <- list(safe_extract(audience_refinement, "genderTarget", character(0)))
    targeting_locations <- list(safe_extract(audience_refinement, "locationTarget", character(0)))
    
    # App Store category as interest targeting
    app_category <- safe_extract(ad, "appCategory")
    targeting_interests <- if (!is.null(app_category)) {
      list(app_category)
    } else {
      list(character(0))
    }
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      advertiser_verified = NA,
      advertiser_type = "app_developer",
      payer = NA_character_,
      beneficiary = NA_character_,
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
      ad_creative_urls = list(character(0)),  # Apple doesn't expose click URLs
      ad_library_url = NA_character_,
      media_type = as.character(media_type),
      media_urls = list(all_media_urls),
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_range = NA_character_,
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

#' Search Apple Advertisers/Developers
#' 
#' @description
#' Search for app developers on Apple platform.
#' 
#' @param search_term Character. Search term for developer/app names
#' @param limit Integer. Maximum number of results
#' 
#' @return tibble. Developer/advertiser information
#' @keywords internal
search_apple_advertisers <- function(search_term, limit = 100) {
  
  config <- get_platform_config("apple")
  
  params <- list(
    keyword = search_term,
    limit = limit
  )
  
  req <- build_apple_request("advertisers", params, config)
  response <- httr2::req_perform(req)
  handle_response_errors(response, "apple", "advertisers")
  
  parsed <- parse_apple_response(response, "advertisers")
  
  # Convert to tibble
  if (length(parsed$data) > 0) {
    advertisers_df <- purrr::map_df(parsed$data, function(entity) {
      tibble::tibble(
        entity_id = safe_extract(entity, "id"),
        entity_name = safe_extract(entity, "name"),
        entity_type = safe_extract(entity, "type"),  # APP or DEVELOPER
        country_code = safe_extract(entity, "countryCode"),
        platform = "apple"
      )
    })
    return(advertisers_df)
  } else {
    return(tibble::tibble(
      entity_id = character(0),
      entity_name = character(0),
      entity_type = character(0),
      country_code = character(0),
      platform = character(0)
    ))
  }
}

#' Get Apple Countries/Regions
#' 
#' @description
#' Retrieves list of available countries/regions from Apple API.
#' 
#' @return tibble. Available countries/regions
#' @keywords internal
get_apple_countries <- function() {
  
  config <- get_platform_config("apple")
  
  req <- build_apple_request("countries", list(), config)
  response <- httr2::req_perform(req)
  handle_response_errors(response, "apple", "countries")
  
  parsed <- parse_apple_response(response, "countries")
  
  # Convert to tibble
  if (length(parsed$data) > 0) {
    countries_df <- purrr::map_df(parsed$data, function(country) {
      tibble::tibble(
        country_code = safe_extract(country, "code"),
        country_name = safe_extract(country, "name"),
        platform = "apple"
      )
    })
    return(countries_df)
  } else {
    return(tibble::tibble(
      country_code = character(0),
      country_name = character(0),
      platform = character(0)
    ))
  }
}

#' Validate Apple Credentials
#' 
#' @description
#' Validates Apple API access by making a test request.
#' Apple API is public so this mainly tests connectivity.
#' 
#' @return List with validation results
#' @keywords internal
validate_apple_credentials <- function() {
  
  # Test basic connectivity with countries endpoint
  tryCatch({
    countries <- get_apple_countries()
    
    if (nrow(countries) > 0) {
      return(list(
        valid = TRUE,
        message = "Apple API accessible",
        authenticated = FALSE,  # Public API
        countries_available = nrow(countries)
      ))
    } else {
      return(list(
        valid = FALSE,
        message = "Apple API returned no countries",
        error = "empty_response"
      ))
    }
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = paste("Apple API connection failed:", e$message),
      error = "connection_error"
    ))
  })
}
