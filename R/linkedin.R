# LinkedIn Ad Library API
#
# This file contains platform-specific functions for accessing
# LinkedIn's Ad Library API with OAuth 2.0 authentication.

#' Build LinkedIn-Specific Request
#' 
#' @description
#' Builds a request specifically for LinkedIn Ad Library API.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
# Just call the working li_query function and return normalized results
linkedin_search_direct <- function(params) {
  
  # Map adlibr params to li_query params
  keyword <- params$keyword %||% params$q
  countries <- params$countries 
  start_date <- params$date_from
  end_date <- params$date_to
  count <- params$limit %||% 1
  current_start <- params$page_token %||% 0
  
  # Get LinkedIn token
  
  token <- Sys.getenv("LINKEDIN_AD_LIBRARY_TOKEN")
  if (nchar(token) == 0) {
    cli::cli_abort("LinkedIn token not found. Set LINKEDIN_AD_LIBRARY_TOKEN environment variable.")
  }
  
  base_url <- "https://api.linkedin.com/rest/adLibrary"
  
  # Build criteria params exactly like working version
  criteria_params <- list(
    keyword = keyword,
    advertiser = NULL,
    countries = if (!is.null(countries)) build_countries_param(countries) else NULL,
    dateRange = if (!is.null(start_date) && !is.null(end_date)) build_daterange_param(start_date, end_date) else NULL
  )
  
  # Remove NULL elements
  criteria_params <- purrr::compact(criteria_params)
  
  # URL encode
  criteria_params <- lapply(names(criteria_params), function(name) {
    value <- criteria_params[[name]]
    if (name == "keyword" || name == "advertiser") {
      value <- urltools::url_encode(value)
    }
    return(paste0(name, "=", value))
  })
  
  # Format for the URL
  criteria_string <- paste(criteria_params, collapse = "&")
  
  # Build the full URL
  # Enforce LinkedIn's 25-ad limit
  actual_count <- min(count, 25)
  full_url <- paste0(base_url, "?q=criteria&", criteria_string, "&start=", current_start, "&count=", actual_count)
  
  # Make the API request exactly like working version
  response <- httr::GET(
    url = full_url,
    httr::add_headers(
      'Authorization' = paste("Bearer", token),
      'X-RestLi-Protocol-Version' = '2.0.0',
      'Linkedin-Version' = '202409'
    )
  )
  
  if (httr::http_error(response)) {
    status_code <- httr::status_code(response)
    error_content <- httr::content(response, 'text', encoding = 'UTF-8')
    cli::cli_abort("LinkedIn API Error {status_code}: {error_content}")
  }
  
  content <- httr::content(response, "parsed", "application/json", encoding = "UTF-8")
  elements <- content$elements
  
  if (is.null(elements) || length(elements) == 0) {
    return(list(elements = list()))
  }
  

  
  return(list(elements = elements))
}

build_linkedin_request <- function(operation, params, config) {
  # Just return a special marker - we'll handle this in adlibr_fetch
  return(list(
    type = "linkedin_direct_search",
    params = params
  ))
}

#' Build LinkedIn-Specific Parameters
#' 
#' @description
#' Converts unified parameters to LinkedIn's API format.
#' 
#' @param params List. Unified parameters
#' @param operation Character. Operation type
#' @param config List. Platform configuration
#' 
#' @return List. LinkedIn-formatted parameters
#' @keywords internal
build_linkedin_params <- function(params, operation, config) {
  
  linkedin_params <- list()
  
  if (operation == "search_ads") {
    
    # Keyword search (main query parameter)
    if (!is.null(params$q)) {
      linkedin_params$keyword <- params$q
    }
    
    # Countries (optional for LinkedIn - use proper URN format if provided)
    if (!is.null(params$countries)) {
      countries <- normalize_country_codes(params$countries)
      # Use LinkedIn's URN format: (value:List(urn%3Ali%3Acountry%3Aus))
      country_urns <- purrr::map_chr(countries, ~ paste0("urn%3Ali%3Acountry%3A", tolower(.x)))
      linkedin_params$countries <- paste0("(value:List(", paste(country_urns, collapse = ","), "))")
    }
    # Don't set a default - let LinkedIn handle global search
    
    # Search by keyword
    if (!is.null(params$keyword)) {
      linkedin_params$keyword <- sanitize_string(params$keyword)
    }
    
    # Search by advertiser
    if (!is.null(params$advertisers)) {
      linkedin_params$advertiser <- sanitize_string(params$advertisers[1])
    }
    
    # Date range
    if (!is.null(params$date_from)) {
      linkedin_params$start_date <- format_date_for_api(params$date_from, "iso")
    }
    if (!is.null(params$date_to)) {
      linkedin_params$end_date <- format_date_for_api(params$date_to, "iso")
    }
    
    # Pagination parameters
    if (!is.null(params$limit)) {
      linkedin_params$count <- min(as.integer(params$limit), 25)  # LinkedIn max is 25
    } else {
      linkedin_params$count <- 25
    }
    
    if (!is.null(params$page_token)) {
      linkedin_params$start <- as.integer(params$page_token)
    } else {
      linkedin_params$start <- 0
    }
    
  } else if (operation == "advertisers") {
    
    # Search by advertiser name
    if (!is.null(params$keyword)) {
      linkedin_params$name <- sanitize_string(params$keyword)
    }
    
    # Pagination
    if (!is.null(params$limit)) {
      linkedin_params$count <- min(as.integer(params$limit), 25)
    } else {
      linkedin_params$count <- 25
    }
    
    if (!is.null(params$page_token)) {
      linkedin_params$start <- as.integer(params$page_token)
    } else {
      linkedin_params$start <- 0
    }
  }
  
  return(linkedin_params)
}

#' Parse LinkedIn Response
#' 
#' @description
#' Parses LinkedIn API response and extracts relevant data.
#' 
#' @param response httr2 response object
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_linkedin_response <- function(response, operation) {
  
  content <- httr2::resp_body_json(response)
  
  if (operation == "search_ads") {
    # LinkedIn returns ads in 'elements' array
    elements <- content$elements %||% list()
    paging <- content$paging %||% list()
    
    result <- list(
      elements = elements,
      paging = paging,
      total_count = content$paging$total %||% length(elements)
    )
    
  } else if (operation == "advertisers") {
    # LinkedIn returns advertisers in 'elements' array
    elements <- content$elements %||% list()
    paging <- content$paging %||% list()
    
    result <- list(
      elements = elements,
      paging = paging,
      total_count = content$paging$total %||% length(elements)
    )
  }
  
  return(result)
}

#' Normalize LinkedIn Data to Common Schema
#' 
#' @description
#' Converts LinkedIn API data to adlibr normalized schema.
#' 
#' @param raw_data List. Raw LinkedIn API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_linkedin_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$elements) || length(raw_data$elements) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  elements <- raw_data$elements
  
  normalized <- purrr::map_df(elements, function(ad) {
    
    # Basic identifiers - extract IDs from URLs
    ad_library_url <- ad$adUrl %||% NA_character_
    ad_id <- if (!is.null(ad_library_url)) {
      # Extract ID from URL like "https://www.linkedin.com/ad-library/detail/756524573"
      stringr::str_extract(ad_library_url, "\\d+$")
    } else NA_character_
    
    advertiser_url <- ad$details$advertiser$advertiserUrl %||% NA_character_
    advertiser_id <- if (!is.null(advertiser_url)) {
      # Extract ID from URL like "https://www.linkedin.com/company/11071464"
      stringr::str_extract(advertiser_url, "\\d+$")
    } else NA_character_
    
    advertiser_name <- ad$details$advertiser$advertiserName %||% NA_character_
    
    # Payment information
    payer <- ad$details$advertiser$adPayer %||% NA_character_
    
    # Dates - check if adStatistics exists
    stats <- ad$details$adStatistics %||% list()
    first_seen <- if (!is.null(stats) && !is.null(stats$firstImpressionAt)) {
      as.Date(as.POSIXct(stats$firstImpressionAt / 1000, origin = "1970-01-01"))
    } else NA
    
    last_seen <- if (!is.null(stats) && !is.null(stats$latestImpressionAt)) {
      as.Date(as.POSIXct(stats$latestImpressionAt / 1000, origin = "1970-01-01"))
    } else NA
    
    # Ad content
    ad_text <- NA_character_  # Not available in search API
    
    # Media type  
    ad_type <- ad$details$type %||% "unknown"
    media_type <- if (grepl("VIDEO", ad_type, ignore.case = TRUE)) "video"
                  else if (grepl("IMAGE", ad_type, ignore.case = TRUE)) "image"
                  else "unknown"
    
    # Impressions data - only available if adStatistics exists
    if (!is.null(stats) && length(stats) > 0) {
      impressions <- stats$totalImpressions %||% list()
      impressions_low <- impressions$from %||% NA_integer_
      impressions_high <- impressions$to %||% NA_integer_
      
      # Geographic data from impressions by country
      impression_countries <- stats$impressionsDistributionByCountry %||% list()
      countries <- if (length(impression_countries) > 0) {
        country_codes <- purrr::map_chr(impression_countries, ~ {
          country_urn <- .x$country %||% ""
          gsub("urn:li:country:", "", country_urn)
        })
        list(country_codes[country_codes != ""])
      } else {
        list(character(0))
      }
    } else {
      # No statistics available in search API
      impressions_low <- NA_integer_
      impressions_high <- NA_integer_
      countries <- list(character(0))
    }
    
    # Targeting data from ad targeting (actual API structure)
    targeting_data <- ad$details$adTargeting %||% list()
    
    # Extract targeting by facet type
    targeting_age <- list(extract_linkedin_targeting(targeting_data, "Age"))
    targeting_gender <- list(extract_linkedin_targeting(targeting_data, "Gender")) 
    targeting_locations <- list(extract_linkedin_targeting(targeting_data, "Location"))
    targeting_interests <- list(extract_linkedin_targeting(targeting_data, c("Interest", "Industry", "Skill")))
    targeting_custom_audiences <- list(extract_linkedin_targeting(targeting_data, "Audience"))
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      platform = "linkedin",
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      advertiser_url = as.character(advertiser_url),
      advertiser_verified = NA,
      advertiser_type = NA_character_,
      payer = as.character(payer),
      beneficiary = NA_character_,
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
      ad_creative_urls = list(character(0)),
      ad_library_url = as.character(ad_library_url),
      media_type = as.character(media_type),
      media_urls = list(character(0)),
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_low = NA_integer_,
      spend_high = NA_integer_,
      spend_range = NA_character_,
      impressions_low = as.integer(impressions_low),
      impressions_high = as.integer(impressions_high),
      impressions_range = if (!is.na(impressions_low) && !is.na(impressions_high)) {
        paste0(format(impressions_low, big.mark = ","), " - ", format(impressions_high, big.mark = ","))
      } else NA_character_,
      eu_reach = NA_character_,
      targeting_age = targeting_age,
      targeting_gender = targeting_gender,
      targeting_locations = targeting_locations,
      targeting_interests = targeting_interests,
      targeting_behaviors = list(character(0)),
      targeting_custom_audiences = targeting_custom_audiences,
      targeting_exclusions = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Extract LinkedIn Targeting Information
#' 
#' @description
#' Extracts targeting information from LinkedIn ad targeting data.
#' 
#' @param targeting_data List. LinkedIn targeting data
#' @param facet_names Character vector. Facet names to extract
#' 
#' @return Character vector. Extracted targeting values
#' @keywords internal
extract_linkedin_targeting <- function(targeting_data, facet_names) {
  
  if (is.null(targeting_data) || length(targeting_data) == 0) {
    return(character(0))
  }
  
  # LinkedIn targeting data is typically a data frame with facet_name and included_segments
  if (is.data.frame(targeting_data)) {
    matching_rows <- targeting_data[targeting_data$facet_name %in% facet_names | 
                                   grepl(paste(tolower(facet_names), collapse = "|"), 
                                        tolower(targeting_data$facet_name)), ]
    
    if (nrow(matching_rows) > 0) {
      segments <- unlist(matching_rows$included_segments)
      return(as.character(segments[!is.na(segments)]))
    }
  }
  
  return(character(0))
}

#' Search LinkedIn Advertisers
#' 
#' @description
#' Search for advertisers on LinkedIn platform.
#' 
#' @param search_term Character. Search term for advertiser names
#' @param limit Integer. Maximum number of results
#' 
#' @return tibble. Advertiser information
#' @keywords internal
search_linkedin_advertisers <- function(search_term, limit = 25) {
  
  config <- get_platform_config("linkedin")
  
  params <- list(
    keyword = search_term,
    limit = limit
  )
  
  req <- build_linkedin_request("advertisers", params, config)
  response <- httr2::req_perform(req)
  handle_response_errors(response, "linkedin", "advertisers")
  
  parsed <- parse_linkedin_response(response, "advertisers")
  
  # Convert to tibble
  if (length(parsed$elements) > 0) {
    advertisers_df <- purrr::map_df(parsed$elements, function(advertiser) {
      tibble::tibble(
        advertiser_id = safe_extract(advertiser, "urn"),
        advertiser_name = safe_extract(advertiser, "name"),
        advertiser_url = safe_extract(advertiser, "url"),
        platform = "linkedin"
      )
    })
    return(advertisers_df)
  } else {
    return(tibble::tibble(
      advertiser_id = character(0),
      advertiser_name = character(0),
      advertiser_url = character(0),
      platform = character(0)
    ))
  }
}

#' Validate LinkedIn Credentials
#' 
#' @description
#' Validates LinkedIn API credentials by making a test request.
#' 
#' @return List with validation results
#' @keywords internal
validate_linkedin_credentials <- function() {
  
  access_token <- get_credential("LINKEDIN_TOKEN")
  
  if (is.null(access_token)) {
    return(list(
      valid = FALSE,
      message = "LINKEDIN_TOKEN not found",
      error = "missing_token"
    ))
  }
  
  # Test with minimal request
  tryCatch({
    config <- get_platform_config("linkedin")
    
    req <- httr2::request(paste0(config$base, "/search")) |>
      httr2::req_headers(
        Authorization = paste("Bearer", access_token),
        `X-RestLi-Protocol-Version` = "2.0.0"
      ) |>
      httr2::req_url_query(
        countries = "us",
        count = 1
      ) |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      content <- httr2::resp_body_json(response)
      return(list(
        valid = TRUE,
        message = "LinkedIn credentials valid",
        test_results = length(content$elements %||% list())
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
# Working helper functions from li_query

#' Build Country Parameter String
#'
#' @param countries A character vector of lowercase 2-letter ISO country codes
#' @return A URL-encoded string for the countries parameter
#' @keywords internal
build_countries_param <- function(countries) {
  if (is.null(countries) || length(countries) == 0) {
    return(NULL)
  }
  
  # Remove empty strings and whitespace
  countries <- trimws(countries)
  countries <- countries[countries != ""]
  
  if (length(countries) == 0) {
    return(NULL)
  }
  
  # Format according to LinkedIn API documentation
  country_urns <- purrr::map_chr(countries, ~ paste0("urn%3Ali%3Acountry%3A", toupper(.x)))
  param_string <- paste0("(value:List(", paste(country_urns, collapse = ","), "))")
  return(param_string)
}

#' Build Date Range Parameter String
#'
#' @param start_date The start date
#' @param end_date The end date
#' @return A URL-encoded string for the dateRange parameter
#' @keywords internal
build_daterange_param <- function(start_date, end_date) {
  if (is.null(start_date) || is.null(end_date)) {
    return(NULL)
  }
  
  # Try to convert dates and handle errors
  tryCatch({
    start_date <- lubridate::as_date(start_date)
    end_date <- lubridate::as_date(end_date)
    
    # Check if dates are valid (not NA)
    if (is.na(start_date) || is.na(end_date)) {
      stop("Invalid date format")
    }
    
    # Format according to LinkedIn API documentation
    start_str <- sprintf("(day:%d,month:%d,year:%d)",
                         lubridate::day(start_date), lubridate::month(start_date), lubridate::year(start_date))
    end_str <- sprintf("(day:%d,month:%d,year:%d)",
                       lubridate::day(end_date), lubridate::month(end_date), lubridate::year(end_date))

    # Combine into the final dateRange format
    param_string <- sprintf("(start:%s,end:%s)", start_str, end_str)
    return(param_string)
  }, error = function(e) {
    stop("Invalid date format: ", e$message)
  })
}
