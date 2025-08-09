# Meta (Facebook/Instagram) Ad Library API
#
# This file contains platform-specific functions for accessing
# Meta's Ad Library API with intelligent pagination and rate limiting.

#' Get Adaptive Delay Based on App Usage
#' 
#' @description 
#' Calculates the appropriate sleep delay based on Meta API usage percentages (calls, CPU time, total time).
#' Inspired by the [Wesleyan Media Project's utilities](https://github.com/Wesleyan-Media-Project/fb_ads_import/blob/main/race2022_utils.R).
#'
#' @param call_pct Numeric. Percentage of allowed call count used.
#' @param cpu_pct Numeric. Percentage of allowed CPU time used.
#' @param time_pct Numeric. Percentage of allowed total time used.
#'
#' @return A numeric value indicating the number of seconds to sleep.
#' @keywords internal
get_delay_value <- function(call_pct = 0, cpu_pct = 0, time_pct = 0) {
  max_usage <- max(call_pct, cpu_pct, time_pct, na.rm = TRUE)
  dplyr::case_when(
    max_usage > 95 ~ 90,
    max_usage > 90 ~ 30,
    max_usage > 80 ~ 10,
    max_usage > 75 ~ 5,
    max_usage > 50 ~ 3,
    TRUE ~ 1
  )
}

#' Paginated Meta API Fetch with Usage-Aware Throttling
#' 
#' @description 
#' Fetches results from the Meta Ad Library API using pagination with adaptive delays 
#' based on Meta's API usage headers (`x-business-use-case-usage`).
#' Inspired by the [Wesleyan Media Project](https://github.com/Wesleyan-Media-Project/fb_ads_import/blob/main/race2022_utils.R).
#'
#' @param params List. Query parameters for the Meta Ad Library API.
#' @param config List. Platform configuration
#' @param max_pages Integer. Max number of pages to fetch (default: 50).
#' @param max_usage_limit Numeric (0–100). Threshold beyond which throttling applies (default: 50).
#' @param verbose Logical. If TRUE (default), outputs progress and usage stats.
#'
#' @return List. Combined results from all API pages with pagination info.
#' @keywords internal
paginate_meta_api <- function(params, config, max_pages = 50, max_usage_limit = 50, verbose = FALSE) {
  
  # Get Meta access token
  access_token <- get_credential("META_TOKEN")
  if (is.null(access_token)) {
    cli::cli_abort("META_TOKEN environment variable required for Meta API access")
  }
  
  # Prepare for iteration
  all_elements <- list()
  next_url <- NULL
  total_fetched <- 0
  
  if (verbose) {
    cli::cli_alert_info("Starting Meta Ad Library API pagination...")
  }
  
  for (i in seq_len(max_pages)) {
    
    if (verbose) {
      cli::cli_alert("Fetching page {i}...")
    }
    
    # Build request URL
    if (is.null(next_url)) {
      # First request - build from params
      query_params <- build_meta_params(params, config)
      query_params$access_token <- access_token
      
      query_string <- purrr::imap_chr(query_params, ~ {
        paste0(URLencode(.y), "=", URLencode(as.character(.x), reserved = TRUE))
      }) %>%
        paste(collapse = "&")
      
      request_url <- paste0(config$base, "/ads_archive?", query_string)
    } else {
      # Use pagination URL
      request_url <- next_url
    }
    
    # Make request using httr for compatibility with existing logic
    raw <- httr::GET(request_url)
    
    # Check for HTTP errors
    if (httr::status_code(raw) != 200) {
      cli::cli_abort("Meta API request failed with status {httr::status_code(raw)}")
    }
    
    # Parse response
    parsed <- jsonlite::fromJSON(httr::content(raw, as = "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
    
    # Extract elements
    page_elements <- parsed$data %||% list()
    all_elements <- c(all_elements, page_elements)
    total_fetched <- total_fetched + length(page_elements)
    
    if (verbose) {
      cli::cli_alert_success("Page {i}: Retrieved {length(page_elements)} ads (total: {total_fetched})")
    }
    
    # Check and handle rate limiting using your custom logic
    ad_headers <- httr::headers(raw)
    app_use <- ad_headers[["x-business-use-case-usage"]]
    
    if (!is.null(app_use)) {
      usage <- tryCatch({
        jsonlite::fromJSON(app_use)[[1]]
      }, error = function(e) {
        list(call_count = 0, total_cputime = 0, total_time = 0)
      })
      
      max_usage <- max(usage$call_count, usage$total_cputime, usage$total_time, na.rm = TRUE)
      
      if (verbose) {
        cli::cli_alert("API usage: calls={usage$call_count}%, cpu={usage$total_cputime}%, time={usage$total_time}%")
      }
      
      if (max_usage > max_usage_limit) {
        delay <- get_delay_value(usage$call_count, usage$total_cputime, usage$total_time)
        if (verbose) {
          cli::cli_alert_warning("High usage detected. Sleeping for {delay}s...")
        }
        Sys.sleep(delay)
      }
    }
    
    # Check for next page
    next_url <- parsed$paging$`next`
    if (is.null(next_url)) {
      if (verbose) {
        cli::cli_alert_success("No more pages available. Stopping pagination.")
      }
      break
    }
  }
  
  if (verbose) {
    cli::cli_alert_success("Pagination complete. Retrieved {total_fetched} total ads across {i} pages.")
  }
  
  return(list(
    elements = all_elements,
    total_count = total_fetched,
    pages_fetched = i,
    next_url = next_url
  ))
}

#' Build Meta-Specific Request
#' 
#' @description
#' Builds a request specifically for Meta's Graph API Ad Library endpoint.
#' 
#' @param operation Character. Operation type ("search_ads", "details")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return httr2 request object
#' @keywords internal
build_meta_request <- function(operation, params, config) {
  
  # Get Meta access token
  access_token <- get_credential("META_TOKEN")
  if (is.null(access_token)) {
    cli::cli_abort("META_TOKEN environment variable required for Meta API access")
  }
  
  # Build base URL
  base_url <- glue::glue("{config$base}/ads_archive", 
                        .envir = list(version = config$version))
  
  # Create request
  req <- httr2::request(base_url) |>
    httr2::req_user_agent(create_user_agent()) |>
    httr2::req_headers(
      Authorization = paste("Bearer", access_token),
      `X-RestLi-Protocol-Version` = "2.0.0"
    ) |>
    httr2::req_timeout(config$default_timeout %||% 30)
  
  # Add query parameters
  query_params <- build_meta_params(params, config)
  if (length(query_params) > 0) {
    req <- httr2::req_url_query(req, !!!query_params)
  }
  
  return(req)
}

#' Build Meta-Specific Parameters
#' 
#' @description
#' Converts unified parameters to Meta's Graph API format.
#' 
#' @param params List. Unified parameters
#' @param config List. Platform configuration
#' 
#' @return List. Meta-formatted parameters
#' @keywords internal
build_meta_params <- function(params, config) {
  
  meta_params <- list()
  
  # Required: ad_reached_countries
  if (!is.null(params$countries)) {
    countries <- normalize_country_codes(params$countries)
    meta_params$ad_reached_countries <- paste(countries, collapse = ",")
  } else {
    # Meta requires this parameter, default to US
    meta_params$ad_reached_countries <- "US"
  }
  
  # Optional: search terms
  if (!is.null(params$keyword)) {
    meta_params$search_terms <- sanitize_string(params$keyword)
  }
  
  # Optional: ad type filter
  if (!is.null(params$types)) {
    allowed_types <- c("ALL", "POLITICAL_AND_ISSUE_ADS", "EMPLOYMENT_ADS", 
                      "HOUSING_ADS", "FINANCIAL_PRODUCTS_AND_SERVICES_ADS")
    if (params$types %in% allowed_types) {
      meta_params$ad_type <- params$types
    }
  }
  
  # Optional: date range
  if (!is.null(params$date_from)) {
    meta_params$ad_delivery_date_min <- format_date_for_api(params$date_from, "iso")
  }
  if (!is.null(params$date_to)) {
    meta_params$ad_delivery_date_max <- format_date_for_api(params$date_to, "iso")
  }
  
  # Optional: languages (ISO 639-1 codes)
  if (!is.null(params$languages)) {
    meta_params$languages <- paste(params$languages, collapse = ",")
  }
  
  # Optional: media type
  if (!is.null(params$media)) {
    meta_params$media_type <- params$media
  }
  
  # Pagination
  if (!is.null(params$limit)) {
    meta_params$limit <- min(as.integer(params$limit), 25)  # Meta max is 25
  } else {
    meta_params$limit <- 25
  }
  
  if (!is.null(params$page_token)) {
    meta_params$after <- params$page_token
  }
  
  # Fields selection - get comprehensive data
  meta_params$fields <- paste(c(
    "id", "page_id", "page_name", "ad_snapshot_url",
    "ad_delivery_start_time", "ad_delivery_stop_time",
    "ad_creative_bodies", "media_type", "spend", "impressions",
    "eu_total_reach", "beneficiary_payers", "target_ages",
    "target_gender", "target_locations", "impressions_by_country",
    "demographic_distribution", "delivery_by_region"
  ), collapse = ",")
  
  return(meta_params)
}

#' Parse Meta Response
#' 
#' @description
#' Parses Meta Graph API response and extracts ads data.
#' 
#' @param response httr2 response object
#' 
#' @return List. Parsed response data
#' @keywords internal
parse_meta_response <- function(response) {
  
  content <- httr2::resp_body_json(response)
  
  # Extract ads data
  ads <- content$data %||% list()
  
  # Extract pagination info
  paging <- content$paging %||% list()
  
  result <- list(
    elements = ads,
    paging = paging,
    total_count = length(ads)
  )
  
  return(result)
}

#' Normalize Meta Data to Common Schema
#' 
#' @description
#' Enhanced Meta data normalization with improved field mapping.
#' 
#' @param raw_data List. Raw Meta API response
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_meta_enhanced <- function(raw_data) {
  
  if (is.null(raw_data$elements) || length(raw_data$elements) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  elements <- raw_data$elements
  
  normalized <- purrr::map_df(elements, function(ad) {
    
    # Basic identifiers
    ad_id <- safe_extract(ad, "id")
    advertiser_id <- safe_extract(ad, "page_id")
    advertiser_name <- safe_extract(ad, "page_name")
    
    # Enhanced advertiser info
    advertiser_verified <- !is.null(safe_extract(ad, "page_verification"))
    
    # Dates (Meta uses milliseconds)
    first_seen <- parse_api_date(safe_extract(ad, "ad_delivery_start_time"), "epoch")
    last_seen <- parse_api_date(safe_extract(ad, "ad_delivery_stop_time"), "epoch")
    
    # Ad content
    ad_text <- collapse_list_column(safe_extract(ad, "ad_creative_bodies"))
    
    # Creative and links
    media_type <- safe_extract(ad, "media_type", "unknown")
    media_urls <- list(safe_extract(ad, "ad_snapshot_url"))
    ad_library_url <- safe_extract(ad, "ad_snapshot_url")
    
    # Financial data
    spend_range <- safe_extract(ad, "spend")
    impressions_range <- safe_extract(ad, "impressions")
    eu_reach <- safe_extract(ad, "eu_total_reach")
    
    # Beneficiary/payer info
    beneficiary_data <- safe_extract(ad, "beneficiary_payers", list())
    payer <- safe_extract(beneficiary_data, "payer")
    beneficiary <- safe_extract(beneficiary_data, "beneficiary")
    
    # Targeting information
    targeting_age <- list(safe_extract(ad, "target_ages", character(0)))
    targeting_gender <- list(safe_extract(ad, "target_gender", character(0)))
    targeting_locations <- list(safe_extract(ad, "target_locations", character(0)))
    
    # Geographic reach
    impression_countries <- safe_extract(ad, "impressions_by_country", list())
    countries <- if (length(impression_countries) > 0) {
      list(purrr::map_chr(impression_countries, ~ safe_extract(.x, "country", "")))
    } else {
      list(character(0))
    }
    
    # Demographics data
    demo_data <- safe_extract(ad, "demographic_distribution", list())
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      advertiser_verified = as.logical(advertiser_verified),
      advertiser_type = NA_character_,
      payer = as.character(payer),
      beneficiary = as.character(beneficiary),
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
      ad_creative_urls = list(character(0)),  # Meta doesn't expose click URLs
      ad_library_url = as.character(ad_library_url),
      media_type = as.character(media_type),
      media_urls = media_urls,
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_range = as.character(spend_range),
      impressions_range = as.character(impressions_range),
      eu_reach = as.character(eu_reach),
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

#' Get Meta Ad Details
#' 
#' @description
#' Retrieve detailed information for specific Meta ads.
#' 
#' @param ad_ids Character vector. Meta ad IDs
#' @param fields Character vector. Specific fields to retrieve
#' 
#' @return tibble. Detailed ad information
#' @keywords internal
get_meta_ad_details <- function(ad_ids, fields = NULL) {
  
  config <- get_platform_config("meta")
  
  results <- list()
  
  for (ad_id in ad_ids) {
    params <- list(
      id = ad_id,
      fields = fields
    )
    
    tryCatch({
      req <- build_meta_request("details", params, config)
      response <- httr2::req_perform(req)
      handle_response_errors(response, "meta", "details")
      
      parsed <- parse_meta_response(response)
      normalized <- normalize_meta_enhanced(parsed)
      
      results[[ad_id]] <- normalized
      
    }, error = function(e) {
      cli::cli_warn("Failed to get details for Meta ad {ad_id}: {e$message}")
    })
  }
  
  if (length(results) > 0) {
    return(dplyr::bind_rows(results))
  } else {
    return(create_empty_normalized_tibble())
  }
}

#' Check Meta Rate Limits
#' 
#' @description
#' Checks remaining rate limit from Meta API response headers.
#' 
#' @param response httr2 response object
#' 
#' @return List with rate limit information
#' @keywords internal
check_meta_rate_limits <- function(response) {
  
  # Meta uses different headers for rate limiting
  app_usage <- httr2::resp_header(response, "x-app-usage")
  business_usage <- httr2::resp_header(response, "x-business-use-case-usage")
  
  rate_info <- list(
    app_usage = app_usage,
    business_usage = business_usage,
    timestamp = Sys.time()
  )
  
  # Parse usage if available
  if (!is.null(app_usage)) {
    tryCatch({
      usage_data <- jsonlite::fromJSON(app_usage)
      rate_info$app_usage_parsed <- usage_data
    }, error = function(e) {
      # Ignore parsing errors
    })
  }
  
  return(rate_info)
}

#' Validate Meta Credentials
#' 
#' @description
#' Validates Meta API credentials by making a test request.
#' 
#' @return List with validation results
#' @keywords internal
validate_meta_credentials <- function() {
  
  access_token <- get_credential("META_TOKEN")
  
  if (is.null(access_token)) {
    return(list(
      valid = FALSE,
      message = "META_TOKEN not found",
      error = "missing_token"
    ))
  }
  
  # Test with minimal request
  tryCatch({
    req <- httr2::request("https://graph.facebook.com/v19.0/ads_archive") |>
      httr2::req_headers(Authorization = paste("Bearer", access_token)) |>
      httr2::req_url_query(
        ad_reached_countries = "US",
        limit = 1,
        fields = "id"
      ) |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(
        valid = TRUE,
        message = "Meta credentials valid",
        rate_info = check_meta_rate_limits(response)
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
