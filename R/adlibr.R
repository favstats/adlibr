# Main User-Facing Functions for adlibr
#
# This file contains the primary functions that users interact with
# to search, retrieve, and analyze ad library data across platforms.

#' Search Ads Across Platforms
#'
#' @description
#' Search for advertisements across supported ad library platforms using
#' unified parameters. This function automatically handles platform-specific
#' API differences, authentication, pagination, and data normalization.
#'
#' @param platform Character. Platform to search ("meta", "tiktok", "microsoft", 
#'   "apple", "amazon", "google", "booking", "pinterest", "snapchat", "x")
#' @param q Character. Keyword search terms (optional)
#' @param advertisers Character vector. Advertiser names or IDs to filter by (optional)
#' @param countries Character vector. ISO 3166-1 alpha-2 country codes (optional)
#' @param date_from Character or Date. Start date (YYYY-MM-DD format, optional)
#' @param date_to Character or Date. End date (YYYY-MM-DD format, optional)
#' @param types Character vector. Ad types to filter by (platform-specific, optional)
#' @param languages Character vector. ISO 639-1 language codes (optional)
#' @param media Character. Media type filter (optional)
#' @param limit Integer. Maximum number of results per page (default: 100)
#' @param page_token Character. Pagination token for next page (optional)
#' @param fields Character vector. Specific fields to retrieve (optional)
#' @param ... Additional platform-specific parameters
#'
#' @return tibble. Normalized ad data with the following columns:
#'   \describe{
#'     \item{ad_id}{Platform ad identifier}
#'     \item{platform}{Source platform}
#'     \item{advertiser_id}{Advertiser identifier}
#'     \item{advertiser_name}{Advertiser display name}
#'     \item{payer}{Entity that paid for the ad}
#'     \item{beneficiary}{Beneficiary entity}
#'     \item{status}{Ad status (ACTIVE/INACTIVE/etc.)}
#'     \item{first_seen}{First shown date}
#'     \item{last_seen}{Last shown date}
#'     \item{ad_text}{Ad text content}
#'     \item{media_type}{Type of media (image/video/etc.)}
#'     \item{media_urls}{List of media asset URLs}
#'     \item{landing_url}{Destination URL}
#'     \item{countries}{List of target countries}
#'     \item{languages}{List of languages}
#'     \item{spend_range}{Spending range (if available)}
#'     \item{impressions_range}{Impression range}
#'     \item{eu_reach}{EU-specific reach data}
#'     \item{targeting_age}{Age targeting criteria}
#'     \item{targeting_gender}{Gender targeting}
#'     \item{targeting_locations}{Location targeting}
#'     \item{targeting_other}{Other targeting criteria}
#'     \item{extra_json}{Raw platform-specific data}
#'     \item{fetched_at}{Data retrieval timestamp}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search Meta ads for "climate change" in US and Canada
#' meta_ads <- adlibr_search("meta", 
#'                          q = "climate change",
#'                          countries = c("US", "CA"),
#'                          date_from = "2024-01-01",
#'                          limit = 50)
#'
#' # Search TikTok ads by advertiser
#' tiktok_ads <- adlibr_search("tiktok",
#'                            advertisers = "Example Corp",
#'                            countries = "US")
#'
#' # Search across multiple platforms (requires separate calls)
#' platforms <- c("meta", "tiktok", "microsoft")
#' all_ads <- purrr::map_df(platforms, ~adlibr_search(.x, q = "election"))
#' }
adlibr_search <- function(platform, q = NULL, advertisers = NULL, countries = NULL,
                         date_from = NULL, date_to = NULL, types = NULL, 
                         languages = NULL, media = NULL, limit = 100, 
                         page_token = NULL, fields = NULL, verbose = FALSE, ...) {
  
  # Validate and normalize parameters
  validated <- validate_parameters(
    platform = platform,
    countries = countries,
    date_from = date_from,
    date_to = date_to,
    limit = limit
  )
  
  platform <- validated$platform
  
  # Check if platform supports search
  if (!platform_supports(platform, "search_ads")) {
    cli::cli_abort("Platform {.val {platform}} does not support ad searching")
  }
  
  # Check authentication
  if (!check_env_vars(platform)) {
    cli::cli_abort(
      "Authentication required for {.val {platform}}",
      "i" = "Run {.code adlibr_auth_setup()} to configure credentials"
    )
  }
  
  # Build request parameters
  params <- list(
    keyword = q,
    advertisers = advertisers,
    countries = validated$countries,
    date_from = validated$date_from,
    date_to = validated$date_to,
    types = types,
    languages = languages,
    media = media,
    limit = validated$limit,
    page_token = page_token,
    fields = fields,
    ...
  )
  
  # Remove NULL parameters
  params <- purrr::compact(params)
  
  # Build and execute request with pagination
  req <- adlibr_build_request(platform, "search_ads", params)
  raw_data <- adlibr_paginate(req, platform, "search_ads", max_pages = 1, verbose = verbose, params = params)
  
  # Normalize to common schema
  normalized_data <- adlibr_normalize(raw_data, platform)
  
  cli::cli_alert_success("Retrieved {nrow(normalized_data)} ads from {platform}")
  
  return(normalized_data)
}

#' Search Advertisers on Platforms
#'
#' @description
#' Search for advertisers/organizations on platforms that support advertiser lookup.
#'
#' @param platform Character. Platform to search
#' @param q Character. Advertiser name to search for
#' @param country Character. Country code to filter by (optional)
#' @param limit Integer. Maximum number of results (default: 100)
#' @param page_token Character. Pagination token (optional)
#' @param fields Character vector. Specific fields to retrieve (optional)
#' @param ... Additional platform-specific parameters
#'
#' @return tibble. Advertiser information
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for advertisers on TikTok
#' advertisers <- adlibr_advertisers("tiktok", q = "Nike")
#'
#' # Search for advertisers in specific country
#' us_advertisers <- adlibr_advertisers("microsoft", q = "Microsoft", country = "US")
#' }
adlibr_advertisers <- function(platform, q, country = NULL, limit = 100, 
                              page_token = NULL, fields = NULL, ...) {
  
  platform <- validate_platform(platform)
  
  # Check if platform supports advertiser search
  if (!platform_supports(platform, "advertisers")) {
    cli::cli_abort("Platform {.val {platform}} does not support advertiser search")
  }
  
  # Check authentication
  if (!check_env_vars(platform)) {
    cli::cli_abort(
      "Authentication required for {.val {platform}}",
      "i" = "Run {.code adlibr_auth_setup()} to configure credentials"
    )
  }
  
  # Validate parameters
  if (is.null(q) || nchar(trimws(q)) == 0) {
    cli::cli_abort("Search query {.arg q} is required for advertiser search")
  }
  
  if (!is.null(limit) && (limit < 1 || limit > 1000)) {
    cli::cli_abort("limit must be between 1 and 1000")
  }
  
  # Build request parameters
  params <- list(
    keyword = q,
    country = country,
    limit = limit,
    page_token = page_token,
    fields = fields,
    ...
  )
  
  # Remove NULL parameters
  params <- purrr::compact(params)
  
  # Build and execute request
  req <- adlibr_build_request(platform, "advertisers", params)
  response <- adlibr_fetch(req, platform, "advertisers")
  
  # Parse response based on platform
  raw_data <- parse_platform_response(response, platform)
  
  # TODO: Implement advertiser-specific normalization
  # For now, return raw data in a tibble format
  
  cli::cli_alert_success("Retrieved advertiser data from {platform}")
  
  return(tibble::as_tibble(raw_data))
}

#' Get Detailed Ad Information
#'
#' @description
#' Retrieve detailed information for specific ads by their IDs.
#'
#' @param platform Character. Platform name
#' @param ad_id Character vector. Ad IDs to retrieve
#' @param fields Character vector. Specific fields to retrieve (optional)
#' @param ... Additional platform-specific parameters
#'
#' @return tibble. Detailed ad information
#' @export
#'
#' @examples
#' \dontrun{
#' # Get details for specific ads
#' details <- adlibr_details("meta", c("123456789", "987654321"))
#'
#' # Get specific fields only
#' details <- adlibr_details("tiktok", "ad_id_123", 
#'                          fields = c("ad.id", "ad.status", "reach"))
#' }
adlibr_details <- function(platform, ad_id, fields = NULL, ...) {
  
  platform <- validate_platform(platform)
  
  # Check if platform supports details
  if (!platform_supports(platform, "details")) {
    cli::cli_abort("Platform {.val {platform}} does not support ad details lookup")
  }
  
  # Check authentication
  if (!check_env_vars(platform)) {
    cli::cli_abort(
      "Authentication required for {.val {platform}}",
      "i" = "Run {.code adlibr_auth_setup()} to configure credentials"
    )
  }
  
  # Validate ad_id parameter
  if (is.null(ad_id) || length(ad_id) == 0) {
    cli::cli_abort("At least one ad_id is required")
  }
  
  ad_id <- as.character(ad_id)
  
  # For platforms that support batch requests, process all at once
  # For others, process individually and combine
  all_results <- list()
  
  for (id in ad_id) {
    
    # Build request parameters
    params <- list(
      ad_id = id,
      fields = fields,
      ...
    )
    
    # Remove NULL parameters
    params <- purrr::compact(params)
    
    tryCatch({
      # Build and execute request
      req <- adlibr_build_request(platform, "details", params)
      response <- adlibr_fetch(req, platform, "details")
      
      # Parse response
      raw_data <- parse_platform_response(response, platform)
      
      # Normalize data
      normalized_data <- adlibr_normalize(raw_data, platform)
      
      all_results[[id]] <- normalized_data
      
    }, error = function(e) {
      cli::cli_warn("Failed to retrieve details for ad {.val {id}}: {e$message}")
    })
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    result <- dplyr::bind_rows(all_results)
    cli::cli_alert_success("Retrieved details for {nrow(result)} ads from {platform}")
    return(result)
  } else {
    cli::cli_warn("No ad details retrieved")
    return(create_empty_normalized_tibble())
  }
}

#' Generate Platform Report
#'
#' @description
#' Generate aggregate reports and statistics for platforms that support it.
#' This function provides summary statistics and insights about ad spending,
#' reach, and other metrics.
#'
#' @param platform Character. Platform name
#' @param report_type Character. Type of report ("spending", "reach", "advertisers")
#' @param ... Additional platform-specific parameters
#'
#' @return tibble. Report data
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate spending report
#' spending_report <- adlibr_report("meta", "spending")
#'
#' # Generate reach report for specific period
#' reach_report <- adlibr_report("google", "reach", 
#'                              date_from = "2024-01-01",
#'                              date_to = "2024-12-31")
#' }
adlibr_report <- function(platform, report_type = "summary", ...) {
  
  platform <- validate_platform(platform)
  
  cli::cli_inform("Generating {report_type} report for {platform}...")
  
  # This is a placeholder for future report functionality
  # Different platforms may offer different reporting capabilities
  
  cli::cli_warn("Reporting functionality not yet implemented for {platform}")
  
  return(tibble::tibble(
    platform = platform,
    report_type = report_type,
    message = "Reporting not yet implemented",
    generated_at = clock::sys_time_now()
  ))
}

#' Parse Platform Response
#'
#' @description
#' Parses raw HTTP responses based on platform-specific formats.
#'
#' @param response httr2 response object or raw data
#' @param platform Character. Platform name
#'
#' @return List. Parsed response data
#' @keywords internal
parse_platform_response <- function(response, platform) {
  
  # Handle BigQuery responses (already parsed)
  if (is.data.frame(response)) {
    return(response)
  }
  
  # Handle httr2 responses
  if (inherits(response, "httr2_response")) {
    
    # Check content type
    content_type <- httr2::resp_content_type(response)
    
    if (grepl("application/json", content_type)) {
      parsed <- httr2::resp_body_json(response)
    } else {
      cli::cli_warn("Unexpected content type: {content_type}")
      parsed <- httr2::resp_body_string(response)
    }
    
  } else {
    # Assume already parsed
    parsed <- response
  }
  
  return(parsed)
}

#' Get All Available Data with Pagination
#'
#' @description
#' Automatically handles pagination to retrieve all available data from a platform.
#' Use with caution as this can result in many API calls.
#'
#' @param platform Character. Platform name
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param max_pages Integer. Maximum number of pages to retrieve (default: 10)
#' @param delay Numeric. Delay between requests in seconds (default: 1)
#'
#' @return tibble. All retrieved data
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all ads for a keyword (up to 10 pages)
#' all_ads <- adlibr_get_all("meta", "search_ads", 
#'                          list(keyword = "election", countries = "US"))
#'
#' # Get all advertisers (with custom limits)
#' all_advertisers <- adlibr_get_all("tiktok", "advertisers", 
#'                                  list(keyword = "tech"),
#'                                  max_pages = 5, delay = 2)
#' }
adlibr_get_all <- function(platform, operation, params = list(), 
                          max_pages = 10, delay = 1) {
  
  platform <- validate_platform(platform)
  
  if (!operation %in% c("search_ads", "advertisers")) {
    cli::cli_abort("operation must be 'search_ads' or 'advertisers'")
  }
  
  if (max_pages < 1 || max_pages > 100) {
    cli::cli_abort("max_pages must be between 1 and 100")
  }
  
  cli::cli_alert_info("Retrieving all data from {platform} (max {max_pages} pages)...")
  
  all_data <- list()
  page_token <- NULL
  page_count <- 0
  
  repeat {
    page_count <- page_count + 1
    
    # Add pagination token to params
    current_params <- params
    if (!is.null(page_token)) {
      current_params$page_token <- page_token
    }
    
    # Make request based on operation
    if (operation == "search_ads") {
      result <- adlibr_search(
        platform = platform,
        q = current_params$keyword,
        advertisers = current_params$advertisers,
        countries = current_params$countries,
        date_from = current_params$date_from,
        date_to = current_params$date_to,
        types = current_params$types,
        languages = current_params$languages,
        media = current_params$media,
        limit = current_params$limit %||% 100,
        page_token = page_token
      )
    } else {
      result <- adlibr_advertisers(
        platform = platform,
        q = current_params$keyword,
        country = current_params$country,
        limit = current_params$limit %||% 100,
        page_token = page_token
      )
    }
    
    if (nrow(result) == 0) {
      cli::cli_alert_info("No more data available")
      break
    }
    
    all_data[[page_count]] <- result
    
    cli::cli_alert_success("Retrieved page {page_count} ({nrow(result)} records)")
    
    # Check if we should continue
    if (page_count >= max_pages) {
      cli::cli_alert_info("Reached maximum pages limit")
      break
    }
    
    # TODO: Extract next page token from response
    # This depends on platform-specific pagination implementation
    page_token <- NULL  # Placeholder
    
    if (is.null(page_token)) {
      cli::cli_alert_info("No more pages available")
      break
    }
    
    # Delay between requests
    if (delay > 0) {
      Sys.sleep(delay)
    }
  }
  
  # Combine all data
  if (length(all_data) > 0) {
    final_data <- dplyr::bind_rows(all_data)
    cli::cli_alert_success("Total records retrieved: {nrow(final_data)}")
    return(final_data)
  } else {
    cli::cli_warn("No data retrieved")
    return(create_empty_normalized_tibble())
  }
}
