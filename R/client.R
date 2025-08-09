# Core HTTP Client Functions
#
# This file contains the HTTP client functions for making API requests
# with automatic retries, rate limiting, and platform-specific handling.

#' Build Platform Request
#' 
#' @description
#' Builds a request object based on platform configuration and parameters.
#' 
#' @param platform Character. Platform name
#' @param operation Character. Operation type
#' @param params List. Request parameters
#' 
#' @return Request object (httr2 request or special platform object)
#' @keywords internal
adlibr_build_request <- function(platform, operation, params) {
  
  # Load platform configuration
  config <- get_platform_config(platform)
  
  # Dispatch to platform-specific request builder
  switch(platform,
    "meta" = build_meta_request(operation, params, config),
    "tiktok" = build_tiktok_request(operation, params, config),
    "linkedin" = build_linkedin_request(operation, params, config),
    "apple" = build_apple_request(operation, params, config),
    "amazon" = build_amazon_request(operation, params, config),
    "google" = build_google_bq_request(operation, params, config),
    "booking" = build_booking_request(operation, params, config),
    "pinterest" = build_pinterest_request(operation, params, config),
    "snapchat" = build_snapchat_request(operation, params, config),
    "x" = build_x_request(operation, params, config),
    "microsoft" = build_microsoft_request(operation, params, config),
    cli::cli_abort("Unsupported platform: {platform}")
  )
}

#' Execute HTTP Request with Retries and Rate Limiting
#' 
#' @description
#' Executes an HTTP request with automatic retry logic, exponential backoff,
#' and platform-specific rate limiting. Handles various error conditions
#' including temporary network issues and API rate limits.
#' 
#' @param req Request object (httr2 request or platform-specific object)
#' @param platform Character. Platform name for error handling
#' @param operation Character. Operation type for error context
#' @param max_retries Integer. Maximum number of retry attempts
#' 
#' @return Response object
#' @keywords internal
#' 
#' @importFrom httr2 req_perform
adlibr_fetch <- function(req, platform, operation, max_retries = 3) {
  
  # Handle BigQuery separately
  if (is.list(req) && !is.null(req$type) && req$type == "bigquery") {
    return(execute_bigquery_request(req$params, platform))
  }
  
  # Handle LinkedIn direct search - returns parsed data directly
  if (is.list(req) && !is.null(req$type) && req$type == "linkedin_direct_search") {
    parsed_data <- linkedin_search_direct(req$params)
    # Add pagination info to match expected format
    parsed_data$paging <- list(
      start = req$params$page_token %||% 0,
      count = req$params$limit %||% 1,
      total = length(parsed_data$elements)
    )
    return(parsed_data)
  }
  
  # Regular HTTP request with retries
  for (attempt in seq_len(max_retries + 1)) {
    
    result <- tryCatch({
      response <- httr2::req_perform(req)
      handle_response_errors(response, platform, operation)
      return(response)
    }, adlibr_error_rate_limit = function(e) {
      if (attempt <= max_retries) {
        delay <- calculate_backoff_delay(attempt, platform)
        cli::cli_warn("Rate limited. Retrying in {delay} seconds... (attempt {attempt}/{max_retries})")
        Sys.sleep(delay)
        "retry"
      } else {
        cli::cli_abort("Rate limit exceeded after {max_retries} retries", call = NULL)
      }
    }, adlibr_error_api = function(e) {
      if (attempt <= max_retries && e$retry) {
        delay <- calculate_backoff_delay(attempt, platform)
        cli::cli_warn("API error (retryable). Retrying in {delay} seconds... (attempt {attempt}/{max_retries})")
        Sys.sleep(delay)
        "retry"
      } else {
        cli::cli_abort("API error: {e$message}", call = NULL)
      }
    }, error = function(e) {
      if (attempt <= max_retries) {
        delay <- calculate_backoff_delay(attempt, platform)
        cli::cli_warn("Request failed. Retrying in {delay} seconds... (attempt {attempt}/{max_retries})")
        Sys.sleep(delay)
        "retry"
      } else {
        cli::cli_abort("Request failed after {max_retries} retries: {e$message}", call = NULL)
      }
    })
    
    # If we get here and result is not "retry", return the response
    if (!identical(result, "retry")) {
      return(result)
    }
  }
  
  # This should never be reached due to the retry logic above
  cli::cli_abort("Unexpected error in retry logic", call = NULL)
}

#' Handle Response Errors
#' 
#' @description
#' Checks response for errors and throws appropriate custom exceptions
#' 
#' @param response HTTP response object
#' @param platform Character. Platform name
#' @param operation Character. Operation type
#' 
#' @return Invisible NULL if no errors
#' @keywords internal
handle_response_errors <- function(response, platform, operation) {
  
  status <- httr2::resp_status(response)
  
  # Success - no error handling needed
  if (status >= 200 && status < 300) {
    return(invisible(NULL))
  }
  
  # Parse error details if available
  error_details <- tryCatch({
    error_body <- httr2::resp_body_json(response)
    # Handle different error formats
    error_body$message %||% 
      error_body$error %||% 
      error_body$debug_message %||%  # Snapchat format
      error_body$display_message %||% # Snapchat format
      "Unknown error"
  }, error = function(e) {
    httr2::resp_body_string(response)
  })
  
  # Handle specific error types
  if (status == 401) {
    adlibr_abort(
      "Authentication failed",
      details = error_details,
      platform = platform,
      operation = operation,
      class = "adlibr_error_auth"
    )
  } else if (status == 403) {
    adlibr_abort(
      "Access forbidden - insufficient permissions",
      details = error_details,
      platform = platform,
      operation = operation,
      class = "adlibr_error_auth"
    )
  } else if (status == 404) {
    adlibr_abort(
      "Resource not found",
      details = error_details,
      platform = platform,
      operation = operation,
      class = "adlibr_error_api"
    )
  } else if (status == 429) {
    # Extract retry-after header if available
    retry_after <- httr2::resp_header(response, "Retry-After")
    
    adlibr_abort(
      "Rate limit exceeded",
      details = error_details,
      retry_after = retry_after,
      platform = platform,
      operation = operation,
      class = "adlibr_error_rate_limit"
    )
  } else if (status >= 500) {
    adlibr_abort(
      "Server error",
      details = error_details,
      platform = platform,
      operation = operation,
      retry = TRUE,
      class = "adlibr_error_api"
    )
  } else {
    adlibr_abort(
      "HTTP error {status}",
      details = error_details,
      platform = platform,
      operation = operation,
      class = "adlibr_error_api"
    )
  }
}

#' Calculate Exponential Backoff Delay
#' 
#' @description
#' Calculates delay for retry attempts using exponential backoff with jitter
#' 
#' @param attempt Integer. Current attempt number (1-based)
#' @param platform Character. Platform name for platform-specific limits
#' 
#' @return Numeric. Delay in seconds
#' @keywords internal
calculate_backoff_delay <- function(attempt, platform) {
  
  # Get platform-specific backoff factor
  config <- get_platform_config(platform)
  backoff_factor <- config$backoff_factor %||% 2
  
  # Base delay: 2^(attempt-1) seconds
  base_delay <- backoff_factor^(attempt - 1)
  
  # Add jitter (±25% random variation) to prevent thundering herd
  jitter_factor <- stats::runif(1, 0.75, 1.25)
  delay <- base_delay * jitter_factor
  
  # Cap at reasonable maximum (5 minutes)
  return(min(delay, 300))
}

#' Execute Request with Platform-Specific Pagination
#' 
#' @description
#' Executes an API request with automatic pagination handling.
#' Uses platform-specific pagination strategies including Meta's 
#' intelligent rate limiting.
#' 
#' @param req httr2 request object or list (for special cases like BigQuery/Meta)
#' @param platform Character. Platform name
#' @param operation Character. Operation type
#' @param max_pages Integer. Maximum pages to fetch
#' @param verbose Logical. Show progress messages
#' @param params List. Original request parameters (for Meta pagination)
#' 
#' @return List. Combined results from all pages
#' @keywords internal
adlibr_paginate <- function(req, platform, operation, max_pages = 10, verbose = FALSE, params = NULL) {
  
  # Handle Meta's special pagination
  if (platform == "meta" && operation == "search_ads") {
    config <- get_platform_config("meta")
    return(paginate_meta_api(params, config, max_pages = max_pages, verbose = verbose))
  }
  
  # Handle BigQuery special case
  if (platform == "google" && is.list(req) && req$type == "bigquery") {
    return(execute_google_bq_request(req))
  }
  
  # Handle LinkedIn direct search
  if (platform == "linkedin" && is.list(req) && req$type == "linkedin_direct_search") {
    # LinkedIn direct search handles its own execution, just return the parsed result
    return(adlibr_fetch(req, platform, operation))
  }
  
  # Standard pagination for other platforms
  all_data <- list()
  current_page <- 1
  next_page_token <- NULL
  total_fetched <- 0
  
  if (verbose) {
    cli::cli_alert_info("Starting {platform} API pagination...")
  }
  
  while (current_page <= max_pages) {
    
    if (verbose) {
      cli::cli_alert("Fetching page {current_page}...")
    }
    
    # Update pagination token if needed
    if (!is.null(next_page_token)) {
      req <- update_pagination_token(req, platform, next_page_token)
    }
    
    # Execute request
    response <- adlibr_fetch(req, platform, operation)
    
    # Parse platform-specific response
    parsed <- parse_platform_response(response, platform, operation)
    
    # Extract data
    page_data <- parsed$data %||% parsed$elements %||% list()
    all_data <- c(all_data, page_data)
    total_fetched <- total_fetched + length(page_data)
    
    if (verbose) {
      cli::cli_alert_success("Page {current_page}: Retrieved {length(page_data)} items (total: {total_fetched})")
    }
    
    # Check for next page
    next_page_token <- extract_next_page_token(parsed, platform)
    if (is.null(next_page_token) || length(page_data) == 0) {
      if (verbose) {
        cli::cli_alert_success("No more pages available. Stopping pagination.")
      }
      break
    }
    
    current_page <- current_page + 1
    
    # Standard rate limiting delay
    Sys.sleep(0.5)
  }
  
  if (verbose) {
    cli::cli_alert_success("Pagination complete. Retrieved {total_fetched} total items across {current_page} pages.")
  }
  
  return(list(
    elements = all_data,
    total_count = total_fetched,
    pages_fetched = current_page - 1
  ))
}

#' Update Pagination Token in Request
#' 
#' @description
#' Updates request with next page token based on platform pagination strategy.
#' 
#' @param req httr2 request object
#' @param platform Character. Platform name
#' @param next_token Character. Next page token
#' 
#' @return Modified httr2 request object
#' @keywords internal
update_pagination_token <- function(req, platform, next_token) {
  
  switch(platform,
    "tiktok" = httr2::req_url_query(req, page_token = next_token),
    "pinterest" = httr2::req_url_query(req, bookmark = next_token),
    "x" = httr2::req_url_query(req, next_token = next_token),
    "linkedin" = httr2::req_url_query(req, start = as.integer(next_token)),
    "apple" = httr2::req_url_query(req, offset = as.integer(next_token)),
    "amazon" = httr2::req_url_query(req, offset = as.integer(next_token)),
    "snapchat" = httr2::req_url_query(req, offset = as.integer(next_token)),
    "microsoft" = httr2::req_url_query(req, `$skip` = as.integer(next_token)),
    # Default offset-based pagination
    httr2::req_url_query(req, offset = as.integer(next_token))
  )
}

#' Extract Next Page Token from Response
#' 
#' @description
#' Extracts next page token from platform response.
#' 
#' @param parsed List. Parsed API response
#' @param platform Character. Platform name
#' 
#' @return Character. Next page token or NULL
#' @keywords internal
extract_next_page_token <- function(parsed, platform) {
  
  paging <- parsed$paging %||% parsed$pagination %||% list()
  
  switch(platform,
    "tiktok" = paging$next_page_token,
    "pinterest" = paging$bookmark,
    "x" = paging$next_token,
    "linkedin" = {
      # LinkedIn uses start + count
      current_start <- paging$start %||% 0
      count <- paging$count %||% 25
      total <- paging$total %||% 0
      if (current_start + count < total) {
        as.character(current_start + count)
      } else {
        NULL
      }
    },
    "apple" = {
      # Apple uses offset
      current_offset <- paging$offset %||% 0
      limit <- paging$limit %||% 100
      total <- paging$total %||% 0
      if (current_offset + limit < total) {
        as.character(current_offset + limit)
      } else {
        NULL
      }
    },
    "amazon" = {
      # Amazon uses offset
      current_offset <- paging$offset %||% 0
      limit <- paging$limit %||% 100
      total <- paging$total %||% 0
      if (current_offset + limit < total) {
        as.character(current_offset + limit)
      } else {
        NULL
      }
    },
    "snapchat" = {
      # Snapchat uses offset
      current_offset <- paging$offset %||% 0
      limit <- paging$limit %||% 100
      total <- paging$total %||% 0
      if (current_offset + limit < total) {
        as.character(current_offset + limit)
      } else {
        NULL
      }
    },
    "microsoft" = {
      # Microsoft uses $skip
      current_skip <- paging$`$skip` %||% 0
      top <- paging$`$top` %||% 1000
      total <- paging$total %||% 0
      if (current_skip + top < total) {
        as.character(current_skip + top)
      } else {
        NULL
      }
    },
    # Default: look for common pagination patterns
    paging$`next` %||% paging$next_token %||% paging$next_page_token
  )
}

#' Parse Platform Response
#' 
#' @description
#' Dispatches to platform-specific response parser.
#' 
#' @param response Raw API response
#' @param platform Character. Platform name
#' @param operation Character. Operation type
#' 
#' @return List. Parsed response
#' @keywords internal
parse_platform_response <- function(response, platform, operation) {
  
  switch(platform,
    "meta" = parse_meta_response(response),
    "tiktok" = parse_tiktok_response(response, operation),
    "linkedin" = parse_linkedin_response(response, operation),
    "apple" = parse_apple_response(response, operation),
    "amazon" = parse_amazon_response(response, operation),
    "google" = parse_google_bq_response(response, operation),
    "booking" = parse_booking_response(response, operation),
    "pinterest" = parse_pinterest_response(response, operation),
    "snapchat" = parse_snapchat_response(response, operation),
    "x" = parse_x_response(response, operation),
    "microsoft" = parse_microsoft_response(response, operation),
    # Default JSON parsing
    httr2::resp_body_json(response)
  )
}

