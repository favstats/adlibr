# Error Handling for adlibr
#
# This file defines custom error classes and error handling utilities
# for the adlibr package.

#' Custom adlibr Error Classes
#' 
#' @description
#' These functions create custom error conditions for different types of
#' errors that can occur in the adlibr package.
#' 
#' @param message Character. The error message
#' @param platform Character. The platform where the error occurred
#' @param endpoint Character. The API endpoint that caused the error
#' @param status_code Integer. HTTP status code (for API errors)
#' @param request_id Character. Request ID if available
#' @param response Character. Raw response content
#' @param call Environment. The call environment
#' 
#' @return An error condition object
#' @keywords internal
#' @name adlibr_errors

#' @rdname adlibr_errors
adlibr_error_api <- function(message, platform = NULL, endpoint = NULL, 
                            status_code = NULL, request_id = NULL, 
                            response = NULL, call = NULL) {
  structure(
    class = c("adlibr_error_api", "adlibr_error", "error", "condition"),
    list(
      message = message,
      platform = platform,
      endpoint = endpoint,
      status_code = status_code,
      request_id = request_id,
      response = response,
      call = call
    )
  )
}

#' @rdname adlibr_errors
adlibr_error_auth <- function(message, platform = NULL, call = NULL) {
  structure(
    class = c("adlibr_error_auth", "adlibr_error", "error", "condition"),
    list(
      message = message,
      platform = platform,
      call = call
    )
  )
}

#' @rdname adlibr_errors
adlibr_error_rate_limit <- function(message, platform = NULL, retry_after = NULL, 
                                   status_code = 429, call = NULL) {
  structure(
    class = c("adlibr_error_rate_limit", "adlibr_error", "error", "condition"),
    list(
      message = message,
      platform = platform,
      retry_after = retry_after,
      status_code = status_code,
      call = call
    )
  )
}

#' @rdname adlibr_errors
adlibr_error_param <- function(message, parameter = NULL, value = NULL, call = NULL) {
  structure(
    class = c("adlibr_error_param", "adlibr_error", "error", "condition"),
    list(
      message = message,
      parameter = parameter,
      value = value,
      call = call
    )
  )
}

#' Handle HTTP Response Errors
#' 
#' @description
#' Checks an HTTP response for errors and throws appropriate adlibr error conditions.
#' 
#' @param response httr2 response object
#' @param platform Character. The platform name
#' @param endpoint Character. The endpoint URL
#' 
#' @return The response object if no errors, otherwise throws an error
#' @keywords internal
#' 
#' @importFrom httr2 resp_status resp_body_string resp_header
handle_response_errors <- function(response, platform, endpoint) {
  status <- httr2::resp_status(response)
  
  # Success codes
  if (status >= 200 && status < 300) {
    return(response)
  }
  
  # Get response body for error details
  response_body <- tryCatch({
    httr2::resp_body_string(response)
  }, error = function(e) {
    "Unable to read response body"
  })
  
  # Get request ID if available
  request_id <- httr2::resp_header(response, "x-request-id") %||%
                httr2::resp_header(response, "request-id") %||%
                httr2::resp_header(response, "x-correlation-id")
  
  # Handle specific error types
  if (status == 401) {
    message <- glue::glue("Authentication failed for {platform}")
    if (nchar(response_body) > 0 && nchar(response_body) < 500) {
      message <- glue::glue("{message}: {response_body}")
    }
    
    cli::cli_abort(
      message = message,
      class = "adlibr_error_auth",
      platform = platform,
      call = rlang::caller_env()
    )
  }
  
  if (status == 403) {
    message <- glue::glue("Access forbidden for {platform}")
    if (nchar(response_body) > 0 && nchar(response_body) < 500) {
      message <- glue::glue("{message}: {response_body}")
    }
    
    cli::cli_abort(
      message = message,
      class = "adlibr_error_auth", 
      platform = platform,
      call = rlang::caller_env()
    )
  }
  
  if (status == 429) {
    retry_after <- httr2::resp_header(response, "retry-after")
    if (!is.null(retry_after)) {
      retry_after <- as.numeric(retry_after)
    }
    
    message <- glue::glue("Rate limit exceeded for {platform}")
    if (!is.null(retry_after)) {
      message <- glue::glue("{message} (retry after {retry_after} seconds)")
    }
    
    cli::cli_abort(
      message = message,
      class = "adlibr_error_rate_limit",
      platform = platform,
      retry_after = retry_after,
      status_code = status,
      call = rlang::caller_env()
    )
  }
  
  # Generic API error
  if (status >= 400 && status < 500) {
    message <- glue::glue("Client error ({status}) for {platform}")
  } else if (status >= 500) {
    message <- glue::glue("Server error ({status}) for {platform}")
  } else {
    message <- glue::glue("HTTP error ({status}) for {platform}")
  }
  
  if (nchar(response_body) > 0 && nchar(response_body) < 500) {
    message <- glue::glue("{message}: {response_body}")
  }
  
  cli::cli_abort(
    message = message,
    class = "adlibr_error_api",
    platform = platform,
    endpoint = endpoint,
    status_code = status,
    request_id = request_id,
    response = response_body,
    call = rlang::caller_env()
  )
}

#' Validate Parameters
#' 
#' @description
#' Validates common parameters used across adlibr functions.
#' 
#' @param platform Character. Platform name to validate
#' @param countries Character vector. Country codes to validate  
#' @param date_from Character or Date. Start date to validate
#' @param date_to Character or Date. End date to validate
#' @param limit Integer. Limit parameter to validate
#' 
#' @return List of validated parameters
#' @keywords internal
validate_parameters <- function(platform = NULL, countries = NULL, 
                               date_from = NULL, date_to = NULL, limit = NULL) {
  
  errors <- character(0)
  
  # Validate platform
  if (!is.null(platform)) {
    tryCatch({
      platform <- validate_platform(platform)
    }, error = function(e) {
      errors <<- c(errors, e$message)
    })
  }
  
  # Validate countries
  if (!is.null(countries)) {
    if (!is.character(countries) || any(is.na(countries))) {
      errors <- c(errors, "Countries must be a character vector without NA values")
    } else {
      # Check for valid ISO country codes (2 characters)
      invalid_countries <- countries[!grepl("^[A-Za-z]{2}$", countries)]
      if (length(invalid_countries) > 0) {
        errors <- c(errors, glue::glue("Invalid country codes: {paste(invalid_countries, collapse = ', ')}"))
      }
    }
  }
  
  # Validate dates
  if (!is.null(date_from)) {
    tryCatch({
      date_from <- as.Date(date_from)
    }, error = function(e) {
      errors <<- c(errors, glue::glue("Invalid date_from format: {e$message}"))
    })
  }
  
  if (!is.null(date_to)) {
    tryCatch({
      date_to <- as.Date(date_to)
    }, error = function(e) {
      errors <<- c(errors, glue::glue("Invalid date_to format: {e$message}"))
    })
  }
  
  # Validate date range
  if (!is.null(date_from) && !is.null(date_to)) {
    if (date_from > date_to) {
      errors <- c(errors, "date_from must be before or equal to date_to")
    }
  }
  
  # Validate limit
  if (!is.null(limit)) {
    if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit < 1) {
      errors <- c(errors, "limit must be a positive integer")
    }
    if (limit > 1000) {
      errors <- c(errors, "limit cannot exceed 1000")
    }
  }
  
  # Report all validation errors at once
  if (length(errors) > 0) {
    cli::cli_abort(
      "Parameter validation failed:",
      "*" = errors,
      class = "adlibr_error_param",
      call = rlang::caller_env()
    )
  }
  
  list(
    platform = platform,
    countries = countries,
    date_from = date_from,
    date_to = date_to,
    limit = limit
  )
}

#' Safely Extract Nested List Elements
#' 
#' @description
#' Safely extracts elements from nested lists, returning NA if path doesn't exist.
#' 
#' @param x List. The list to extract from
#' @param path Character vector. Path elements to traverse
#' @param default Default value to return if path doesn't exist
#' 
#' @return The extracted value or default
#' @keywords internal
safe_extract <- function(x, path, default = NA) {
  tryCatch({
    result <- x
    for (element in path) {
      if (is.list(result) && element %in% names(result)) {
        result <- result[[element]]
      } else {
        return(default)
      }
    }
    return(result)
  }, error = function(e) {
    return(default)
  })
}

#' Null Coalescing Operator
#' 
#' @description
#' Returns the first non-NULL value from left to right.
#' 
#' @param x First value
#' @param y Second value
#' 
#' @return The first non-NULL value
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
