# Platform Registry Functions
# 
# This file contains functions for loading and managing platform configurations
# from the YAML registry file.

#' Load Platform Registry
#' 
#' @description
#' Loads the platform registry configuration from the package's YAML file.
#' This registry contains all platform-specific configuration including endpoints,
#' parameter mappings, authentication methods, and rate limits.
#' 
#' @return A list containing the platform registry configuration
#' @keywords internal
#' 
#' @importFrom yaml read_yaml
load_platform_registry <- function() {
  registry_path <- system.file("registry", "platforms.yml", package = "adlibr")
  
  if (!file.exists(registry_path)) {
    cli::cli_abort("Platform registry file not found at {.path {registry_path}}")
  }
  
  tryCatch({
    registry <- yaml::read_yaml(registry_path)
    return(registry)
  }, error = function(e) {
    cli::cli_abort("Failed to parse platform registry: {.code {e$message}}")
  })
}

#' Get Platform Configuration
#' 
#' @description
#' Retrieves configuration for a specific platform from the registry.
#' 
#' @param platform Character. The platform name (e.g., "meta", "tiktok")
#' @param registry List. Optional pre-loaded registry. If NULL, loads from file.
#' 
#' @return A list containing the platform's configuration
#' @keywords internal
get_platform_config <- function(platform, registry = NULL) {
  if (is.null(registry)) {
    registry <- load_platform_registry()
  }
  
  platform <- tolower(platform)
  
  if (!platform %in% names(registry)) {
    available_platforms <- setdiff(names(registry), "global")
    cli::cli_abort(
      "Platform {.val {platform}} not found in registry.",
      "i" = "Available platforms: {.val {available_platforms}}"
    )
  }
  
  config <- registry[[platform]]
  
  # Add global config as defaults
  if ("global" %in% names(registry)) {
    global_config <- registry[["global"]]
    for (key in names(global_config)) {
      if (!key %in% names(config)) {
        config[[key]] <- global_config[[key]]
      }
    }
  }
  
  return(config)
}

#' Get Supported Platforms
#' 
#' @description
#' Returns a character vector of all supported platform names.
#' 
#' @return Character vector of platform names
#' @export
adlibr_platforms <- function() {
  registry <- load_platform_registry()
  platforms <- setdiff(names(registry), "global")
  return(sort(platforms))
}

#' Validate Platform Parameter
#' 
#' @description
#' Validates that a platform name is supported and returns the normalized name.
#' 
#' @param platform Character. Platform name to validate
#' 
#' @return Character. Validated and normalized platform name
#' @keywords internal
validate_platform <- function(platform) {
  if (is.null(platform) || !is.character(platform) || length(platform) != 1) {
    cli::cli_abort("Platform must be a single character string")
  }
  
  platform <- tolower(trimws(platform))
  supported_platforms <- adlibr_platforms()
  
  if (!platform %in% supported_platforms) {
    cli::cli_abort(
      "Platform {.val {platform}} is not supported.",
      "i" = "Supported platforms: {.val {supported_platforms}}"
    )
  }
  
  return(platform)
}

#' Get Platform Parameter Mapping
#' 
#' @description
#' Gets the parameter name mapping for a specific platform and operation.
#' 
#' @param platform Character. The platform name
#' @param param Character. The unified parameter name
#' @param config List. Optional pre-loaded platform config
#' 
#' @return Character. The platform-specific parameter name, or NULL if not supported
#' @keywords internal
get_platform_param <- function(platform, param, config = NULL) {
  if (is.null(config)) {
    config <- get_platform_config(platform)
  }
  
  if ("params" %in% names(config) && param %in% names(config$params)) {
    return(config$params[[param]])
  }
  
  return(NULL)
}

#' Get Platform Endpoint
#' 
#' @description
#' Constructs the full endpoint URL for a platform operation.
#' 
#' @param platform Character. The platform name
#' @param operation Character. The operation name (e.g., "search_ads", "details")
#' @param config List. Optional pre-loaded platform config
#' @param ... Additional parameters for URL template substitution
#' 
#' @return Character. The full endpoint URL
#' @keywords internal
get_platform_endpoint <- function(platform, operation, config = NULL, ...) {
  if (is.null(config)) {
    config <- get_platform_config(platform)
  }
  
  if (!"endpoints" %in% names(config) || !operation %in% names(config$endpoints)) {
    cli::cli_abort(
      "Operation {.val {operation}} not supported for platform {.val {platform}}"
    )
  }
  
  endpoint_path <- config$endpoints[[operation]]
  
  if (is.null(endpoint_path)) {
    cli::cli_abort(
      "Operation {.val {operation}} is not available for platform {.val {platform}}"
    )
  }
  
  # Handle special case for BigQuery
  if (platform == "google" && config$base == "bigquery") {
    return("bigquery")
  }
  
  # Construct base URL with version substitution
  base_url <- config$base
  if (!is.null(config$version)) {
    base_url <- gsub("\\{version\\}", config$version, base_url)
  }
  
  # Add endpoint path
  full_url <- file.path(base_url, endpoint_path)
  
  # Handle additional parameter substitution
  params <- list(...)
  for (param_name in names(params)) {
    pattern <- paste0("\\{", param_name, "\\}")
    full_url <- gsub(pattern, params[[param_name]], full_url)
  }
  
  return(full_url)
}

#' Get Platform Rate Limits
#' 
#' @description
#' Gets rate limiting configuration for a platform.
#' 
#' @param platform Character. The platform name
#' @param config List. Optional pre-loaded platform config
#' 
#' @return List with rate limiting parameters
#' @keywords internal
get_platform_rate_limits <- function(platform, config = NULL) {
  if (is.null(config)) {
    config <- get_platform_config(platform)
  }
  
  # Default rate limits
  defaults <- list(
    requests_per_hour = 100,
    requests_per_day = 1000,
    retry_after_default = 60
  )
  
  if ("rate_limits" %in% names(config)) {
    # Merge with defaults
    rate_limits <- modifyList(defaults, config$rate_limits)
  } else {
    rate_limits <- defaults
  }
  
  return(rate_limits)
}

#' Check Platform Feature Support
#' 
#' @description
#' Checks if a platform supports a specific feature or parameter.
#' 
#' @param platform Character. The platform name
#' @param feature Character. The feature to check (e.g., "search_ads", "pagination")
#' @param config List. Optional pre-loaded platform config
#' 
#' @return Logical. TRUE if the feature is supported
#' @keywords internal
platform_supports <- function(platform, feature, config = NULL) {
  if (is.null(config)) {
    config <- get_platform_config(platform)
  }
  
  switch(feature,
    "search_ads" = "search_ads" %in% names(config$endpoints) && !is.null(config$endpoints$search_ads),
    "details" = "details" %in% names(config$endpoints) && !is.null(config$endpoints$details),
    "advertisers" = "advertisers" %in% names(config$endpoints) && !is.null(config$endpoints$advertisers),
    "pagination" = "paging" %in% names(config),
    "keyword_search" = !is.null(get_platform_param(platform, "keyword", config)),
    "date_filtering" = !is.null(get_platform_param(platform, "date_from", config)),
    "country_filtering" = !is.null(get_platform_param(platform, "countries", config)),
    FALSE
  )
}
