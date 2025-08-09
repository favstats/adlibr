# LinkedIn Ad Library API R Functions

# This script provides a set of functions to interact with the LinkedIn Ad Library API.
# It includes helpers for authentication, URL parameter construction, and a primary
# function for querying ads with automatic pagination.

#==============================================================================#
# Dependencies
#==============================================================================#

# Ensure you have these packages installed:
# install.packages(c("httr", "jsonlite", "dplyr", "purrr", "tibble", "usethis", "lubridate", "urltools"))




#==============================================================================#
# 2. Helper Functions for Parameter Formatting
#==============================================================================#

#' Build Country Parameter String
#'
#' Formats a vector of 2-letter country codes into the specific URN list
#' format required by the LinkedIn API.
#'
#' @param countries A character vector of lowercase 2-letter ISO country codes (e.g., c("us", "gb")).
#' @return A URL-encoded string for the `countries` parameter.
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
  # Example: countries=(value:List(urn%3Ali%3Acountry%3Aus))
  country_urns <- purrr::map_chr(countries, ~ paste0("urn%3Ali%3Acountry%3A", toupper(.x)))
  param_string <- paste0("(value:List(", paste(country_urns, collapse = ","), "))")
  return(param_string)
}

#' Build Date Range Parameter String
#'
#' Formats start and end dates into the specific date range format
#' required by the LinkedIn API.
#'
#' @param start_date The start date (Date object or "YYYY-MM-DD" string).
#' @param end_date The end date (Date object or "YYYY-MM-DD" string).
#' @return A URL-encoded string for the `dateRange` parameter.
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
    # Example: dateRange=(start:(day:1,month:1,year:2023),end:(day:1,month:2,year:2023))
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


#==============================================================================#
# 3. Main Query Function
#==============================================================================#

#' Query the LinkedIn Ad Library with Automatic Pagination
#'
#' This function queries the LinkedIn Ad Library API based on specified criteria.
#' It automatically handles pagination to retrieve all available results up to the
#' specified `max_pages` limit. Supports all official API parameters including
#' keyword search, advertiser filtering, country targeting, and date ranges.
#'
#' @param keyword A string of one or more space-separated keywords to search in ad content.
#'   Multiple keywords are treated with logical AND operation. Optional.
#' @param advertiser A string containing the name of the advertiser to filter by. Optional.
#' @param countries A character vector of 2-letter ISO country codes (e.g., c("us", "gb", "de")).
#'   Case insensitive. Optional.
#' @param start_date The start date for the ad search range (inclusive). Can be a Date object 
#'   or "YYYY-MM-DD" string. Optional.
#' @param end_date The end date for the ad search range (exclusive). Can be a Date object 
#'   or "YYYY-MM-DD" string. Optional.
#' @param count The number of ads to return per page. Maximum is 25 (API limitation). 
#'   Defaults to 25.
#' @param max_pages The maximum number of pages to retrieve. Set to control API usage.
#'   Defaults to `Inf` (retrieve all available pages).
#' @param linkedin_api_version The LinkedIn API version to use. Defaults to "202409".
#'   Should not normally need to be changed.
#' @param verbose Logical. If TRUE, provides detailed progress output during execution.
#'   Defaults to TRUE.
#'
#' @return A `tibble` containing the ad data with the following columns:
#'   \describe{
#'     \item{ad_url}{Direct URL to the ad preview page}
#'     \item{is_restricted}{Boolean indicating if ad violates LinkedIn policies}
#'     \item{restriction_details}{Details about policy violations (if any)}
#'     \item{advertiser_name}{Name of the advertising organization}
#'     \item{advertiser_url}{LinkedIn page URL of the advertiser}
#'     \item{ad_payer}{Entity that paid for the advertisement}
#'     \item{ad_type}{Format/type of the advertisement (e.g., "SPOTLIGHT_V2")}
#'     \item{first_impression_at}{First time the ad was shown (POSIXct)}
#'     \item{latest_impression_at}{Most recent time the ad was shown (POSIXct)}
#'     \item{total_impressions_from}{Lower bound of total impressions}
#'     \item{total_impressions_to}{Upper bound of total impressions}
#'     \item{impressions_by_country}{List-column with country-wise impression data}
#'     \item{ad_targeting}{List-column with detailed targeting criteria}
#'   }
#'
#' @section Authentication:
#' Requires prior authentication using \code{\link{li_auth_configure}} and \code{\link{li_auth}}.
#' The function automatically uses cached OAuth tokens.
#'
#' @section Rate Limits:
#' The LinkedIn API limits responses to 25 ads per request. Use the `max_pages` parameter
#' to control the total number of API calls. Large queries may take time to complete.
#'
#' @section API Reference:
#' Based on the LinkedIn Ad Library API: \url{https://www.linkedin.com/ad-library/api/ads}
#'
#' @export
#'
#' @importFrom httr GET add_headers content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_df
#' @importFrom tibble as_tibble
#' @importFrom usethis ui_done ui_oops ui_info
#' @importFrom lubridate as_date day month year
#' @importFrom urltools url_encode
#'
#' @examples
#' \dontrun{
#' # First, authenticate:
#' # li_auth()
#' # readRenviron(".Renviron") # Run this after authenticating
#'
#' # Example 1: Search for ads by keyword and country
# ads_by_keyword <- li_query(
#   keyword = "data science",
#   countries = c("us", "ca"),
#   start_date = "2024-01-01",
#   end_date = "2024-03-31"
# )
#'
#' # Example 2: Search for ads by a specific advertiser, limiting to 2 pages
# ads_by_advertiser <- li_query(
#   advertiser = "Google",
#   countries = c("gb"),
#   start_date = "2024-05-01",
#   end_date = "2024-06-01",
#   max_pages = 2
# )
#' }
li_query <- function(keyword = NULL,
                              advertiser = NULL,
                              countries = NULL,
                              start_date = NULL,
                              end_date = NULL,
                              count = 25,
                              max_pages = Inf,
                              linkedin_api_version = "202409",
                              verbose = TRUE) {

  # Handle edge case where max_pages is 0
  if (max_pages <= 0) {
    if (verbose) usethis::ui_info("max_pages is 0 or negative, returning empty result.")
    return(tibble::tibble(
      ad_url = character(0),
      is_restricted = logical(0),
      restriction_details = character(0),
      advertiser_name = character(0),
      advertiser_url = character(0),
      ad_payer = character(0),
      ad_type = character(0),
      first_impression_at = .POSIXct(numeric(0), tz = "UTC"),
      latest_impression_at = .POSIXct(numeric(0), tz = "UTC"),
      total_impressions_from = integer(0),
      total_impressions_to = integer(0),
      impressions_by_country = list(),
      ad_targeting = list()
    ))
  }

  token <- li_get_token()
  if (is.null(token)) {
    stop("Authentication required. Please run `li_auth()` first.")
  }
  
  # Extract the access token from the OAuth token object
  # The cached token is a list, so we need to get the first element
  if (is.list(token) && length(token) > 0) {
    oauth_token <- token[[1]]
    access_token <- oauth_token$credentials$access_token
  } else {
    access_token <- token$credentials$access_token
  }
  
  if (is.null(access_token) || access_token == "") {
    stop("Invalid access token. Please re-authenticate with `li_auth()`.")
  }

  if (count > 25) {
    usethis::ui_info("`count` cannot exceed 25. Setting to 25.")
    count <- 25
  }

  base_url <- "https://api.linkedin.com/rest/adLibrary"
  all_ads_data <- list()
  current_start <- 0
  page <- 1

  # --- Main Pagination Loop ---
  repeat {
    if (verbose) usethis::ui_info("Retrieving page {page} (starting at index {current_start})...")

    # Build the 'q=criteria' part of the query
    criteria_params <- list(
      keyword = keyword,
      advertiser = advertiser,
      countries = build_countries_param(countries),
      dateRange = build_daterange_param(start_date, end_date)
    )
    # Remove NULL elements
    criteria_params <- purrr::compact(criteria_params)

    # URL encode only the parameter values that need it (like keyword with spaces)
    criteria_params <- lapply(names(criteria_params), function(name) {
      value <- criteria_params[[name]]
      if (name == "keyword" || name == "advertiser") {
        # URL encode simple text parameters
        value <- urltools::url_encode(value)
      }
      # Countries and dateRange are already properly formatted with URL encoding
      return(paste0(name, "=", value))
    })

    # Format for the URL
    criteria_string <- paste(criteria_params, collapse = "&")

    # Build the full URL with both criteria and paging parameters
    full_url <- paste0(base_url, "?q=criteria&", criteria_string, "&start=", current_start, "&count=", count)

    # Make the API request
    response <- tryCatch({
      httr::GET(
        url = full_url,
        httr::add_headers(
          'Authorization' = paste("Bearer", access_token),
          'X-RestLi-Protocol-Version' = '2.0.0',
          'Linkedin-Version' = linkedin_api_version
        )
      )
    }, error = function(e) {
      usethis::ui_oops("API request failed: {e$message}")
      return(NULL)
    })

    if (is.null(response)) break

    # Check for HTTP errors
    if (httr::http_error(response)) {
      usethis::ui_oops("API Error: {httr::status_code(response)}")
      usethis::ui_oops("Response content: {httr::content(response, 'text', encoding = 'UTF-8')}")
      break
    }

    content <- httr::content(response, "parsed", "application/json", encoding = "UTF-8")
    elements <- content$elements

    if (is.null(elements) || length(elements) == 0) {
      if (verbose) usethis::ui_done("No more results found.")
      break
    }

    if (verbose) usethis::ui_done("Retrieved {length(elements)} ads.")

    # Process and flatten the response
    page_data <- purrr::map_df(elements, function(el) {
      # Safely handle impressions by country data
      impressions_data <- tryCatch({
        if (!is.null(el$details$adStatistics$impressionsDistributionByCountry) && 
            length(el$details$adStatistics$impressionsDistributionByCountry) > 0) {
          tibble::as_tibble(el$details$adStatistics$impressionsDistributionByCountry)
        } else {
          tibble::tibble()
        }
      }, error = function(e) tibble::tibble())
      
      # Safely handle ad targeting data
      targeting_data <- tryCatch({
        if (!is.null(el$details$adTargeting) && length(el$details$adTargeting) > 0) {
          # Convert list to tibble with proper column names
          if (is.list(el$details$adTargeting) && !is.data.frame(el$details$adTargeting)) {
            # Handle case where adTargeting is a list of objects
            purrr::map_df(el$details$adTargeting, ~ {
              tibble::tibble(
                facetName = .x$facetName %||% NA_character_,
                isInclusive = .x$isInclusive %||% NA,
                inclusiveSegments = list(.x$inclusiveSegments %||% character(0)),
                isExclusive = .x$isExclusive %||% NA,
                exclusiveSegments = list(.x$exclusiveSegments %||% character(0))
              )
            })
          } else {
            tibble::as_tibble(el$details$adTargeting)
          }
        } else {
          tibble::tibble()
        }
      }, error = function(e) tibble::tibble())
      
      tibble::tibble(
        ad_url = el$adUrl %||% NA_character_,
        is_restricted = el$isRestricted %||% FALSE,
        restriction_details = el$restrictionDetails %||% NA_character_,
        advertiser_name = el$details$advertiser$advertiserName %||% NA_character_,
        advertiser_url = el$details$advertiser$advertiserUrl %||% NA_character_,
        ad_payer = el$details$advertiser$adPayer %||% NA_character_,
        ad_type = el$details$type %||% NA_character_,
        first_impression_at = if (!is.null(el$details$adStatistics$firstImpressionAt)) {
          .POSIXct(el$details$adStatistics$firstImpressionAt / 1000, tz = "UTC")
        } else NA,
        latest_impression_at = if (!is.null(el$details$adStatistics$latestImpressionAt)) {
          .POSIXct(el$details$adStatistics$latestImpressionAt / 1000, tz = "UTC")
        } else NA,
        total_impressions_from = el$details$adStatistics$totalImpressions$from %||% NA_integer_,
        total_impressions_to = el$details$adStatistics$totalImpressions$to %||% NA_integer_,
        impressions_by_country = list(impressions_data),
        ad_targeting = list(targeting_data)
      )
    })

    all_ads_data[[page]] <- page_data

    # Check termination conditions
    if (length(elements) < count || page >= max_pages) {
      if (verbose && page >= max_pages) usethis::ui_done("Reached `max_pages` limit.")
      break
    }

    # Prepare for the next page
    page <- page + 1
    current_start <- current_start + count
  }

  # Combine all pages into a single tibble
  final_df <- dplyr::bind_rows(all_ads_data)

  if (verbose) usethis::ui_done("Total ads retrieved: {nrow(final_df)}")

  return(final_df)
}
