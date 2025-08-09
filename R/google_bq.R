# Google Ads Transparency (BigQuery) API
#
# This file contains platform-specific functions for accessing
# Google's Ads Transparency data via BigQuery public datasets.

#' Build Google BigQuery Request
#' 
#' @description
#' Builds a BigQuery SQL query for Google Ads Transparency data.
#' 
#' @param operation Character. Operation type ("search_ads", "advertisers")
#' @param params List. Request parameters
#' @param config List. Platform configuration
#' 
#' @return List. BigQuery request configuration
#' @keywords internal
build_google_bq_request <- function(operation, params, config) {
  
  # Check BigQuery authentication
  if (!check_google_bq_auth()) {
    cli::cli_abort("Google BigQuery authentication required. Set GOOGLE_APPLICATION_CREDENTIALS.")
  }
  
  # Build SQL query based on operation
  sql_query <- switch(operation,
    "search_ads" = build_google_ads_query(params, config),
    "advertisers" = build_google_advertisers_query(params, config),
    cli::cli_abort("Unknown Google BigQuery operation: {operation}")
  )
  
  return(list(
    type = "bigquery",
    query = sql_query,
    dataset = determine_google_dataset(params),
    billing_project = Sys.getenv("GOOGLE_CLOUD_PROJECT", ""),
    params = params
  ))
}

#' Check Google BigQuery Authentication
#' 
#' @description
#' Checks if Google BigQuery authentication is properly configured.
#' 
#' @return Logical. TRUE if authentication is available
#' @keywords internal
check_google_bq_auth <- function() {
  
  # Check for service account credentials
  creds_path <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")
  if (nchar(creds_path) > 0 && file.exists(creds_path)) {
    return(TRUE)
  }
  
  # Check for other auth methods (user auth, etc.)
  if (requireNamespace("bigrquery", quietly = TRUE)) {
    tryCatch({
      # Try to authenticate
      bigrquery::bq_auth()
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  return(FALSE)
}

#' Determine Google Dataset
#' 
#' @description
#' Determines which Google dataset to query based on parameters.
#' 
#' @param params List. Query parameters
#' 
#' @return Character. Dataset identifier
#' @keywords internal
determine_google_dataset <- function(params) {
  
  # Default to political ads dataset
  # Could be extended to check for other datasets based on params
  return("bigquery-public-data.google_political_ads")
}

#' Build Google Ads Query
#' 
#' @description
#' Builds SQL query for Google political ads data.
#' 
#' @param params List. Query parameters
#' @param config List. Platform configuration
#' 
#' @return Character. SQL query string
#' @keywords internal
build_google_ads_query <- function(params, config) {
  
  # Base query for creative stats
  base_query <- "
    SELECT 
      ad_id,
      ad_url,
      ad_text,
      advertiser_id,
      advertiser_name,
      date_range_start,
      date_range_end,
      regions,
      age_targeting,
      gender_targeting,
      spend_range_min_usd,
      spend_range_max_usd,
      impressions_min,
      impressions_max
    FROM `bigquery-public-data.google_political_ads.creative_stats`
  "
  
  # Build WHERE conditions
  conditions <- character(0)
  
  # Keyword search in ad text
  if (!is.null(params$keyword)) {
    keyword_condition <- sprintf(
      "LOWER(ad_text) LIKE LOWER('%%%s%%')",
      gsub("'", "''", params$keyword)  # Escape single quotes
    )
    conditions <- c(conditions, keyword_condition)
  }
  
  # Advertiser search
  if (!is.null(params$advertisers)) {
    advertiser_condition <- sprintf(
      "LOWER(advertiser_name) LIKE LOWER('%%%s%%')",
      gsub("'", "''", params$advertisers[1])
    )
    conditions <- c(conditions, advertiser_condition)
  }
  
  # Date range filtering
  if (!is.null(params$date_from)) {
    date_from_condition <- sprintf(
      "date_range_start >= '%s'",
      format_date_for_api(params$date_from, "iso")
    )
    conditions <- c(conditions, date_from_condition)
  }
  
  if (!is.null(params$date_to)) {
    date_to_condition <- sprintf(
      "date_range_end <= '%s'",
      format_date_for_api(params$date_to, "iso")
    )
    conditions <- c(conditions, date_to_condition)
  }
  
  # Countries/regions filtering
  if (!is.null(params$countries)) {
    countries <- normalize_country_codes(params$countries)
    countries_list <- paste0("'", countries, "'", collapse = ",")
    
    # Google uses regions array - check if any of our countries are in the regions
    regions_condition <- sprintf(
      "EXISTS (SELECT 1 FROM UNNEST(regions) AS region WHERE region IN (%s))",
      countries_list
    )
    conditions <- c(conditions, regions_condition)
  }
  
  # Combine conditions
  if (length(conditions) > 0) {
    where_clause <- paste("WHERE", paste(conditions, collapse = " AND "))
    base_query <- paste(base_query, where_clause)
  }
  
  # Add ordering and limit
  base_query <- paste(base_query, "ORDER BY date_range_start DESC")
  
  if (!is.null(params$limit)) {
    limit_clause <- sprintf("LIMIT %d", min(as.integer(params$limit), 10000))
    base_query <- paste(base_query, limit_clause)
  } else {
    base_query <- paste(base_query, "LIMIT 1000")
  }
  
  return(base_query)
}

#' Build Google Advertisers Query
#' 
#' @description
#' Builds SQL query for Google advertiser information.
#' 
#' @param params List. Query parameters
#' @param config List. Platform configuration
#' 
#' @return Character. SQL query string
#' @keywords internal
build_google_advertisers_query <- function(params, config) {
  
  # Query for unique advertisers
  base_query <- "
    SELECT DISTINCT
      advertiser_id,
      advertiser_name,
      COUNT(*) as total_ads,
      MIN(date_range_start) as first_ad_date,
      MAX(date_range_end) as last_ad_date
    FROM `bigquery-public-data.google_political_ads.creative_stats`
  "
  
  # Build WHERE conditions
  conditions <- character(0)
  
  # Advertiser name search
  if (!is.null(params$keyword)) {
    advertiser_condition <- sprintf(
      "LOWER(advertiser_name) LIKE LOWER('%%%s%%')",
      gsub("'", "''", params$keyword)
    )
    conditions <- c(conditions, advertiser_condition)
  }
  
  # Date range filtering
  if (!is.null(params$date_from)) {
    date_condition <- sprintf(
      "date_range_start >= '%s'",
      format_date_for_api(params$date_from, "iso")
    )
    conditions <- c(conditions, date_condition)
  }
  
  # Combine conditions
  if (length(conditions) > 0) {
    where_clause <- paste("WHERE", paste(conditions, collapse = " AND "))
    base_query <- paste(base_query, where_clause)
  }
  
  # Add grouping and ordering
  base_query <- paste(base_query, "
    GROUP BY advertiser_id, advertiser_name
    ORDER BY total_ads DESC
  ")
  
  # Add limit
  if (!is.null(params$limit)) {
    limit_clause <- sprintf("LIMIT %d", min(as.integer(params$limit), 1000))
    base_query <- paste(base_query, limit_clause)
  } else {
    base_query <- paste(base_query, "LIMIT 100")
  }
  
  return(base_query)
}

#' Execute Google BigQuery Request
#' 
#' @description
#' Executes BigQuery request and returns results.
#' 
#' @param req_config List. Request configuration from build_google_bq_request
#' 
#' @return tibble. Query results
#' @keywords internal
execute_google_bq_request <- function(req_config) {
  
  if (req_config$type != "bigquery") {
    cli::cli_abort("Invalid request configuration for Google BigQuery")
  }
  
  if (!requireNamespace("bigrquery", quietly = TRUE)) {
    cli::cli_abort("bigrquery package required for Google Ads Transparency data")
  }
  
  tryCatch({
    # Set billing project if provided
    billing_project <- req_config$billing_project
    if (nchar(billing_project) == 0) {
      billing_project <- NULL
    }
    
    # Execute query
    result <- bigrquery::bq_project_query(
      x = billing_project,
      query = req_config$query,
      quiet = TRUE
    )
    
    # Download results
    data <- bigrquery::bq_table_download(result, quiet = TRUE)
    
    return(data)
    
  }, error = function(e) {
    cli::cli_abort("Google BigQuery request failed: {e$message}")
  })
}

#' Normalize Google BigQuery Data to Common Schema
#' 
#' @description
#' Converts Google BigQuery political ads data to adlibr normalized schema.
#' 
#' @param raw_data tibble. Raw BigQuery results
#' 
#' @return tibble. Normalized data
#' @keywords internal
normalize_google_bq_enhanced <- function(raw_data) {
  
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  normalized <- raw_data %>%
    dplyr::mutate(
      # Basic identifiers
      ad_id = as.character(ad_id %||% paste0("google_", row_number())),
      platform = "google",
      advertiser_id = as.character(advertiser_id %||% NA_character_),
      advertiser_name = as.character(advertiser_name %||% NA_character_),
      advertiser_verified = NA,
      advertiser_type = "political",  # Google dataset is political ads
      payer = NA_character_,
      beneficiary = NA_character_,
      status = NA_character_,
      
      # Dates
      first_seen = parse_api_date(date_range_start %||% NA),
      last_seen = parse_api_date(date_range_end %||% NA),
      
      # Ad content
      ad_text = as.character(ad_text %||% NA_character_),
      ad_creative_urls = list(character(0)),
      ad_library_url = as.character(ad_url %||% NA_character_),
      
      # Media (Google doesn't provide media info in this dataset)
      media_type = "unknown",
      media_urls = list(character(0)),
      landing_url = NA_character_,
      
      # Geographic data
      countries = purrr::map(regions, ~ {
        if (!is.null(.x) && length(.x) > 0) {
          # Convert region names to country codes where possible
          convert_google_regions_to_countries(.x)
        } else {
          character(0)
        }
      }),
      languages = list(character(0)),
      
      # Financial data
      spend_range = case_when(
        !is.na(spend_range_min_usd) & !is.na(spend_range_max_usd) ~
          paste0("$", spend_range_min_usd, " - $", spend_range_max_usd),
        !is.na(spend_range_min_usd) ~ paste0("$", spend_range_min_usd, "+"),
        TRUE ~ NA_character_
      ),
      impressions_range = case_when(
        !is.na(impressions_min) & !is.na(impressions_max) ~
          paste0(format(impressions_min, big.mark = ","), " - ", format(impressions_max, big.mark = ",")),
        !is.na(impressions_min) ~ paste0(format(impressions_min, big.mark = ","), "+"),
        TRUE ~ NA_character_
      ),
      eu_reach = NA_character_,
      
      # Targeting data (from Google's structured fields)
      targeting_age = purrr::map(age_targeting, ~ {
        if (!is.null(.x) && length(.x) > 0) {
          as.character(.x)
        } else {
          character(0)
        }
      }),
      targeting_gender = purrr::map(gender_targeting, ~ {
        if (!is.null(.x) && length(.x) > 0) {
          as.character(.x)
        } else {
          character(0)
        }
      }),
      targeting_locations = purrr::map(regions, ~ {
        if (!is.null(.x) && length(.x) > 0) {
          as.character(.x)
        } else {
          character(0)
        }
      }),
      targeting_interests = list(character(0)),
      targeting_behaviors = list(character(0)),
      targeting_custom_audiences = list(character(0)),
      targeting_exclusions = list(character(0)),
      
      # Preserve original data
      extra_json = purrr::map(seq_len(n()), ~ {
        row_data <- slice(raw_data, .x)
        as.list(row_data)
      })
    ) %>%
    dplyr::select(
      ad_id, platform, advertiser_id, advertiser_name, advertiser_verified,
      advertiser_type, payer, beneficiary, status, first_seen, last_seen,
      ad_text, ad_creative_urls, ad_library_url, media_type, media_urls,
      landing_url, countries, languages, spend_range, impressions_range,
      eu_reach, targeting_age, targeting_gender, targeting_locations,
      targeting_interests, targeting_behaviors, targeting_custom_audiences,
      targeting_exclusions, extra_json
    )
  
  return(normalized)
}

#' Convert Google Regions to Country Codes
#' 
#' @description
#' Converts Google's region names to ISO country codes where possible.
#' 
#' @param regions Character vector. Google region names
#' 
#' @return Character vector. ISO country codes
#' @keywords internal
convert_google_regions_to_countries <- function(regions) {
  
  if (is.null(regions) || length(regions) == 0) {
    return(character(0))
  }
  
  # Simple mapping for common regions
  region_mapping <- list(
    "United States" = "US",
    "California" = "US",
    "New York" = "US",
    "Texas" = "US",
    "Florida" = "US",
    "United Kingdom" = "GB",
    "Canada" = "CA",
    "Australia" = "AU",
    "Germany" = "DE",
    "France" = "FR",
    "Italy" = "IT",
    "Spain" = "ES",
    "Netherlands" = "NL",
    "Belgium" = "BE",
    "Sweden" = "SE",
    "Denmark" = "DK",
    "Norway" = "NO",
    "Finland" = "FI",
    "Poland" = "PL",
    "Czech Republic" = "CZ",
    "Hungary" = "HU",
    "Romania" = "RO",
    "Bulgaria" = "BG",
    "Croatia" = "HR",
    "Slovenia" = "SI",
    "Slovakia" = "SK",
    "Lithuania" = "LT",
    "Latvia" = "LV",
    "Estonia" = "EE",
    "Ireland" = "IE",
    "Portugal" = "PT",
    "Greece" = "GR",
    "Cyprus" = "CY",
    "Malta" = "MT",
    "Luxembourg" = "LU"
  )
  
  # Convert regions to country codes
  country_codes <- character(0)
  for (region in regions) {
    if (region %in% names(region_mapping)) {
      country_codes <- c(country_codes, region_mapping[[region]])
    } else {
      # Keep original region name if no mapping found
      country_codes <- c(country_codes, region)
    }
  }
  
  return(unique(country_codes))
}

#' Search Google Advertisers via BigQuery
#' 
#' @description
#' Search for political advertisers in Google's BigQuery dataset.
#' 
#' @param search_term Character. Search term for advertiser names
#' @param limit Integer. Maximum number of results
#' 
#' @return tibble. Advertiser information
#' @keywords internal
search_google_advertisers <- function(search_term, limit = 100) {
  
  config <- get_platform_config("google")
  
  params <- list(
    keyword = search_term,
    limit = limit
  )
  
  req_config <- build_google_bq_request("advertisers", params, config)
  result <- execute_google_bq_request(req_config)
  
  # Add platform column
  if (nrow(result) > 0) {
    result$platform <- "google"
  }
  
  return(result)
}

#' Validate Google BigQuery Credentials
#' 
#' @description
#' Validates Google BigQuery authentication and access to public datasets.
#' 
#' @return List with validation results
#' @keywords internal
validate_google_bq_credentials <- function() {
  
  if (!requireNamespace("bigrquery", quietly = TRUE)) {
    return(list(
      valid = FALSE,
      message = "bigrquery package not installed",
      error = "missing_package"
    ))
  }
  
  # Check authentication
  if (!check_google_bq_auth()) {
    return(list(
      valid = FALSE,
      message = "Google BigQuery authentication not configured",
      error = "missing_auth"
    ))
  }
  
  # Test with simple query
  tryCatch({
    test_query <- "
      SELECT COUNT(*) as total_ads 
      FROM `bigquery-public-data.google_political_ads.creative_stats` 
      LIMIT 1
    "
    
    result <- bigrquery::bq_project_query(
      x = Sys.getenv("GOOGLE_CLOUD_PROJECT"),
      query = test_query,
      quiet = TRUE
    )
    
    data <- bigrquery::bq_table_download(result, quiet = TRUE)
    
    if (nrow(data) > 0) {
      return(list(
        valid = TRUE,
        message = "Google BigQuery access confirmed",
        total_ads_available = data$total_ads[1]
      ))
    } else {
      return(list(
        valid = FALSE,
        message = "Google BigQuery test query failed",
        error = "query_failed"
      ))
    }
    
  }, error = function(e) {
    return(list(
      valid = FALSE,
      message = paste("Google BigQuery validation failed:", e$message),
      error = "validation_error"
    ))
  })
}
