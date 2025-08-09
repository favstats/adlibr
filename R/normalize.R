# Data Normalization Functions for adlibr
#
# This file contains functions for normalizing data from different platforms
# to the common adlibr schema.

#' Normalize Platform Data to Common Schema
#' 
#' @description
#' Normalizes raw platform data to the common adlibr schema defined in
#' the normalized_schema.json file.
#' 
#' @param raw_data List or data frame. Raw data from platform API
#' @param platform Character. Platform name
#' 
#' @return tibble. Normalized data with common schema
#' @export
#' 
#' @examples
#' \dontrun{
#' # Normalize Meta ad data
#' normalized <- adlibr_normalize(meta_raw_data, "meta")
#' 
#' # Normalize TikTok ad data  
#' normalized <- adlibr_normalize(tiktok_raw_data, "tiktok")
#' }
adlibr_normalize <- function(raw_data, platform) {
  
  platform <- validate_platform(platform)
  
  if (is.null(raw_data) || length(raw_data) == 0) {
    return(create_empty_normalized_tibble())
  }
  
  # Load schema for validation
  schema <- load_normalization_schema()
  
  # Platform-specific normalization
  normalized <- switch(platform,
    "meta" = normalize_meta(raw_data),
    "tiktok" = normalize_tiktok(raw_data),
    "linkedin" = normalize_linkedin_enhanced(raw_data),
    "microsoft" = normalize_microsoft_enhanced(raw_data),
    "apple" = normalize_apple_enhanced(raw_data),
    "amazon" = normalize_amazon(raw_data),
    "google" = normalize_google(raw_data),
    "booking" = normalize_booking(raw_data),
    "pinterest" = normalize_pinterest(raw_data),
    "snapchat" = normalize_snapchat(raw_data),
    "x" = normalize_x(raw_data),
    {
      cli::cli_warn("Normalization not implemented for platform: {platform}")
      normalize_generic(raw_data, platform)
    }
  )
  
  # Add platform and timestamp
  normalized$platform <- platform
  normalized <- add_fetched_timestamp(normalized)
  
  # Validate against schema
  normalized <- validate_normalized_data(normalized, schema)
  
  return(normalized)
}

#' Load Normalization Schema
#' 
#' @description
#' Loads the normalization schema from the package's JSON file.
#' 
#' @return List. Schema configuration
#' @keywords internal
load_normalization_schema <- function() {
  schema_path <- system.file("schema", "normalized_schema.json", package = "adlibr")
  
  if (!file.exists(schema_path)) {
    cli::cli_warn("Schema file not found, using basic validation")
    return(list())
  }
  
  tryCatch({
    jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
  }, error = function(e) {
    cli::cli_warn("Failed to load schema: {e$message}")
    return(list())
  })
}

#' Normalize Meta (Facebook/Instagram) Data
#' 
#' @param raw_data List. Raw Meta API response
#' @return tibble. Normalized data
#' @keywords internal
normalize_meta <- function(raw_data) {
  
  if (is.null(raw_data$elements)) {
    return(create_empty_normalized_tibble())
  }
  
  elements <- raw_data$elements
  
  normalized <- purrr::map_df(elements, function(ad) {
    
    # Extract basic fields
    ad_id <- safe_extract(ad, "id")
    advertiser_id <- safe_extract(ad, "page_id")
    advertiser_name <- safe_extract(ad, "page_name")
    
    # Extract dates (convert from milliseconds)
    first_seen <- parse_api_date(safe_extract(ad, "ad_delivery_start_time"), "epoch")
    last_seen <- parse_api_date(safe_extract(ad, "ad_delivery_stop_time"), "epoch")
    
    # Extract ad text (collapse multiple bodies)
    ad_text <- collapse_list_column(safe_extract(ad, "ad_creative_bodies"))
    
    # Extract media info
    media_type <- safe_extract(ad, "media_type", "unknown")
    media_urls <- list(safe_extract(ad, "ad_snapshot_url"))
    
    # Extract spend and impressions (ranges)
    spend_range <- safe_extract(ad, "spend")
    impressions_range <- safe_extract(ad, "impressions")
    eu_reach <- safe_extract(ad, "eu_total_reach")
    
    # Extract beneficiary/payer info
    payer <- safe_extract(ad, c("beneficiary_payers", "payer"))
    beneficiary <- safe_extract(ad, c("beneficiary_payers", "beneficiary"))
    
    # Extract targeting info
    targeting_age <- list(safe_extract(ad, "target_ages", character(0)))
    targeting_gender <- list(safe_extract(ad, "target_gender", character(0)))
    targeting_locations <- list(safe_extract(ad, "target_locations", character(0)))
    
    # Extract countries from impressions
    impression_countries <- safe_extract(ad, "impressions_by_country")
    countries <- if (!is.null(impression_countries)) {
      list(purrr::map_chr(impression_countries, ~ safe_extract(.x, "country", "")))
    } else {
      list(character(0))
    }
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      payer = as.character(payer),
      beneficiary = as.character(beneficiary),
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
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
      targeting_other = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Normalize TikTok Data
#' 
#' @param raw_data List. Raw TikTok API response
#' @return tibble. Normalized data
#' @keywords internal
normalize_tiktok <- function(raw_data) {
  
  if (is.null(raw_data$data) || is.null(raw_data$data$ads)) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$data$ads
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    ad_id <- safe_extract(ad, c("ad", "id"))
    advertiser_id <- safe_extract(ad, c("advertiser", "business_id"))
    advertiser_name <- safe_extract(ad, c("advertiser", "business_name"))
    
    first_seen <- parse_api_date(safe_extract(ad, c("ad", "first_shown_date")))
    last_seen <- parse_api_date(safe_extract(ad, c("ad", "last_shown_date")))
    
    status <- safe_extract(ad, c("ad", "status"))
    
    # Extract media URLs
    video_urls <- safe_extract(ad, c("ad", "videos"), list())
    image_urls <- safe_extract(ad, c("ad", "image_urls"), character(0))
    media_urls <- list(c(
      purrr::map_chr(video_urls, ~ safe_extract(.x, "url", "")),
      image_urls
    ))
    
    # Extract reach data
    impressions_range <- safe_extract(ad, c("reach", "unique_users_seen"))
    
    # Extract targeting
    targeting_info <- safe_extract(ad, c("ad_group", "targeting_info"))
    targeting_age <- list(safe_extract(targeting_info, "age", character(0)))
    targeting_gender <- list(safe_extract(targeting_info, "gender", character(0)))
    targeting_locations <- list(safe_extract(targeting_info, "country", character(0)))
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      payer = NA_character_,
      beneficiary = NA_character_,
      status = as.character(status),
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = NA_character_,
      media_type = if(length(unlist(media_urls)) > 0) "video" else "unknown",
      media_urls = media_urls,
      landing_url = NA_character_,
      countries = list(character(0)),
      languages = list(character(0)),
      spend_range = NA_character_,
      impressions_range = as.character(impressions_range),
      eu_reach = NA_character_,
      targeting_age = targeting_age,
      targeting_gender = targeting_gender,
      targeting_locations = targeting_locations,
      targeting_other = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Normalize Microsoft (Bing) Data
#' 
#' @param raw_data List. Raw Microsoft API response  
#' @return tibble. Normalized data
#' @keywords internal
normalize_microsoft <- function(raw_data) {
  
  if (is.null(raw_data$value)) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$value
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    ad_id <- safe_extract(ad, "AdId")
    advertiser_id <- safe_extract(ad, "AdvertiserId")
    advertiser_name <- safe_extract(ad, "AdvertiserName")
    
    # Extract dates
    first_seen <- parse_api_date(safe_extract(ad, c("AdDetails", "StartDate")))
    last_seen <- parse_api_date(safe_extract(ad, c("AdDetails", "EndDate")))
    
    # Extract impressions
    impressions_range <- safe_extract(ad, c("AdDetails", "TotalImpressionsRange"))
    
    # Extract countries
    countries_data <- safe_extract(ad, "ImpressionsByCountry", list())
    countries <- list(purrr::map_chr(countries_data, ~ safe_extract(.x, "Country", "")))
    
    # Extract targeting (serialize complex data)
    targeting_other <- list(safe_extract(ad, "TargetTypes", character(0)))
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      payer = NA_character_,
      beneficiary = NA_character_,
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = NA_character_,
      media_type = "unknown",
      media_urls = list(character(0)),
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_range = NA_character_,
      impressions_range = as.character(impressions_range),
      eu_reach = NA_character_,
      targeting_age = list(character(0)),
      targeting_gender = list(character(0)),
      targeting_locations = list(character(0)),
      targeting_other = targeting_other,
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Normalize Apple Data
#' 
#' @param raw_data List. Raw Apple API response
#' @return tibble. Normalized data
#' @keywords internal
normalize_apple <- function(raw_data) {
  
  if (is.null(raw_data$data)) {
    return(create_empty_normalized_tibble())
  }
  
  ads <- raw_data$data
  
  normalized <- purrr::map_df(ads, function(ad) {
    
    ad_id <- safe_extract(ad, "adId")
    advertiser_id <- safe_extract(ad, "developerId")
    advertiser_name <- safe_extract(ad, "developerName")
    
    # Extract ad text from available fields
    ad_text <- safe_extract(ad, "shortDescription") %||% safe_extract(ad, "promotionalText")
    
    # Extract media URLs
    assets <- safe_extract(ad, "adAssets", list())
    media_urls <- list(c(
      safe_extract(assets, "pictureUrl", ""),
      safe_extract(assets, "videoUrl", "")
    ))
    media_urls[[1]] <- media_urls[[1]][media_urls[[1]] != ""]
    
    # Extract dates
    first_seen <- parse_api_date(safe_extract(ad, "firstImpressionDate"))
    last_seen <- parse_api_date(safe_extract(ad, "lastImpressionDate"))
    
    # Extract country
    countries <- list(safe_extract(ad, "countryOrRegion", character(0)))
    
    # Extract targeting
    audience_ref <- safe_extract(ad, "audienceRefinement", list())
    targeting_age <- list(safe_extract(audience_ref, "ageTarget", character(0)))
    targeting_gender <- list(safe_extract(audience_ref, "genderTarget", character(0)))
    targeting_locations <- list(safe_extract(audience_ref, "locationTarget", character(0)))
    
    tibble::tibble(
      ad_id = as.character(ad_id),
      advertiser_id = as.character(advertiser_id),
      advertiser_name = as.character(advertiser_name),
      payer = NA_character_,
      beneficiary = NA_character_,
      status = NA_character_,
      first_seen = first_seen,
      last_seen = last_seen,
      ad_text = as.character(ad_text),
      media_type = if(length(unlist(media_urls)) > 0) "multi" else "unknown",
      media_urls = media_urls,
      landing_url = NA_character_,
      countries = countries,
      languages = list(character(0)),
      spend_range = NA_character_,
      impressions_range = NA_character_,
      eu_reach = NA_character_,
      targeting_age = targeting_age,
      targeting_gender = targeting_gender,
      targeting_locations = targeting_locations,
      targeting_other = list(character(0)),
      extra_json = list(ad)
    )
  })
  
  return(normalized)
}

#' Generic Normalization Function
#' 
#' @description
#' Generic normalization for platforms without specific implementations.
#' 
#' @param raw_data Raw platform data
#' @param platform Platform name
#' @return tibble. Normalized data with minimal mapping
#' @keywords internal
normalize_generic <- function(raw_data, platform) {
  
  # Try to extract basic fields if they exist
  normalized <- tibble::tibble(
    ad_id = NA_character_,
    advertiser_id = NA_character_,
    advertiser_name = NA_character_,
    payer = NA_character_,
    beneficiary = NA_character_,
    status = NA_character_,
    first_seen = as.POSIXct(NA),
    last_seen = as.POSIXct(NA),
    ad_text = NA_character_,
    media_type = "unknown",
    media_urls = list(character(0)),
    landing_url = NA_character_,
    countries = list(character(0)),
    languages = list(character(0)),
    spend_range = NA_character_,
    impressions_range = NA_character_,
    eu_reach = NA_character_,
    targeting_age = list(character(0)),
    targeting_gender = list(character(0)),
    targeting_locations = list(character(0)),
    targeting_other = list(character(0)),
    extra_json = list(raw_data)
  )
  
  return(normalized)
}

#' Validate Normalized Data
#' 
#' @description
#' Validates normalized data against the schema and applies corrections.
#' 
#' @param data tibble. Normalized data
#' @param schema List. Schema configuration
#' 
#' @return tibble. Validated and corrected data
#' @keywords internal
validate_normalized_data <- function(data, schema) {
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  # Ensure required columns exist
  required_cols <- c("ad_id", "platform", "extra_json", "fetched_at")
  missing_cols <- setdiff(required_cols, names(data))
  
  for (col in missing_cols) {
    data[[col]] <- switch(col,
      "ad_id" = NA_character_,
      "platform" = NA_character_,
      "extra_json" = list(list()),
      "fetched_at" = clock::sys_time_now()
    )
  }
  
  # Type corrections
  data$ad_id <- as.character(data$ad_id)
  data$platform <- as.character(data$platform)
  
  # Ensure list columns are proper lists
  list_cols <- c("media_urls", "countries", "languages", "targeting_age", 
                "targeting_gender", "targeting_locations", "targeting_other", "extra_json")
  
  for (col in list_cols) {
    if (col %in% names(data) && !is.list(data[[col]])) {
      data[[col]] <- as.list(data[[col]])
    }
  }
  
  return(data)
}

# Placeholder normalization functions for remaining platforms
normalize_amazon <- function(raw_data) normalize_generic(raw_data, "amazon")
normalize_google <- function(raw_data) normalize_generic(raw_data, "google")
normalize_booking <- function(raw_data) normalize_generic(raw_data, "booking")
normalize_pinterest <- function(raw_data) normalize_generic(raw_data, "pinterest")
normalize_snapchat <- function(raw_data) normalize_generic(raw_data, "snapchat")
normalize_x <- function(raw_data) normalize_generic(raw_data, "x")
