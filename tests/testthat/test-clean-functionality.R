# Tests for clean parameter functionality

test_that("li_query validates clean and direction parameters", {
  # Mock token setup
  mock_token <- structure(
    list(credentials = list(access_token = "valid_token")),
    class = "Token2.0"
  )
  
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  saveRDS(mock_token, ".httr-oauth")
  
  # Test invalid clean parameter
  expect_error(
    li_query(keyword = "test", clean = "yes"),
    "Parameter 'clean' must be TRUE or FALSE"
  )
  
  # Test invalid direction parameter
  expect_error(
    li_query(keyword = "test", direction = "invalid"),
    "Parameter 'direction' must be 'wide' or 'long'"
  )
  
  # Test valid parameters don't error (with empty result)
  expect_error(
    li_query(keyword = "test", clean = TRUE, direction = "wide", max_pages = 0),
    NA
  )
  
  expect_error(
    li_query(keyword = "test", clean = TRUE, direction = "long", max_pages = 0),
    NA
  )
})

test_that("clean_liads_data handles empty data correctly", {
  # Test with empty tibble
  empty_data <- tibble::tibble(
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
  )
  
  result_wide <- clean_liads_data(empty_data, direction = "wide")
  result_long <- clean_liads_data(empty_data, direction = "long")
  
  expect_s3_class(result_wide, "data.frame")
  expect_s3_class(result_long, "data.frame")
  expect_equal(nrow(result_wide), 0)
  expect_equal(nrow(result_long), 0)
})

test_that("clean_liads_data processes targeting data correctly", {
  # Create sample data with targeting information
  sample_data <- tibble::tibble(
    ad_url = c("https://example.com/ad1", "https://example.com/ad2"),
    is_restricted = c(FALSE, FALSE),
    restriction_details = c(NA_character_, NA_character_),
    advertiser_name = c("Test Advertiser 1", "Test Advertiser 2"),
    advertiser_url = c("https://company1.com", "https://company2.com"),
    ad_payer = c("Payer1", "Payer2"),
    ad_type = c("SPONSORED", "SPONSORED"),
    first_impression_at = rep(.POSIXct(1234567890, tz = "UTC"), 2),
    latest_impression_at = rep(.POSIXct(1234567890, tz = "UTC"), 2),
    total_impressions_from = c(0L, 100L),
    total_impressions_to = c(1000L, 5000L),
    impressions_by_country = list(
      tibble::tibble(country = "urn:li:country:US", impression_percentage = 75.0),
      tibble::tibble(country = character(0), impression_percentage = numeric(0))
    ),
    ad_targeting = list(
      tibble::tibble(
        facet_name = c("Language", "Location"),
        is_included = c(TRUE, TRUE),
        included_segments = list(c("English"), c("United States", "Canada")),
        is_excluded = c(FALSE, FALSE),
        excluded_segments = list(character(0), character(0))
      ),
      tibble::tibble(
        facet_name = character(0),
        is_included = logical(0),
        included_segments = list(),
        is_excluded = logical(0),
        excluded_segments = list()
      )
    )
  )
  
  # Test wide format
  result_wide <- clean_liads_data(sample_data, direction = "wide")
  
  expect_s3_class(result_wide, "data.frame")
  expect_true("included_segments_text" %in% names(result_wide))
  expect_true("excluded_segments_text" %in% names(result_wide))
  expect_true("country" %in% names(result_wide))
  expect_true("impression_percentage" %in% names(result_wide))
  
  # Check that text conversion worked
  targeting_rows <- result_wide[!is.na(result_wide$facet_name), ]
  if (nrow(targeting_rows) > 0) {
    expect_true(any(grepl("English", targeting_rows$included_segments_text, fixed = TRUE)))
    expect_true(any(grepl("United States, Canada", targeting_rows$included_segments_text, fixed = TRUE)))
  }
  
  # Test long format
  result_long <- clean_liads_data(sample_data, direction = "long")
  
  expect_s3_class(result_long, "data.frame")
  expect_true("data_type" %in% names(result_long))
  expect_true("category" %in% names(result_long))
  expect_true("value" %in% names(result_long))
  
  # Check data types are present
  data_types <- unique(result_long$data_type)
  data_types <- data_types[!is.na(data_types)]
  expect_true(length(data_types) > 0)
})

test_that("clean_liads_data handles missing targeting/impression data gracefully", {
  # Create sample data with no targeting or impression data
  sample_data <- tibble::tibble(
    ad_url = "https://example.com/ad1",
    is_restricted = FALSE,
    restriction_details = NA_character_,
    advertiser_name = "Test Advertiser",
    advertiser_url = "https://company.com",
    ad_payer = "Payer",
    ad_type = "SPONSORED",
    first_impression_at = .POSIXct(1234567890, tz = "UTC"),
    latest_impression_at = .POSIXct(1234567890, tz = "UTC"),
    total_impressions_from = 0L,
    total_impressions_to = 1000L,
    impressions_by_country = list(tibble::tibble(
      country = character(0), 
      impression_percentage = numeric(0)
    )),
    ad_targeting = list(tibble::tibble(
      facet_name = character(0),
      is_included = logical(0),
      included_segments = list(),
      is_excluded = logical(0),
      excluded_segments = list()
    ))
  )
  
  result_wide <- clean_liads_data(sample_data, direction = "wide")
  result_long <- clean_liads_data(sample_data, direction = "long")
  
  expect_s3_class(result_wide, "data.frame")
  expect_s3_class(result_long, "data.frame")
  expect_equal(nrow(result_wide), 1)  # Should have the base ad data
  expect_equal(nrow(result_long), 1)  # Should have the base ad data
  
  # Check that base columns are preserved
  expect_equal(result_wide$ad_url, "https://example.com/ad1")
  expect_equal(result_long$ad_url, "https://example.com/ad1")
})

test_that("li_query with clean=TRUE parameter integration", {
  skip("Integration test - requires manual verification with real API")
  
  # This test documents the expected behavior when using clean=TRUE
  # To test manually:
  # result_raw <- li_query(keyword = "test", max_pages = 1, clean = FALSE)
  # result_clean_wide <- li_query(keyword = "test", max_pages = 1, clean = TRUE, direction = "wide")
  # result_clean_long <- li_query(keyword = "test", max_pages = 1, clean = TRUE, direction = "long")
  
  # Expected behavior:
  # - clean=FALSE: returns list-columns (ad_targeting, impressions_by_country)
  # - clean=TRUE, wide: no list-columns, targeting/impression data as separate columns
  # - clean=TRUE, long: no list-columns, targeting/impression data stacked with data_type indicator
  
  expect_true(TRUE)  # Placeholder
})
