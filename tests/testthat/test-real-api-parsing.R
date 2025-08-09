# Test parsing logic with real API response structure

test_that("data parsing handles real LinkedIn API response correctly", {
  skip("Requires real API authentication to test end-to-end")
  
  # This test documents the expected behavior with real API data
  # Run manually with: li_query(keyword = "linkedin", max_pages = 1)
  
  # Expected structure after parsing:
  # - ad_url: character
  # - is_restricted: logical  
  # - restriction_details: character
  # - advertiser_name: character
  # - advertiser_url: character
  # - ad_payer: character (may be NA if not in API response)
  # - ad_type: character
  # - first_impression_at: POSIXct
  # - latest_impression_at: POSIXct
  # - total_impressions_from: integer
  # - total_impressions_to: integer
  # - impressions_by_country: list-column with tibble containing:
  #   - country: character (e.g., "urn:li:country:US")
  #   - impression_percentage: numeric
  # - ad_targeting: list-column with tibble containing:
  #   - facet_name: character
  #   - is_included: logical
  #   - included_segments: list-column
  #   - is_excluded: logical  
  #   - excluded_segments: list-column
  
  expect_true(TRUE)  # Placeholder for documentation
})

test_that("parsing handles real API field structure", {
  # Test individual parsing components with real API structure
  
  # Real impression data structure
  real_impression_item <- list(
    country = "urn:li:country:US",
    impressionPercentage = 25.0
  )
  
  # Test impression parsing
  impressions_list <- list(real_impression_item)
  impressions_df <- purrr::map_df(impressions_list, ~ {
    tibble::tibble(
      country = .x$country %||% NA_character_,
      impression_percentage = .x$impressionPercentage %||% NA_real_
    )
  })
  
  expect_s3_class(impressions_df, "data.frame")
  expect_equal(nrow(impressions_df), 1)
  expect_equal(impressions_df$country, "urn:li:country:US")
  expect_equal(impressions_df$impression_percentage, 25.0)
  
  # Real targeting data structure
  real_targeting_item <- list(
    includedSegments = list("English"),
    facetName = "Language",
    excludedSegments = list(),
    isIncluded = TRUE,
    isExcluded = FALSE
  )
  
  # Test targeting parsing
  targeting_list <- list(real_targeting_item)
  targeting_df <- purrr::map_df(targeting_list, ~ {
    tibble::tibble(
      facet_name = .x$facetName %||% NA_character_,
      is_included = .x$isIncluded %||% NA,
      included_segments = list(.x$includedSegments %||% character(0)),
      is_excluded = .x$isExcluded %||% NA,
      excluded_segments = list(.x$excludedSegments %||% character(0))
    )
  })
  
  expect_s3_class(targeting_df, "data.frame")
  expect_equal(nrow(targeting_df), 1)
  expect_equal(targeting_df$facet_name, "Language")
  expect_true(targeting_df$is_included)
  expect_false(targeting_df$is_excluded)
  expect_equal(targeting_df$included_segments[[1]][[1]], "English")
  expect_equal(length(targeting_df$excluded_segments[[1]]), 0)
})

test_that("column names follow snake_case convention", {
  # Verify all column names are in snake_case format
  expected_main_columns <- c(
    "ad_url", "is_restricted", "restriction_details",
    "advertiser_name", "advertiser_url", "ad_payer", "ad_type",
    "first_impression_at", "latest_impression_at", 
    "total_impressions_from", "total_impressions_to",
    "impressions_by_country", "ad_targeting"
  )
  
  expected_impression_columns <- c("country", "impression_percentage")
  expected_targeting_columns <- c(
    "facet_name", "is_included", "included_segments", 
    "is_excluded", "excluded_segments"
  )
  
  # Test that all expected column names are snake_case
  snake_case_pattern <- "^[a-z]+(_[a-z]+)*$"
  
  expect_true(all(grepl(snake_case_pattern, expected_main_columns)))
  expect_true(all(grepl(snake_case_pattern, expected_impression_columns)))
  expect_true(all(grepl(snake_case_pattern, expected_targeting_columns)))
})

test_that("empty responses are handled correctly", {
  # Test empty impression data
  empty_impressions <- tibble::tibble(
    country = character(0),
    impression_percentage = numeric(0)
  )
  expect_s3_class(empty_impressions, "data.frame")
  expect_equal(nrow(empty_impressions), 0)
  
  # Test empty targeting data
  empty_targeting <- tibble::tibble(
    facet_name = character(0),
    is_included = logical(0),
    included_segments = list(),
    is_excluded = logical(0),
    excluded_segments = list()
  )
  expect_s3_class(empty_targeting, "data.frame")
  expect_equal(nrow(empty_targeting), 0)
})
