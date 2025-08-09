# Simplified tests for error handling scenarios

test_that("build_countries_param handles edge cases", {
  # Test with empty vector
  expect_null(build_countries_param(character(0)))
  
  # Test with NULL
  expect_null(build_countries_param(NULL))
  
  # Test with empty string (should return NULL after trimming)
  expect_null(build_countries_param(""))
  
  # Test with whitespace only
  expect_null(build_countries_param("   "))
  
  # Test with mixed case
  result <- build_countries_param(c("US", "gb", "DE"))
  expect_true(grepl("US", result))
  expect_true(grepl("GB", result)) 
  expect_true(grepl("DE", result))
  
  # Test with valid input
  result <- build_countries_param("us")
  expected <- "(value:List(urn%3Ali%3Acountry%3AUS))"
  expect_equal(result, expected)
})

test_that("build_daterange_param handles invalid dates", {
  # Should handle invalid date strings
  expect_error(suppressWarnings(build_daterange_param("invalid-date", "2024-01-01")))
  expect_error(suppressWarnings(build_daterange_param("2024-01-01", "invalid-date")))
  
  # Should handle NULL values
  expect_null(build_daterange_param(NULL, "2024-01-01"))
  expect_null(build_daterange_param("2024-01-01", NULL))
  expect_null(build_daterange_param(NULL, NULL))
  
  # Should handle valid dates
  result <- build_daterange_param("2024-01-01", "2024-01-31")
  expected <- "(start:(day:1,month:1,year:2024),end:(day:31,month:1,year:2024))"
  expect_equal(result, expected)
})

test_that("build_daterange_param handles Date objects", {
  start_date <- as.Date("2024-01-01")
  end_date <- as.Date("2024-01-31")
  result <- build_daterange_param(start_date, end_date)
  expected <- "(start:(day:1,month:1,year:2024),end:(day:31,month:1,year:2024))"
  expect_equal(result, expected)
})

test_that("parameter functions return correct types", {
  # Countries function
  expect_null(build_countries_param(NULL))
  expect_type(build_countries_param("us"), "character")
  
  # Date range function  
  expect_null(build_daterange_param(NULL, NULL))
  expect_type(build_daterange_param("2024-01-01", "2024-01-31"), "character")
})
