# Tests for parameter building functions

test_that("build_countries_param handles NULL input", {
  expect_null(build_countries_param(NULL))
  expect_null(build_countries_param(character(0)))
})

test_that("build_countries_param formats single country correctly", {
  result <- build_countries_param("us")
  expected <- "(value:List(urn%3Ali%3Acountry%3AUS))"
  expect_equal(result, expected)
})

test_that("build_countries_param formats multiple countries correctly", {
  result <- build_countries_param(c("us", "gb", "de"))
  expected <- "(value:List(urn%3Ali%3Acountry%3AUS,urn%3Ali%3Acountry%3AGB,urn%3Ali%3Acountry%3ADE))"
  expect_equal(result, expected)
})

test_that("build_countries_param handles mixed case input", {
  result <- build_countries_param(c("us", "GB"))
  expected <- "(value:List(urn%3Ali%3Acountry%3AUS,urn%3Ali%3Acountry%3AGB))"
  expect_equal(result, expected)
})

test_that("build_daterange_param handles NULL inputs", {
  expect_null(build_daterange_param(NULL, NULL))
  expect_null(build_daterange_param("2024-01-01", NULL))
  expect_null(build_daterange_param(NULL, "2024-01-31"))
})

test_that("build_daterange_param formats dates correctly", {
  start_date <- as.Date("2024-01-15")
  end_date <- as.Date("2024-02-20")
  
  result <- build_daterange_param(start_date, end_date)
  expected <- "(start:(day:15,month:1,year:2024),end:(day:20,month:2,year:2024))"
  expect_equal(result, expected)
})

test_that("build_daterange_param handles string dates", {
  result <- build_daterange_param("2024-03-01", "2024-03-31")
  expected <- "(start:(day:1,month:3,year:2024),end:(day:31,month:3,year:2024))"
  expect_equal(result, expected)
})

test_that("build_daterange_param handles leap year correctly", {
  result <- build_daterange_param("2024-02-29", "2024-03-01")
  expected <- "(start:(day:29,month:2,year:2024),end:(day:1,month:3,year:2024))"
  expect_equal(result, expected)
})
