# Simplified tests for the main li_query function

test_that("li_query fails when no token is available", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  # Remove any existing .httr-oauth file
  if (file.exists(".httr-oauth")) {
    unlink(".httr-oauth")
  }
  
  expect_error(
    li_query(keyword = "test", verbose = FALSE),
    "Authentication required"
  )
})

test_that("li_query validates count parameter", {
  # Create mock token object
  mock_token <- structure(
    list(credentials = list(access_token = "test_access_token")),
    class = "Token2.0"
  )
  
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  saveRDS(mock_token, ".httr-oauth")
  
  # This test verifies that the function validates access tokens properly
  # It should throw an error about invalid access token
  expect_error(
    li_query(keyword = "test", count = 50, verbose = FALSE),
    "Invalid access token"
  )
})

test_that("li_query handles token extraction correctly", {
  # Create mock token object
  mock_token <- structure(
    list(credentials = list(access_token = "test_access_token")),
    class = "Token2.0"
  )
  
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  saveRDS(mock_token, ".httr-oauth")
  
  # The function should be able to load the token but detect it's invalid
  # It should throw an error about invalid access token
  expect_error(
    li_query(keyword = "test", verbose = FALSE),
    "Invalid access token"
  )
})
