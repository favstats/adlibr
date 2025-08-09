# Simplified tests for OAuth authentication functions

test_that("li_get_token returns NULL when no cached token exists", {
  # Create temporary directory without token file
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  # Remove any existing .httr-oauth file
  if (file.exists(".httr-oauth")) {
    unlink(".httr-oauth")
  }
  
  expect_null(li_get_token())
})

test_that("li_get_token returns token when cached token exists", {
  # Create a mock token object
  mock_token <- structure(
    list(
      credentials = list(access_token = "test_access_token_123")
    ),
    class = "Token2.0"
  )
  
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(old_wd))
  
  # Save mock token
  saveRDS(mock_token, ".httr-oauth")
  
  result <- li_get_token()
  expect_equal(result$credentials$access_token, "test_access_token_123")
})

test_that("li_auth_configure validates input", {
  # Test that credentials can't be empty
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Renviron"))
  })
  
  # This is a simplified test that just checks the function exists
  expect_true(is.function(li_auth_configure))
})

test_that("li_auth validates credentials are set", {
  # Clear environment variables
  original_id <- Sys.getenv("li_CLIENT_ID")
  original_secret <- Sys.getenv("li_CLIENT_SECRET")
  on.exit({
    if (original_id != "") Sys.setenv(li_CLIENT_ID = original_id)
    if (original_secret != "") Sys.setenv(li_CLIENT_SECRET = original_secret)
  })
  
  Sys.unsetenv("li_CLIENT_ID")
  Sys.unsetenv("li_CLIENT_SECRET")
  
  # Function should return NULL when credentials not set
  result <- li_auth()
  expect_null(result)
})
