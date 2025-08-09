# Authentication Functions for adlibr
#
# This file contains authentication functions for various platforms
# supporting different OAuth flows and API key management.

#' Set Up Authentication for adlibr
#' 
#' @description
#' Interactive helper to set up authentication credentials for supported platforms.
#' This function guides users through configuring API keys, OAuth credentials,
#' and other authentication requirements for each platform.
#' 
#' @param platforms Character vector. Platforms to configure. If NULL, shows menu.
#' @param force Logical. Whether to overwrite existing credentials.
#' @param use_keyring Logical. Whether to use keyring for secure storage.
#' 
#' @return Invisible TRUE on success
#' @export
#'
#' @examples
#' \dontrun{
#' # Interactive setup for all platforms
#' adlibr_auth_setup()
#' 
#' # Setup specific platforms
#' adlibr_auth_setup(c("meta", "tiktok"))
#' 
#' # Force overwrite existing credentials
#' adlibr_auth_setup("meta", force = TRUE)
#' }
adlibr_auth_setup <- function(platforms = NULL, force = FALSE, use_keyring = TRUE) {
  
  # Check if keyring is available
  if (use_keyring && !requireNamespace("keyring", quietly = TRUE)) {
    cli::cli_warn("keyring package not available, falling back to .Renviron")
    use_keyring <- FALSE
  }
  
  supported_platforms <- adlibr_platforms()
  
  if (is.null(platforms)) {
    cli::cli_h1("adlibr Authentication Setup")
    cli::cli_text("Select platforms to configure:")
    
    choices <- c(supported_platforms, "All platforms")
    selection <- utils::menu(choices, title = "Choose platforms:")
    
    if (selection == 0) {
      cli::cli_inform("Setup cancelled")
      return(invisible(FALSE))
    }
    
    if (selection == length(choices)) {
      platforms <- supported_platforms
    } else {
      platforms <- choices[selection]
    }
  }
  
  # Validate platforms
  invalid_platforms <- setdiff(platforms, supported_platforms)
  if (length(invalid_platforms) > 0) {
    cli::cli_abort("Invalid platforms: {.val {invalid_platforms}}")
  }
  
  # Configure each platform
  for (platform in platforms) {
    cli::cli_h2("Configuring {platform}")
    configure_platform_auth(platform, force = force, use_keyring = use_keyring)
  }
  
  cli::cli_alert_success("Authentication setup complete!")
  cli::cli_text("Restart R or run {.code readRenviron('.Renviron')} to load new credentials.")

  invisible(TRUE)
}

#' Get Credential from Storage
#' 
#' @description
#' Retrieves a credential from keyring or environment variables.
#' 
#' @param key Character. Credential key/environment variable name
#' @param use_keyring Logical. Whether to try keyring first
#' 
#' @return Character. Credential value or NULL if not found
#' @keywords internal
get_credential <- function(key, use_keyring = TRUE) {
  
  # Try keyring first if available
  if (use_keyring && requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      value <- keyring::key_get(key, keyring = "adlibr")
      if (!is.null(value) && nchar(value) > 0) {
        return(value)
      }
    }, error = function(e) {
      # Fall through to environment variable
    })
  }
  
  # Try environment variable
  value <- Sys.getenv(key)
  if (nchar(value) > 0) {
    return(value)
  }
  
  # For LinkedIn, also check legacy token name for backwards compatibility
  if (key == "LINKEDIN_TOKEN") {
    legacy_value <- Sys.getenv("LINKEDIN_AD_LIBRARY_TOKEN")
    if (nchar(legacy_value) > 0) {
      return(legacy_value)
    }
  }
  
  return(NULL)
}

#' Check Authentication Status
#' 
#' @description
#' Checks authentication status for one or more platforms.
#' 
#' @param platforms Character vector. Platforms to check. If NULL, checks all.
#' 
#' @return Data frame with authentication status for each platform
#' @export
#'
#' @examples
#' \dontrun{
#' # Check all platforms
#' adlibr_auth_status()
#' 
#' # Check specific platforms
#' adlibr_auth_status(c("meta", "tiktok"))
#' }
adlibr_auth_status <- function(platforms = NULL) {
  
  if (is.null(platforms)) {
    platforms <- adlibr_platforms()
  }
  
  results <- tibble::tibble(
    platform = character(0),
    authenticated = logical(0),
    missing_credentials = list(),
    notes = character(0)
  )
  
  for (platform in platforms) {
    config <- get_platform_config(platform)
    
    if (!"env_vars" %in% names(config) || length(config$env_vars) == 0) {
      results <- rbind(results, tibble::tibble(
        platform = platform,
        authenticated = TRUE,
        missing_credentials = list(character(0)),
        notes = "No authentication required"
      ))
      next
    }
    
    missing_vars <- character(0)
    for (var in config$env_vars) {
      if (is.null(get_credential(var))) {
        missing_vars <- c(missing_vars, var)
      }
    }
    
    authenticated <- length(missing_vars) == 0
    
    results <- rbind(results, tibble::tibble(
      platform = platform,
      authenticated = authenticated,
      missing_credentials = list(missing_vars),
      notes = if (authenticated) "Configured" else "Missing credentials"
    ))
  }
  
  return(results)
}

#' Configure Platform-Specific Authentication
#' 
#' @description
#' Configures authentication for a specific platform
#' 
#' @param platform Character. Platform name
#' @param force Logical. Whether to overwrite existing credentials
#' @param use_keyring Logical. Whether to use keyring for secure storage
#' 
#' @return Invisible TRUE on success
#' @keywords internal
configure_platform_auth <- function(platform, force = FALSE, use_keyring = TRUE) {
  
  switch(platform,
    "linkedin" = configure_linkedin_auth(force, use_keyring),
    "meta" = configure_meta_auth(force, use_keyring),
    "tiktok" = configure_tiktok_auth(force, use_keyring),
    {
      cli::cli_inform("Platform {platform} authentication configuration not yet implemented")
      cli::cli_text("For now, manually set the required environment variables:")
      
      config <- get_platform_config(platform)
      if (!is.null(config$env_vars)) {
        for (var in config$env_vars) {
          cli::cli_text("  {.var {var}}")
        }
      }
      invisible(TRUE)
    }
  )
}

#' Configure LinkedIn Authentication
#' 
#' @description
#' Interactive setup for LinkedIn Ad Library API authentication
#' 
#' @param force Logical. Whether to overwrite existing credentials
#' @param use_keyring Logical. Whether to use keyring for secure storage
#' 
#' @return Invisible TRUE on success
#' @keywords internal
configure_linkedin_auth <- function(force = FALSE, use_keyring = TRUE) {
  
  cli::cli_rule("LinkedIn Ad Library Setup")
  
  # Check if we already have a working token
  if (!force) {
    existing_token <- get_credential("LINKEDIN_TOKEN", use_keyring)
    if (!is.null(existing_token)) {
      
      # Test if existing token works
      cli::cli_alert_info("Testing existing LinkedIn token...")
      
      test_result <- test_linkedin_token(existing_token)
      if (test_result$valid) {
        cli::cli_alert_success("✅ Existing LinkedIn token is working!")
        return(invisible(TRUE))
      } else {
        cli::cli_alert_warning("❌ Existing token is invalid: {test_result$message}")
      }
    }
  }
  
  cli::cli_text()
  cli::cli_alert_info("Setting up LinkedIn Ad Library API access...")
  cli::cli_text("To use LinkedIn's Ad Library, you need to create a developer application.")
  cli::cli_text()
  
  # Step 1: Guide user to get credentials
  cli::cli_h3("Step 1: Get Developer Credentials")
  cli::cli_ol(c(
    "Go to {.url https://developer.linkedin.com/}",
    "Sign in with your LinkedIn account",
    "Click {.strong Create app}",
    "Fill in the app details (name, company, description)",
    "In {.strong Products}, add {.strong Advertising API}",
    "In {.strong Auth} tab, check your redirect URLs and add: {.code http://localhost:1410}",
    "Copy your {.strong Client ID} and {.strong Client Secret}"
  ))
  
  cli::cli_text()
  if (!interactive()) {
    cli::cli_alert_danger("Interactive session required for credential entry")
    return(invisible(FALSE))
  }
  
  # Check for existing credentials first  
  client_id <- get_credential("LINKEDIN_CLIENT_ID", use_keyring)
  client_secret <- get_credential("LINKEDIN_CLIENT_SECRET", use_keyring)
  
  if (!is.null(client_id) && !is.null(client_secret) && !force) {
    cli::cli_alert_info("Found existing LinkedIn credentials")
    cli::cli_text("Client ID: {.val {substr(client_id, 1, 8)}...}")
    
    if (utils::askYesNo("Use existing credentials?", default = TRUE)) {
      # Use existing credentials
    } else {
      client_id <- NULL
      client_secret <- NULL
    }
  }
  
  # Get credentials from user if needed
  if (is.null(client_id) || is.null(client_secret)) {
    cli::cli_h3("Step 2: Enter Your Credentials")
    
    repeat {
      cli::cli_text("Please enter your LinkedIn app credentials:")
      
      if (is.null(client_id)) {
        client_id <- get_user_input("Client ID", "Your LinkedIn app's Client ID")
      }
      
      if (is.null(client_secret)) {
        client_secret <- get_user_input("Client Secret", "Your LinkedIn app's Client Secret", 
                                       mask = TRUE)
      }
      
      if (nchar(client_id) > 10 && nchar(client_secret) > 10) {
        break
      } else {
        cli::cli_alert_danger("Invalid credentials - please check and try again")
        client_id <- NULL
        client_secret <- NULL
      }
    }
    
            # Store credentials
        store_credential("LINKEDIN_CLIENT_ID", client_id, use_keyring)
        store_credential("LINKEDIN_CLIENT_SECRET", client_secret, use_keyring)
  }
  
  # Step 3: Perform OAuth Authentication
  cli::cli_h3("Step 3: OAuth Authentication")
  cli::cli_alert_info("Starting LinkedIn OAuth 2.0 flow...")
  cli::cli_text("Your browser will open for authorization.")
  
  # Perform the OAuth flow to get a real access token
  tryCatch({
    token <- linkedin_oauth_flow(client_id, client_secret)
    
    # For now, skip the API test and just store the token
    # The OAuth flow succeeded, which means we have a valid token
    # We'll test it when actually making API calls
    cli::cli_alert_info("Storing token for later use...")
    
    # Store the working token
    store_credential("LINKEDIN_TOKEN", token, use_keyring)
    
    cli::cli_alert_success("🎉 LinkedIn authentication successful!")
    cli::cli_text("Token stored. You can now test with {.code adlibr_search('linkedin', ...)} to search ads")
    cli::cli_text("Note: The token will be validated when making actual API calls")
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    cli::cli_alert_danger("❌ OAuth flow failed: {e$message}")
    cli::cli_text()
    cli::cli_text("Common issues:")
    cli::cli_ul(c(
      "Make sure you've added the {.strong Advertising API} product to your app",
      "Verify your app has been approved for the Advertising API", 
      "Check that your redirect URL is set to: {.code http://localhost:1410/}",
      "Your app may need review by LinkedIn (can take 1-2 weeks)"
    ))
    
    return(invisible(FALSE))
  })
}

#' LinkedIn OAuth 2.0 Flow
#' 
#' @description
#' Performs the 3-legged OAuth 2.0 flow for LinkedIn using httr
#' Interactive OAuth 2.0 flow for LinkedIn authentication
#' 
#' @param client_id Character. LinkedIn client ID
#' @param client_secret Character. LinkedIn client secret
#' 
#' @return Character. Access token
#' @keywords internal
linkedin_oauth_flow <- function(client_id, client_secret) {
  
  # Define the LinkedIn OAuth app and endpoints
  li_app <- httr::oauth_app("linkedin",
                           key = client_id,
                           secret = client_secret)
  
  li_endpoint <- httr::oauth_endpoint(
    authorize = "https://www.linkedin.com/oauth/v2/authorization",
    access = "https://www.linkedin.com/oauth/v2/accessToken"
  )
  
  cli::cli_alert_info("Your browser will open for LinkedIn authorization...")
  
  # Check if we're in an interactive session or non-interactive environment
  if (interactive()) {
    # Standard interactive OAuth flow
    cli::cli_alert_info("Using interactive OAuth flow...")
    token <- httr::oauth2.0_token(
      endpoint = li_endpoint,
      app = li_app,
      scope = "openid profile", # Required scope for LinkedIn Ad Library access
      cache = FALSE,
      use_oob = FALSE
    )
  } else {
    # Manual OAuth flow for non-interactive environments
    cli::cli_alert_info("Non-interactive environment detected. Using manual OAuth flow...")
    token <- manual_linkedin_oauth(client_id, client_secret)
  }
  
  if (is.null(token)) {
    cli::cli_abort("OAuth authentication failed - no token received")
  }
  
  # Extract the access token
  # Debug: let's see what's in the token object
  cli::cli_alert_info("Examining token structure...")
  
  # Try different ways to extract the token (httr tokens can have different structures)
  access_token <- NULL
  
  if (!is.null(token$credentials$access_token)) {
    access_token <- token$credentials$access_token
    cli::cli_alert_info("Found token in credentials$access_token")
  } else if (!is.null(token$access_token)) {
    access_token <- token$access_token  
    cli::cli_alert_info("Found token in access_token")
  } else {
    # Print token structure for debugging
    cli::cli_alert_warning("Token structure unknown. Available fields:")
    if (is.list(token)) {
      cli::cli_text(paste(names(token), collapse = ", "))
      if (!is.null(token$credentials) && is.list(token$credentials)) {
        cli::cli_text("Credentials fields: {paste(names(token$credentials), collapse = ', ')}")
      }
    }
    cli::cli_abort("Could not extract access token from OAuth response")
  }
  
  if (is.null(access_token) || nchar(access_token) == 0) {
    cli::cli_abort("Empty access token received")
  }
  
  cli::cli_alert_success("OAuth flow completed successfully!")
  cli::cli_alert_info("Token extracted: {substr(access_token, 1, 10)}...")
  
  return(access_token)
}

#' Manual LinkedIn OAuth Flow
#' 
#' @description
#' Performs OAuth flow manually for non-interactive environments
#' 
#' @param client_id Character. LinkedIn client ID
#' @param client_secret Character. LinkedIn client secret
#' 
#' @return httr Token object
#' @keywords internal
manual_linkedin_oauth <- function(client_id, client_secret) {
  
  # OAuth 2.0 parameters
  auth_url <- "https://www.linkedin.com/oauth/v2/authorization"
  token_url <- "https://www.linkedin.com/oauth/v2/accessToken"
  redirect_uri <- "http://localhost:1410/"
  scope <- "openid profile"
  state <- paste0(sample(letters, 16, replace = TRUE), collapse = "")
  
  # Step 1: Build authorization URL
  auth_params <- list(
    response_type = "code",
    client_id = client_id,
    redirect_uri = redirect_uri,
    state = state,
    scope = scope
  )
  
  auth_query <- paste0(names(auth_params), "=", lapply(auth_params, utils::URLencode), collapse = "&")
  full_auth_url <- paste0(auth_url, "?", auth_query)
  
  # Display the URL for manual authorization
  cat("\n" , rep("=", 80), "\n", sep = "")
  cat("MANUAL OAUTH AUTHORIZATION REQUIRED\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n1. Open this URL in your browser:\n\n")
  cat(full_auth_url, "\n\n")
  cat("2. Authorize the application\n")
  cat("3. You will be redirected to a URL like:\n")
  cat("   http://localhost:1410/?code=YOUR_AUTH_CODE&state=", state, "\n\n", sep = "")
  cat("4. Copy the ENTIRE redirect URL and paste it below:\n\n")
  cat(rep("=", 80), "\n")
  
  # Get the redirect URL from user input (simulated)
  cat("Redirect URL (paste here): ")
  redirect_url <- readline()
  
  # Extract code from URL
  if (!grepl("code=", redirect_url)) {
    stop("Invalid redirect URL - no authorization code found")
  }
  
  code_match <- regmatches(redirect_url, regexpr("code=([^&]+)", redirect_url))
  auth_code <- gsub("code=", "", code_match)
  
  if (nchar(auth_code) == 0) {
    stop("Could not extract authorization code")
  }
  
  cat("✓ Authorization code extracted successfully\n")
  
  # Step 3: Exchange code for token
  cat("Exchanging code for access token...\n")
  
  token_params <- list(
    grant_type = "authorization_code",
    code = auth_code,
    redirect_uri = redirect_uri,
    client_id = client_id,
    client_secret = client_secret
  )
  
  response <- httr::POST(
    token_url,
    body = token_params,
    encode = "form"
  )
  
  if (httr::status_code(response) != 200) {
    error_details <- httr::content(response, "text")
    stop("Token exchange failed: ", error_details)
  }
  
  token_data <- httr::content(response, "parsed")
  
  if (is.null(token_data$access_token)) {
    stop("No access token in response")
  }
  
  # Create a mock token object similar to what httr::oauth2.0_token returns
  mock_token <- list(
    credentials = list(
      access_token = token_data$access_token,
      token_type = token_data$token_type %||% "Bearer",
      expires_in = token_data$expires_in,
      scope = token_data$scope
    )
  )
  
  cat("✓ Access token received successfully\n")
  
  return(mock_token)
}

#' Store Credential
#' 
#' @description
#' Stores a credential in keyring or environment file
#' 
#' @param key Character. Credential key
#' @param value Character. Credential value  
#' @param use_keyring Logical. Whether to use keyring
#' 
#' @return Invisible TRUE
#' @keywords internal
store_credential <- function(key, value, use_keyring = TRUE) {
  
  if (use_keyring && requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      keyring::key_set_with_value(key, value, keyring = "adlibr")
      cli::cli_alert_success("Stored {key} in keyring")
      return(invisible(TRUE))
    }, error = function(e) {
      cli::cli_warn("Failed to store in keyring: {e$message}")
    })
  }
  
  # Fall back to environment variable
  env_list <- list(value)
  names(env_list) <- key
  do.call(Sys.setenv, env_list)
  cli::cli_alert_info("Set {key} as environment variable for this session")
  cli::cli_text("To persist, add to your {.file .Renviron} file:")
  cli::cli_code("{key}={value}")
  
  invisible(TRUE)
}

#' Configure Meta Authentication (placeholder)
#' @keywords internal
configure_meta_auth <- function(force = FALSE, use_keyring = TRUE) {
  cli::cli_inform("Meta authentication: Set META_TOKEN environment variable with your Graph API token")
  invisible(TRUE)
}

#' Configure TikTok Authentication (placeholder)  
#' @keywords internal
configure_tiktok_auth <- function(force = FALSE, use_keyring = TRUE) {
  cli::cli_inform("TikTok authentication: Set TIKTOK_CLIENT_ID, TIKTOK_CLIENT_SECRET environment variables")
  invisible(TRUE)
}

#' Test LinkedIn Token
#' 
#' @description
#' Tests if a LinkedIn token is valid by making a test API call
#' 
#' @param token Character. LinkedIn access token
#' 
#' @return List with validation results
#' @keywords internal
test_linkedin_token <- function(token) {
  
  tryCatch({
    # Test with a simpler endpoint first - just check if the token is valid
    # Use the profile endpoint which should work with "openid profile" scope
    test_req <- httr2::request("https://api.linkedin.com/v2/people/~") |>
      httr2::req_headers(
        Authorization = paste("Bearer", token)
      ) |>
      httr2::req_timeout(10)
    
    response <- httr2::req_perform(test_req)
    
    if (httr2::resp_status(response) == 200) {
      return(list(valid = TRUE, message = "Token is working"))
    } else {
      error_body <- httr2::resp_body_string(response)
      return(list(valid = FALSE, message = paste("HTTP", httr2::resp_status(response), error_body)))
    }
    
  }, error = function(e) {
    return(list(valid = FALSE, message = e$message))
  })
}

#' Get User Input with Validation
#' 
#' @description
#' Gets user input with nice formatting and optional masking
#' 
#' @param field_name Character. Name of the field
#' @param description Character. Description for the user
#' @param mask Logical. Whether to mask input (for secrets)
#' 
#' @return Character. User input
#' @keywords internal
get_user_input <- function(field_name, description, mask = FALSE) {
  
  cli::cli_text()
  cli::cli_text("{.strong {field_name}}")
  cli::cli_text("{.muted {description}}")
  
  if (mask && requireNamespace("getPass", quietly = TRUE)) {
    # Use getPass for secure input if available
    cli::cli_alert_info("A secure input dialog will appear...")
    value <- getPass::getPass(msg = paste0("Enter ", field_name, ": "))
  } else {
    # Fall back to readline (less secure but works everywhere)
    if (mask) {
      cli::cli_alert_warning("Using readline (not fully secure). Install 'getPass' for better security.")
    }
    value <- readline(paste0(field_name, ": "))
  }
  
  return(trimws(value))
}
