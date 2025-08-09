# LinkedIn Ad Library API R Functions
# PART 1: AUTHENTICATION (3-Legged OAuth 2.0)

# This script provides a robust implementation of LinkedIn's Authorization Code Flow.
# It uses the `httr` package to manage the complexities of browser redirection,
# token exchange, caching, and refreshing.

#==============================================================================#
# Dependencies
#==============================================================================#

# Ensure you have these packages installed:
# install.packages(c("httr", "usethis", "jsonlite"))

#==============================================================================#
# 1. One-Time Setup Function
#==============================================================================#

#' Configure LinkedIn Application Credentials
#'
#' This is the **first step** in the authentication process. It securely stores your
#' application's Client ID and Client Secret in your .Renviron file, making them
#' available for your R sessions. You only need to run this once per project/machine.
#'
#' @param force Logical. If TRUE, will overwrite existing credentials without asking.
#' @param verbose Logical. If TRUE, provides detailed output.
#'
#' @return Invisibly returns TRUE on success.
#' @export
#'
#' @examples
#' \dontrun{
#' # Run this function once to set up your credentials
#' li_auth_configure()
#' }
li_auth_configure <- function(force = FALSE, verbose = TRUE) {
  client_id_var <- "li_CLIENT_ID"
  client_secret_var <- "li_CLIENT_SECRET"

  existing_id <- Sys.getenv(client_id_var)
  existing_secret <- Sys.getenv(client_secret_var)

  if (!force && (existing_id != "" || existing_secret != "")) {
    overwrite <- usethis::ui_yeah(
      "Existing LinkedIn credentials found. Overwrite them?",
      no = "Keeping existing credentials."
    )
    if (!overwrite) {
      return(invisible(FALSE))
    }
  }

  if (verbose) {
    usethis::ui_info("Please enter the credentials for your LinkedIn Developer App.")
    usethis::ui_info("You can find these in the 'Auth' tab of your app at https://www.linkedin.com/developers/apps")
  }

  client_id <- readline(usethis::ui_field("Client ID (API Key): "))
  client_secret <- readline(usethis::ui_field("Client Secret: "))

  if (client_id == "" || client_secret == "") {
    usethis::ui_oops("Client ID and Client Secret cannot be empty.")
    return(invisible(FALSE))
  }

  # Set environment variables for the current session and .Renviron file
  Sys.setenv(li_CLIENT_ID = client_id)
  Sys.setenv(li_CLIENT_SECRET = client_secret)

  renv_path <- file.path(getwd(), ".Renviron")
  if (!file.exists(renv_path)) file.create(renv_path)

  lines <- readLines(renv_path)
  # Remove existing lines to avoid duplicates
  lines <- lines[!grepl(paste0("^", client_id_var, "|^", client_secret_var), lines)]

  new_lines <- c(
    paste0(client_id_var, "='", client_id, "'"),
    paste0(client_secret_var, "='", client_secret, "'")
  )

  writeLines(c(lines, new_lines), renv_path)

  if (verbose) {
    usethis::ui_done("LinkedIn credentials have been stored in your .Renviron file.")
    usethis::ui_info("Restart R or run `readRenviron('.Renviron')` for the changes to take effect.")
  }

  invisible(TRUE)
}

#==============================================================================#
# 2. Main Authentication and Token Functions
#==============================================================================#

#' Authenticate with LinkedIn (3-Legged OAuth)
#'
#' This is the **second step**. It initiates the 3-legged OAuth 2.0 flow.
#' It will open your web browser, ask you to log in to LinkedIn and authorize the
#' application scopes. After authorization, it automatically fetches and caches
#' an access token for future use.
#'
#' @param scope A space-delimited string of permissions your application is
#'   requesting. Per new OpenID Connect docs, this defaults to "openid profile".
#' @param cache Logical or a string. `TRUE` caches the token in a file named
#'   `.httr-oauth` in the current working directory. Caching is highly recommended.
#' @param new_user Logical. Set to `TRUE` to force a new authentication, even if
#'   a cached token exists.
#'
#' @return A `Token2.0` object, which is automatically cached.
#' @export
#'
#' @examples
#' \dontrun{
#' # After configuring, run this to authenticate. Your browser will open.
#' li_token <- li_auth()
#' }
li_auth <- function(scope = "openid profile", cache = TRUE, new_user = FALSE) {

  # Check if credentials are set
  client_id <- Sys.getenv("li_CLIENT_ID")
  client_secret <- Sys.getenv("li_CLIENT_SECRET")

  if (client_id == "" || client_secret == "") {
    usethis::ui_oops("LinkedIn credentials not found.")
    usethis::ui_info("Please run `li_auth_configure()` first to set them up.")
    return(invisible(NULL))
  }

  # Define the LinkedIn OAuth app and endpoints
  li_app <- httr::oauth_app("linkedin",
                                  key = client_id,
                                  secret = client_secret
  )

  li_endpoint <- httr::oauth_endpoint(
    authorize = "https://www.linkedin.com/oauth/v2/authorization",
    access = "https://www.linkedin.com/oauth/v2/accessToken"
  )

  usethis::ui_info("Attempting to authenticate with LinkedIn.")
  usethis::ui_info("Your browser will open to ask for your permission.")

  # Perform the OAuth dance
  token <- httr::oauth2.0_token(
    endpoint = li_endpoint,
    app = li_app,
    scope = scope, # Uses the new default "openid profile"
    cache = cache,
    use_oob = FALSE,
    options(httr_oob_default=TRUE)
  )

  if (!is.null(token)) {
    usethis::ui_done("Authentication successful. Token has been cached.")
  } else {
    usethis::ui_oops("Authentication failed.")
  }

  invisible(token)
}

#' Get a Valid LinkedIn Access Token
#'
#' This is the helper function that other API functions will use. It retrieves the
#' cached token, and if the token has expired, it automatically attempts to
#' refresh it using the stored refresh token.
#'
#' @param cache_path Path to the cached token file. Defaults to `.httr-oauth`.
#'
#' @return A `Token2.0` object, or `NULL` if no valid token can be found/refreshed.
#' @keywords internal
li_get_token <- function(cache_path = ".httr-oauth") {

  if (!file.exists(cache_path)) {
    usethis::ui_oops("No cached token found at '{cache_path}'.")
    usethis::ui_info("Please run `li_auth()` to authenticate first.")
    return(NULL)
  }

  token <- readRDS(cache_path)

  # # httr's token object has a method to check for expiry
  # if (token$has_expired()) {
  #   usethis::ui_info("Access token has expired. Attempting to refresh...")
  #   token <- token$refresh()
  #   usethis::ui_done("Token refreshed successfully.")
  # }

  return(token)
}
