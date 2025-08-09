# adlibr Basic Usage Examples
# 
# This file demonstrates basic usage of the adlibr package
# for accessing ad library APIs across different platforms.

library(adlibr)
library(dplyr)

# Check available platforms
cat("Available platforms:\n")
print(adlibr_platforms())

# Check authentication status
cat("\nAuthentication status:\n")
auth_status <- adlibr_auth_status()
print(auth_status)

# Example 1: Meta (Facebook/Instagram) Ad Search
# Note: Requires META_TOKEN environment variable
if (Sys.getenv("META_TOKEN") != "") {
  cat("\n=== Meta Ad Search Example ===\n")
  
  meta_ads <- adlibr_search("meta", 
                           q = "climate change",
                           countries = c("US", "CA"),
                           date_from = "2024-01-01",
                           limit = 10)
  
  cat("Found", nrow(meta_ads), "Meta ads\n")
  if (nrow(meta_ads) > 0) {
    cat("Sample advertiser names:\n")
    print(head(meta_ads$advertiser_name[!is.na(meta_ads$advertiser_name)], 3))
  }
}

# Example 2: TikTok Ad Search
# Note: Requires TIKTOK_CLIENT_ID, TIKTOK_CLIENT_SECRET
if (Sys.getenv("TIKTOK_CLIENT_ID") != "") {
  cat("\n=== TikTok Ad Search Example ===\n")
  
  tryCatch({
    tiktok_ads <- adlibr_search("tiktok",
                               q = "technology",
                               countries = "US",
                               limit = 5)
    
    cat("Found", nrow(tiktok_ads), "TikTok ads\n")
  }, error = function(e) {
    cat("TikTok search failed:", e$message, "\n")
  })
}

# Example 3: Microsoft (Bing) Ad Search  
# Note: Public API, no authentication required
cat("\n=== Microsoft Ad Search Example ===\n")

tryCatch({
  microsoft_ads <- adlibr_search("microsoft",
                                q = "software",
                                countries = "US",
                                limit = 5)
  
  cat("Found", nrow(microsoft_ads), "Microsoft ads\n")
}, error = function(e) {
  cat("Microsoft search failed:", e$message, "\n")
})

# Example 4: Apple App Store Ad Search
# Note: Public API, no authentication required
cat("\n=== Apple Ad Search Example ===\n")

tryCatch({
  apple_ads <- adlibr_search("apple",
                            advertisers = "Apple Inc",
                            countries = "US",
                            limit = 5)
  
  cat("Found", nrow(apple_ads), "Apple ads\n")
}, error = function(e) {
  cat("Apple search failed:", e$message, "\n")
})

# Example 5: Cross-Platform Analysis
cat("\n=== Cross-Platform Analysis Example ===\n")

# Platforms that don't require authentication
public_platforms <- c("microsoft", "apple")

cross_platform_results <- list()

for (platform in public_platforms) {
  tryCatch({
    ads <- adlibr_search(platform,
                        q = "education",
                        countries = "US", 
                        limit = 3)
    
    cross_platform_results[[platform]] <- ads
    cat("Platform", platform, "returned", nrow(ads), "ads\n")
    
  }, error = function(e) {
    cat("Platform", platform, "failed:", e$message, "\n")
  })
}

# Combine results if any succeeded
if (length(cross_platform_results) > 0) {
  all_ads <- bind_rows(cross_platform_results)
  
  cat("\nCross-platform summary:\n")
  platform_summary <- all_ads %>%
    count(platform, sort = TRUE)
  print(platform_summary)
  
  cat("\nSample normalized schema:\n")
  print(colnames(all_ads))
}

# Example 6: Advertiser Search
cat("\n=== Advertiser Search Example ===\n")

# Search for advertisers on platforms that support it
advertiser_platforms <- c("tiktok", "microsoft")

for (platform in advertiser_platforms) {
  if (platform_supports(platform, "advertisers")) {
    tryCatch({
      advertisers <- adlibr_advertisers(platform, 
                                       q = "technology",
                                       limit = 3)
      
      cat("Found", nrow(advertisers), "advertisers on", platform, "\n")
      
    }, error = function(e) {
      cat("Advertiser search on", platform, "failed:", e$message, "\n")
    })
  }
}

cat("\n=== Example Complete ===\n")
cat("For authentication setup, run: adlibr_auth_setup()\n")
cat("For more examples, see the package documentation and vignettes.\n")
