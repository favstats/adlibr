
<!-- README.md is generated from README.Rmd. Please edit that file -->

# liads

<!-- badges: start -->

[![R-CMD-check](https://github.com/favstats/liads/workflows/R-CMD-check/badge.svg)](https://github.com/favstats/liads/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/liads)](https://CRAN.R-project.org/package=liads)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

**liads** is a comprehensive R client for the [LinkedIn Ad Library
API](https://www.linkedin.com/ad-library/api/ads). It provides tools for
OAuth 2.0 authentication, querying ads by various criteria, automatic
pagination, and robust data processing.

## Features

- **🔐 OAuth 2.0 Authentication**: Complete 3-legged authentication
  workflow
- **🔍 Comprehensive Search**: Query by keyword, advertiser, country,
  and date range
- **📊 Automatic Pagination**: Handles large result sets automatically
- **🛡️ Robust Error Handling**: Graceful handling of API errors and edge
  cases
- **📈 Rich Data Structure**: Returns structured data with targeting and
  impression data
- **✅ Full API Coverage**: Supports all parameters from the official
  LinkedIn API

## Installation

You can install the development version of liads from
[GitHub](https://github.com/favstats/liads) with:

``` r
# install.packages("devtools")
devtools::install_github("favstats/liads")
```

## Quick Start

### 1. Authentication Setup

First, set up your LinkedIn Developer App credentials (one-time setup):

``` r
library(liads)

# Configure your LinkedIn app credentials
li_auth_configure()  # You'll be prompted to enter Client ID and Secret

# Authenticate (opens browser for OAuth)
li_auth()
```

### 2. Basic Usage

``` r
# Search for ads by keyword
marketing_ads <- li_query(
  keyword = "digital marketing",
  countries = c("us", "ca"),
  start_date = "2024-01-01",
  end_date = "2024-03-31"
)

# Search for ads by advertiser
google_ads <- li_query(
  advertiser = "Google",
  countries = c("us"),
  max_pages = 5
)

# Search for recent ads in specific countries
recent_ads <- li_query(
  countries = c("gb", "de", "fr"),
  start_date = "2024-06-01",
  end_date = "2024-06-30"
)

# Get cleaned data without list-columns (easier to work with)
clean_ads <- li_query(
  keyword = "artificial intelligence",
  countries = c("us"),
  clean = TRUE,
  direction = "wide"
)
```

## API Parameters

The `li_query()` function supports all parameters from the [LinkedIn Ad
Library API](https://www.linkedin.com/ad-library/api/ads):

| Parameter | Type | Description | Example |
|----|----|----|----|
| `keyword` | String | Keywords to search in ad content (AND logic) | `"data science"` |
| `advertiser` | String | Advertiser name to search | `"Microsoft"` |
| `countries` | Character vector | 2-letter ISO country codes | `c("us", "gb", "de")` |
| `start_date` | Date/String | Start date (inclusive) | `"2024-01-01"` |
| `end_date` | Date/String | End date (exclusive) | `"2024-12-31"` |
| `count` | Integer | Results per page (max 25) | `25` |
| `max_pages` | Integer | Maximum pages to retrieve | `10` |
| `clean` | Logical | Return simplified data without list-columns | `TRUE` |
| `direction` | String | When clean=TRUE: “wide” or “long” format | `"wide"` |

## Data Structure

The function returns a tibble with the following columns:

``` r
# View the structure of returned data
str(marketing_ads)

# Key columns include:
# - ad_url: Direct link to ad preview
# - advertiser_name: Name of the advertiser
# - ad_type: Type of advertisement
# - first_impression_at, latest_impression_at: Date ranges
# - total_impressions_from, total_impressions_to: Impression ranges
# - impressions_by_country: List-column with country breakdown
# - ad_targeting: List-column with targeting criteria
```

## Advanced Examples

### Search with Multiple Criteria

``` r
# Complex search combining multiple parameters
tech_ads <- li_query(
  keyword = "artificial intelligence machine learning",
  countries = c("us", "gb", "de", "fr"),
  start_date = "2024-01-01",
  end_date = "2024-06-30",
  max_pages = 10
)

print(paste("Found", nrow(tech_ads), "AI/ML ads"))
```

### Analyze Targeting Data

``` r
# Extract and analyze targeting information
targeting_data <- google_ads |>
  tidyr::unnest(ad_targeting) |>
  dplyr::filter(!is.na(facetName))

# Most common targeting criteria
targeting_summary <- targeting_data |>
  dplyr::count(facetName, sort = TRUE)

print(targeting_summary)
```

### Country-wise Impression Analysis

``` r
# Analyze impression distribution by country
impression_data <- google_ads |>
  tidyr::unnest(impressions_by_country) |>
  dplyr::filter(!is.na(country))

# Top countries by impression percentage
country_summary <- impression_data |>
  dplyr::group_by(country) |>
  dplyr::summarise(
    avg_impression_pct = mean(impression_percentage, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(avg_impression_pct))

print(country_summary)
```

### Using Clean Data Format

``` r
# Get data in clean format (no list-columns)
clean_wide <- li_query(
  keyword = "machine learning",
  countries = c("us"),
  clean = TRUE,
  direction = "wide",
  max_pages = 1
)

# Wide format: targeting data as separate columns
print("Clean wide format columns:")
print(names(clean_wide))

# Long format: all targeting/impression data stacked
clean_long <- li_query(
  keyword = "machine learning", 
  countries = c("us"),
  clean = TRUE,
  direction = "long",
  max_pages = 1
)

print("Clean long format columns:")
print(names(clean_long))

# Long format makes it easy to analyze all targeting data
if (nrow(clean_long) > 0 && "data_type" %in% names(clean_long)) {
  targeting_analysis <- clean_long |>
    dplyr::filter(data_type == "targeting", !is.na(category)) |>
    dplyr::count(category, value, sort = TRUE)
  
  if (nrow(targeting_analysis) > 0) {
    print("Most common targeting values:")
    print(head(targeting_analysis))
  } else {
    print("No targeting data available for this query")
  }
}
```

## Data Format Options

| Format | Description | Use Case |
|----|----|----|
| **Raw** (`clean = FALSE`) | List-columns preserved | Advanced analysis, full data access |
| **Clean Wide** (`clean = TRUE, direction = "wide"`) | Targeting/impression data as separate columns | Simple analysis, CSV export |
| **Clean Long** (`clean = TRUE, direction = "long"`) | Targeting/impression data stacked with type indicators | Comparative analysis across ad types |

## Rate Limits and Best Practices

- **Maximum 25 results per request** (API limitation)
- **Use pagination** for large datasets with `max_pages` parameter
- **Specific searches** may return fewer results than broad searches
- **Date ranges**: start is inclusive, end is exclusive
- **Keywords**: Multiple keywords use AND logic
- **Countries**: Use lowercase 2-letter ISO codes

## Authentication Details

The package uses OAuth 2.0 with automatic token caching:

1.  **Setup**: `li_auth_configure()` stores app credentials in
    `.Renviron`
2.  **Authentication**: `li_auth()` opens browser for user consent
3.  **Token Storage**: Tokens are cached in `.httr-oauth` for reuse
4.  **Automatic Refresh**: Tokens are automatically refreshed when
    needed

## API Reference

Full API documentation is available at:
<https://www.linkedin.com/ad-library/api/ads>

## Contributing

Please report bugs and feature requests at:
<https://github.com/favstats/liads/issues>

## License

MIT License. See `LICENSE` file for details.
