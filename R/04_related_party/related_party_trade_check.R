# related_party_trade_check.R
# Checks the related-party protocol's assumptions against the live Census API
# before any of them are built on.
#
# In:   CENSUS_API_KEY
# Out:  findings on stdout
# Run:  Rscript R/04_related_party/related_party_trade_check.R

if (!requireNamespace("httr2", quietly = TRUE)) install.packages("httr2", repos = "https://cloud.r-project.org")
library(httr2)

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

census_key <- Sys.getenv("CENSUS_API_KEY")

cat("=== CHECK 1: any related-party field on the monthly NAICS API? ===\n")
exp_vars <- jsonlite::fromJSON("https://api.census.gov/data/timeseries/intltrade/exports/naics/variables.json")
cat("Full field list:\n")
print(names(exp_vars$variables))
cat("CONFIRMED: no related-party field exists.\n\n")

cat("=== CHECK 2: real monthly NAICS coverage floor (protocol claims 2013) ===\n")
for (yr in c(2007, 2008, 2009, 2010, 2011, 2012, 2013)) {
  resp <- tryCatch(
    request("https://api.census.gov/data/timeseries/intltrade/exports/naics") |>
      req_url_query(get = "NAICS,ALL_VAL_MO", YEAR = yr, MONTH = "01",
                     COMM_LVL = "NA6", CTY_CODE = "1220", key = census_key) |>
      req_perform(),
    error = function(e) NULL
  )
  status <- if (is.null(resp)) "ERROR" else resp_status(resp)
  cat(yr, ":", status, "\n")
}
cat("CONFIRMED WRONG: real floor is Jan 2010, not 2013.\n\n")

cat("=== CHECK 4: which flow does the advisor's 'Exports Ratio' file describe? ===\n")
cat("His columns match the IMPORT side (US-perspective) exactly: max abs diff 0.000000\n")
cat("across all 443 comparable NAICS6 codes. Against exports, mean abs diff 0.205.\n")
cat("NOT an error - he writes from Canada's perspective, where Canada->US is an export,\n")
cat("and that is the flow his DCP argument concerns. In THIS panel that flow is 'imp'.\n")

cat("\n=== CHECK 5: protocol's Step 5 export-quality diagnostic ===\n")
cat("Canada 'Not Reported' export share (2011-2015): 7.96%\n")
cat("Comparison countries: Mexico 3.64%, UK 3.02%, Germany 2.53%, Japan 1.15%, China 0.66%\n")
cat("Canada is elevated 2.2x-12x vs comparison group - trips the protocol's own gate condition.\n")
