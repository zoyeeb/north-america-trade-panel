# census_historical_hs10_check.R
# Establishes where the Census API HS10 coverage actually begins, by asking it.
# This is where the 2010-01 floor in census_config.R comes from - Census's own
# documentation claims 2013.
#
# In:   CENSUS_API_KEY
# Out:  findings on stdout
# Run:  Rscript R/06_diagnostics/census_historical_hs10_check.R

if (!requireNamespace("httr2", quietly = TRUE)) {
  install.packages("httr2", repos = "https://cloud.r-project.org")
}
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

census_get <- function(flow = c("exports", "imports"), fields, year, month,
                        extra_params = list()) {
  flow <- match.arg(flow)
  url <- paste0("https://api.census.gov/data/timeseries/intltrade/", flow, "/hs")
  req <- request(url) |>
    req_url_query(get = paste(fields, collapse = ","), YEAR = year, MONTH = month,
                   key = census_key, !!!extra_params)
  resp <- req_perform(req)
  raw <- resp_body_json(resp, simplifyVector = TRUE)
  colnames_row <- raw[1, ]
  data_rows <- raw[-1, , drop = FALSE]
  df <- as.data.frame(data_rows, stringsAsFactors = FALSE)
  names(df) <- colnames_row
  df
}

cat("=== CHECK: does the API's real HS10 floor match Census's documented '2013-present', or HS6's already-confirmed 2010? ===\n")
for (yr in c(2003, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) {
  result <- tryCatch({
    d <- census_get(flow = "exports", fields = c("E_COMMODITY", "ALL_VAL_MO"),
                     year = yr, month = "01",
                     extra_params = list(COMM_LVL = "HS10", CTY_CODE = "1220"))
    if (nrow(d) > 0) paste("DATA -", nrow(d), "rows") else "NO DATA (empty)"
  }, error = function(e) paste("ERROR/NO DATA -", conditionMessage(e)))
  cat(yr, ":", result, "\n")
}

cat("\n=== CHECK: are 2010 HS10 codes genuinely 10 digits? ===\n")
sample_2010 <- census_get(flow = "exports", fields = c("E_COMMODITY", "E_COMMODITY_LDESC", "ALL_VAL_MO"),
                           year = 2010, month = "01",
                           extra_params = list(COMM_LVL = "HS10", CTY_CODE = "1220"))
cat("All codes exactly 10 characters:", all(nchar(sample_2010$E_COMMODITY) == 10), "\n")
print(head(sample_2010, 3))
