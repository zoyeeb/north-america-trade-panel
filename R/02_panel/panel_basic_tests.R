# panel_basic_tests.R
# A few quick reads against the built panel, to confirm it opens and slices.
#
# In:   $DATA/panel/
# Run:  Rscript R/02_panel/panel_basic_tests.R

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

na_source("R/02_panel/panel_open.R")

COUNTRY <- "CA"          # "CA" or "MX"
YEARS   <- 2015:2019     # one year: 2019      range: 2015:2019

d <- panel_slice(reporter = "US", partner = COUNTRY, years = YEARS,
                 cols = c("period", "direction", "basis", "hs_code", "hs6",
                          "value", "quantity_1", "unit_1"))

imp <- d[d$direction == "imp", ]        # 598,490  (Canada -> US)
panel_xlsx(imp, "canada_to_us_20152019.xlsx")

exp <- d[d$direction == "imp", ]        # 598,490  (Canada -> US)
panel_xlsx(imp, "canada_to_us_20XX_20XX.xlsx")

exp_tot <- aggregate(value ~ period + hs_code, d[d$direction == "exp", ], sum)
