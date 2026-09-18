# census_config.R
# Sample boundaries for the US side. Sourced by census_api_pull.R and
# census_api_validate.R so the two cannot disagree about where the panel ends.
#
# Out: START_YEAR, END_YEAR, END_MONTH, PANEL_YEARS, expected_months(),
#      expected_month_labels()

# 2010-01 is the Census API's real floor for HS10, confirmed empirically in
# 06_diagnostics/census_historical_hs10_check.R. Census's own docs claim 2013.
START_YEAR <- 2010L

# A deliberate cutoff, not an API limit. Census keeps publishing past it, and a
# sample whose size depends on when it was last run is not reproducible.
END_YEAR   <- 2026L
END_MONTH  <- 7L

expected_months <- function(yr) {
  yr <- as.integer(yr)
  if (yr < END_YEAR)  return(12L)
  if (yr == END_YEAR) return(END_MONTH)
  return(0L)
}

# WHICH months, not just how many: 01-06 plus 08 is seven months and still wrong.
expected_month_labels <- function(yr) {
  n <- expected_months(yr)
  if (n == 0L) return(character(0))
  sprintf("%02d", seq_len(n))
}

PANEL_YEARS <- START_YEAR:END_YEAR
