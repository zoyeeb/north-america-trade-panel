# config.R
# Resolves the repo root and all data locations. Sourced by every script.
# Data is not in the repo; set NA_PANEL_DATA_DIR in .Renviron to relocate it.
#
# Out: NA_PANEL_ROOT, DATA_DIR, CIMT_DIR, UTO_IN_DIR, na_data(), require_key()

# Keyed on the .Rproj, not .Renviron: .Renviron is gitignored, so a fresh clone
# would fail before it could explain why.
NA_PANEL_ROOT <- local({
  for (up in c(".", "..", "../..", "../../..")) {
    if (file.exists(file.path(up, "north-america-panel.Rproj"))) {
      return(normalizePath(up, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Cannot find the repo root from ", getwd(),
       ". Open north-america-panel.Rproj, or setwd() to the repo root.")
})

# R only auto-loads .Renviron from the working directory at session start, so a
# script run from a subfolder would not see the API keys.
if (file.exists(file.path(NA_PANEL_ROOT, ".Renviron"))) {
  readRenviron(file.path(NA_PANEL_ROOT, ".Renviron"))
}

DATA_DIR   <- Sys.getenv("NA_PANEL_DATA_DIR",
                         file.path(NA_PANEL_ROOT, "data"))
CIMT_DIR   <- Sys.getenv("NA_PANEL_CIMT_DIR", file.path(DATA_DIR, "cimt"))
UTO_IN_DIR <- Sys.getenv("NA_PANEL_UTO_IN_DIR",
                         file.path(DATA_DIR, "uto_raw"))

na_data <- function(...) file.path(DATA_DIR, ...)

# Source another script by its repo-relative path, so scripts can call each
# other without depending on the working directory.
na_source <- function(rel) {
  f <- file.path(NA_PANEL_ROOT, rel)
  if (!file.exists(f)) stop("Not found: ", rel)
  source(f)
}

# Fails by name rather than on an opaque HTTP 403 mid-pull.
require_key <- function(name) {
  key <- Sys.getenv(name)
  if (!nzchar(key)) {
    stop(name, " is not set.\n",
         "  Copy .Renviron.example to .Renviron, fill it in, restart R.")
  }
  key
}
