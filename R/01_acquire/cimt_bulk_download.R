# cimt_bulk_download.R
# Canadian side of the panel: Statistics Canada CIMT bulk files, one zip per
# (direction, year). No login, no request process.
#
# In:   nothing (network)
# Out:  $CIMT_DIR/CIMT-CICM_{Imp,Tot_Exp,Dom_Exp}_<year>.zip
# Run:  Rscript R/01_acquire/cimt_bulk_download.R
#
# Download only: the CSVs inside are ~7x the zips, so the pipeline reads
# selectively with unz() rather than unpacking. See README for the ODPFN table
# numbering and the HS10-imports / HS8-exports asymmetry.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

# CIMT_DIR can point outside the repo - this is ~1-2 GB of raw files that
# StatCan will serve again at any time.
OUT_DIR <- CIMT_DIR
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# THE MOST IMPORTANT LINE HERE. download.file() defaults to a 60-SECOND
# timeout, which silently caps the size of file this can fetch: at ~308 KB/s
# every 14 MB export succeeded and almost every 65 MB import failed. It looked
# like network instability and was wrapped in a retry loop, which then failed
# ten times for the same deterministic reason. The tell was that the split
# fell along file size, not along time or year.
options(timeout = 1800)

BASE_URL <- "https://www150.statcan.gc.ca/n1/pub/71-607-x/2021004/zip"

# 2002 matches the floor of the US side. CIMT goes back to 1992 if a longer
# Canadian series is ever wanted.
if (!exists("YEARS")) YEARS <- 2002:2026

if (!exists("DIRECTIONS")) {
  # THREE directions. Canada splits Total Exports (Canadian-made plus
  # re-exports) and Domestic Exports (Canadian-made only) into separate
  # archives, where Census uses a DF row flag. Keeping both gives re-exports
  # as (Tot - Dom); a re-exported good was priced by a foreign producer, so
  # folding it into a unit value contaminates the exporter-pricing question.
  DIRECTIONS <- c(imp     = "CIMT-CICM_Imp",
                  exp     = "CIMT-CICM_Tot_Exp",
                  dom_exp = "CIMT-CICM_Dom_Exp")
}

# Resumable, and mode = "wb" is required or R corrupts the zip on Windows by
# translating line endings. The .part file is renamed only on success, so an
# interrupted download cannot leave a truncated zip looking complete.
download_one <- function(url, dest) {
  if (file.exists(dest)) {
    cat(sprintf("skip   %-34s (already on disk)\n", basename(dest)))
    return(invisible(TRUE))
  }

  tmp <- paste0(dest, ".part")

  ok <- tryCatch({
    utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    cat(sprintf("FAIL   %-34s %s\n", basename(dest), conditionMessage(e)))
    FALSE
  })

  if (!ok) {
    if (file.exists(tmp)) file.remove(tmp)
    return(invisible(FALSE))
  }

  # A 404 page or truncated transfer will not open as a zip. Checking here
  # stops a bad file being kept and then skipped as "already on disk".
  contents <- tryCatch(utils::unzip(tmp, list = TRUE), error = function(e) NULL)

  if (is.null(contents) || nrow(contents) == 0) {
    cat(sprintf("FAIL   %-34s (not a readable zip - deleted)\n",
                basename(dest)))
    file.remove(tmp)
    return(invisible(FALSE))
  }

  file.rename(tmp, dest)
  cat(sprintf("saved  %-34s %6.1f MB, %d files inside\n",
              basename(dest), file.size(dest) / 1e6, nrow(contents)))
  invisible(TRUE)
}

for (dir_code in names(DIRECTIONS)) {
  for (yr in YEARS) {
    stem <- sprintf("%s_%d.zip", DIRECTIONS[[dir_code]], yr)
    download_one(file.path(BASE_URL, stem), file.path(OUT_DIR, stem))
  }
}

cat("\n--- on disk ---\n")
# Report what is actually on disk, so gaps are visible at a glance.
for (dir_code in names(DIRECTIONS)) {
  have <- c()
  for (yr in YEARS) {
    f <- file.path(OUT_DIR, sprintf("%s_%d.zip", DIRECTIONS[[dir_code]], yr))
    if (file.exists(f)) have <- c(have, yr)
  }
  cat(sprintf("%s: %d files", dir_code, length(have)))
  if (length(have)) cat(sprintf("  (%d-%d)", min(have), max(have)))
  missing <- setdiff(YEARS, have)
  if (length(missing)) {
    cat(sprintf("  MISSING: %s", paste(missing, collapse = ", ")))
  }
  cat("\n")
}
