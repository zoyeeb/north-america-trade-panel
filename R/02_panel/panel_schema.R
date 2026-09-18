# panel_schema.R
# The one place the panel schema is defined. Sourced by all three extractors so
# they cannot drift apart.
#
# Out: CORE_COLS and the shared direction/currency conventions
# Run: na_source("R/02_panel/panel_schema.R")
#
# Census, UTO and CIMT measure the same flows from different sides of the
# border, and the point of holding all three is that they eventually join. If
# each extractor invented its own column names and direction labels, the
# outputs would be silently incompatible.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

CORE_COLS <- c(
  "period",         # YYYYMM, character - kept as text so 200201 never becomes
  "reporter",       # "US" or "CA" - which agency reported the row
  "partner",        # "CA", "MX" or "US"
  "flow_reporter",  # "import"/"export", the reporter's own view. Unambiguous.
  "direction",      # project US-perspective convention, NA for CA-MX
  "basis",          # domestic / foreign / total / NA
  "hs_code",        # native code as reported
  "hs_level",       # "HS10" or "HS8" - so nobody has to infer it from nchar
  "hs6",            # first 6 characters, the level the two sources join on
  "value",          # native currency, numeric
  "currency",       # "USD" or "CAD"
  "quantity_1", "unit_1",
  "quantity_2", "unit_2",
  "source"          # which file this row came from, for tracing back
)

CENSUS_CTY <- c("1220" = "CA", "2010" = "MX")

to_us_direction <- function(reporter, partner, flow_reporter) {
  out <- rep(NA_character_, length(reporter))
  i <- reporter == "US"
  out[i] <- ifelse(flow_reporter[i] == "export", "exp", "imp")
  j <- reporter == "CA" & partner == "US"
  out[j] <- ifelse(flow_reporter[j] == "import", "exp", "imp")
  out
}

to_hs6 <- function(code) {
  code <- trimws(as.character(code))
  ifelse(is.na(code) | nchar(code) < 6, NA_character_, substr(code, 1, 6))
}

rbind_align <- function(parts, who = "") {
  cols <- unique(unlist(lapply(parts, names)))
  ragged <- character(0)
  parts <- lapply(parts, function(p) {
    miss <- setdiff(cols, names(p))
    if (length(miss))
      ragged <<- c(ragged, sprintf("  %s lacks: %s", p$source[1],
                                   paste(miss, collapse = ", ")))
    for (k in miss) p[[k]] <- NA_character_
    p[cols]
  })
  if (length(ragged)) {
    message("*** RAGGED EXTRAS (", who, ") - these cells do not carry every column;")
    message("*** the gap is filled with NA, which is a MISSING COLUMN, not a zero:")
    for (r in ragged) message(r)
  }
  do.call(rbind, parts)
}
check_schema <- function(d, who) {
  problems <- character(0)

  if (!identical(names(d), CORE_COLS)) {
    miss  <- setdiff(CORE_COLS, names(d))
    extra <- setdiff(names(d), CORE_COLS)
    if (length(miss))  problems <- c(problems, paste("missing:", paste(miss, collapse = ", ")))
    if (length(extra)) problems <- c(problems, paste("unexpected:", paste(extra, collapse = ", ")))
    if (!length(miss) && !length(extra))
      problems <- c(problems, "columns present but in the wrong ORDER - rbind would misalign them")
  }

  if ("period" %in% names(d)) {
    bad <- !grepl("^[0-9]{6}$", d$period)
    if (any(bad)) problems <- c(problems,
      sprintf("%d rows have a malformed period (e.g. %s)",
              sum(bad), d$period[which(bad)[1]]))
  }

  vocab <- list(reporter      = c("US", "CA"),
                partner       = c("US", "CA", "MX"),
                flow_reporter = c("import", "export"),
                direction     = c("exp", "imp", NA),
                basis         = c("domestic", "foreign", "total", NA),
                currency      = c("USD", "CAD"),
                hs_level      = c("HS10", "HS8"))
  for (nm in names(vocab)) {
    if (!nm %in% names(d)) next
    bad <- setdiff(unique(d[[nm]]), vocab[[nm]])
    if (length(bad)) problems <- c(problems,
      sprintf("%s has unexpected value(s): %s", nm, paste(utils::head(bad, 5), collapse = ", ")))
  }

  if ("value" %in% names(d) && any(is.na(d$value)))
    problems <- c(problems, sprintf("%d rows have NA value", sum(is.na(d$value))))

  if (all(c("reporter","partner","flow_reporter","direction") %in% names(d))) {
    expect <- to_us_direction(d$reporter, d$partner, d$flow_reporter)
    wrong  <- which(!identical(expect, d$direction) &
                    (is.na(expect) != is.na(d$direction) |
                     (!is.na(expect) & expect != d$direction)))
    if (length(wrong)) problems <- c(problems,
      sprintf("%d rows where direction disagrees with the reporter/partner rule", length(wrong)))
  }

  if (length(problems)) {
    message("SCHEMA FAIL (", who, "):")
    for (p in problems) message("  - ", p)
    stop("schema check failed for ", who, " - not written")
  }
  message("schema OK (", who, "): ", format(nrow(d), big.mark = ","), " rows")
  invisible(TRUE)
}
