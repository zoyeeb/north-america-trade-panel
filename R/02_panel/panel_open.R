# panel_open.R
# Two ways to work with the 19.3M-row panel without loading it.
#
# In:   $DATA/panel/
# Out:  panel_slice(), panel_open(), panel_to_parquet(), PANEL_TYPES
# Run:  na_source("R/02_panel/panel_open.R")
#
#   d <- panel_slice(partner = "CA", direction = "imp", years = 2010:2025)
#
#   panel_to_parquet()                    # one-time, ~5 min
#   d <- panel_open() |> filter(partner == "CA") |> collect()
#
# Use the slice for a one-off look; convert to Parquet if you will come back
# more than a couple of times, since a query then touches only the partitions
# and columns it names. Parquet is NOT smaller here - 319 MB against 248 MB -
# the win is query time and memory, not disk.
#
# `year` is an integer partition key; `period` is character. filter(year >=
# 2010), not filter(year >= "2010").

suppressMessages({
  library(vroom)
  library(arrow)
})

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

PANEL_DIR   <- na_data("panel")
PARQUET_DIR <- file.path(PANEL_DIR, "parquet")

PANEL_TYPES <- vroom::cols(
  period        = "c", reporter   = "c", partner    = "c", flow_reporter = "c",
  direction     = "c", basis      = "c", hs_code    = "c", hs_level      = "c",
  hs6           = "c", value      = "d", currency   = "c", quantity_1    = "d",
  unit_1        = "c", quantity_2 = "d", unit_2     = "c", source        = "c"
)

SOURCE_SPEC <- list(
  uto    = list(file = "uto_core.csv.gz",    reporter = "US", y0 = 2002, y1 = 2009),
  census = list(file = "census_core.csv.gz", reporter = "US", y0 = 2010, y1 = 2026),
  cimt   = list(file = "cimt_core.csv.gz",   reporter = "CA", y0 = 2002, y1 = 2026)
)

panel_slice <- function(reporter = NULL, partner = NULL, direction = NULL,
                        years = NULL, cols = NULL) {

  keep <- cols
  if (!is.null(cols)) {
    bad <- setdiff(cols, names(PANEL_TYPES$cols))
    if (length(bad))
      stop("not panel columns: ", paste(bad, collapse = ", "),
           "\n  available: ", paste(names(PANEL_TYPES$cols), collapse = ", "))
    cols <- union(cols, c(if (!is.null(partner))   "partner",
                          if (!is.null(direction)) "direction",
                          if (!is.null(years))     "period"))
  }

  want <- Filter(function(s) {
    (is.null(reporter) || s$reporter == reporter) &&
    (is.null(years)    || (min(years) <= s$y1 && max(years) >= s$y0))
  }, SOURCE_SPEC)

  if (identical(partner, "MX") && identical(reporter, "CA"))
    want <- want["cimt"]
  if (!length(want))
    stop("no source file covers reporter=", reporter %||% "any",
         " years=", if (is.null(years)) "any" else paste0(min(years), "-", max(years)))

  message("reading ", length(want), " source file(s): ", paste(names(want), collapse = ", "))

  parts <- list()
  for (nm in names(want)) {
    f <- file.path(PANEL_DIR, want[[nm]]$file)
    if (!file.exists(f))
      stop("missing ", f, " - run the extractor for '", nm, "' first")

    d <- if (is.null(cols)) {
      vroom(f, col_types = PANEL_TYPES, progress = FALSE)
    } else {
      vroom(f, col_types = PANEL_TYPES, col_select = all_of(cols), progress = FALSE)
    }

    if (!is.null(partner)) {
      if (!"partner" %in% names(d)) stop("internal: 'partner' was not read")
      d <- d[d$partner == partner, ]
    }
    if (!is.null(direction)) {
      if (!"direction" %in% names(d)) stop("internal: 'direction' was not read")
      d <- d[!is.na(d$direction) & d$direction == direction, ]
    }
    if (!is.null(years)) {
      if (!"period" %in% names(d)) stop("internal: 'period' was not read")
      d <- d[as.integer(substr(d$period, 1, 4)) %in% years, ]
    }

    message(sprintf("  %-8s %s rows", nm, format(nrow(d), big.mark = ",")))
    parts[[nm]] <- d
    rm(d); invisible(gc(verbose = FALSE))
  }

  out <- do.call(rbind, parts)
  if (!is.null(keep)) out <- out[, keep, drop = FALSE]
  message("total: ", format(nrow(out), big.mark = ","), " rows")
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

panel_to_parquet <- function(force = FALSE) {
  if (dir.exists(PARQUET_DIR)) {
    if (!force)
      stop(PARQUET_DIR, " already exists.\n",
           "  Use panel_open() to query it, or panel_to_parquet(force = TRUE) ",
           "to rebuild it from scratch.")
    message("force = TRUE: removing the existing dataset first")
    unlink(PARQUET_DIR, recursive = TRUE)
  }
  dir.create(PARQUET_DIR, recursive = TRUE, showWarnings = FALSE)

  totals <- list()
  for (nm in names(SOURCE_SPEC)) {
    f <- file.path(PANEL_DIR, SOURCE_SPEC[[nm]]$file)
    if (!file.exists(f)) { message("SKIP ", nm, " - ", f, " not found"); next }

    message("reading ", nm, " ...")
    d <- vroom(f, col_types = PANEL_TYPES, progress = FALSE)
    d$year <- substr(d$period, 1, 4)   # partition key; period stays character

    totals[[nm]] <- list(n = nrow(d), v = tapply(d$value, d$currency, sum))

    write_dataset(
      d, PARQUET_DIR,
      format            = "parquet",
      partitioning      = c("reporter", "partner", "flow_reporter", "year"),
      basename_template = paste0(nm, "-part-{i}.parquet"),
      existing_data_behavior = "overwrite"
    )
    message(sprintf("  %-8s %s rows written", nm, format(nrow(d), big.mark = ",")))
    rm(d); invisible(gc(verbose = FALSE))
  }

  ds <- open_dataset(PARQUET_DIR)
  got_n <- nrow(ds)
  exp_n <- sum(vapply(totals, function(t) t$n, numeric(1)))
  if (got_n != exp_n)
    stop("parquet has ", format(got_n, big.mark = ","), " rows, expected ",
         format(exp_n, big.mark = ","), " - the conversion lost or duplicated rows")

  got_v <- as.data.frame(
    dplyr::collect(dplyr::summarise(dplyr::group_by(ds, currency),
                                    v = sum(value, na.rm = TRUE))))
  for (nm in names(totals)) for (cur in names(totals[[nm]]$v)) {
    exp_v <- sum(vapply(totals, function(t) if (cur %in% names(t$v)) t$v[[cur]] else 0,
                        numeric(1)))
    act_v <- got_v$v[got_v$currency == cur]
    if (!length(act_v) || abs(exp_v - act_v) > 1e-6 * max(abs(exp_v), 1))
      stop("parquet ", cur, " total is ", format(act_v, scientific = FALSE),
           ", expected ", format(exp_v, scientific = FALSE))
  }

  message("\nverified: ", format(got_n, big.mark = ","),
          " rows and every currency total match the CSVs")
  message("size: ", round(sum(file.size(list.files(PARQUET_DIR, recursive = TRUE,
          full.names = TRUE))) / 1e6), " MB  (the .csv.gz is 248 MB - Parquet ",
          "is BIGGER here; the win is query speed and memory, not disk)")
  message("open it with: panel_open()   -- note year is an INTEGER: year >= 2010")
  invisible(PARQUET_DIR)
}

panel_open <- function() {
  if (!dir.exists(PARQUET_DIR))
    stop("no Parquet dataset at ", PARQUET_DIR,
         "\n  Build it once with:  panel_to_parquet()")
  open_dataset(PARQUET_DIR)
}

panel_xlsx <- function(d, path) {
  LIMIT <- 1048576L - 1L   # one row goes to the header
  if (nrow(d) > LIMIT)
    stop(format(nrow(d), big.mark = ","), " rows exceeds Excel's limit of ",
         format(LIMIT + 1L, big.mark = ","), " by ",
         format(nrow(d) - LIMIT, big.mark = ","), ".\n",
         "  Excel would open it TRUNCATED, with no warning. Narrow the slice ",
         "or aggregate first\n  (HS6 instead of HS10, or annual instead of ",
         "monthly).")
  writexl::write_xlsx(d, path)
  message("wrote ", path, " (", format(nrow(d), big.mark = ","), " rows)")
  invisible(path)
}

message("panel_open.R loaded. Functions:")
message("  panel_slice(reporter, partner, direction, years, cols)   read from .csv.gz")
message("  panel_to_parquet()  /  panel_open()                      convert, then query lazily")
message("  panel_xlsx(d, path)                                      export a slice, row-limit guarded")
