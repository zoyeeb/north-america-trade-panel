# census_api_pull.R
# US side of the panel: native HS10, monthly, Canada and Mexico, both
# directions, from the Census International Trade API. Resumable - each
# (country, direction, year) is its own file and is skipped once complete.
#
# In:   CENSUS_API_KEY
# Out:  $DATA/census_api/census_{can,mex}_{exp,imp}_{year}.csv
#       $DATA/census_api/code_lookup_{exp,imp}.csv
# Run:  Rscript R/01_acquire/census_api_pull.R
#
# One year per call: omitting MONTH returns all 12, and asking for two years at
# once returns HTTP 500. The endpoints are not symmetric - exports use
# E_COMMODITY/ALL_VAL_MO/QTY_1_MO, imports use I_COMMODITY and split value into
# GEN_ (everything entering the country) and CON_ (clearing into the domestic
# economy). A wrong name returns HTTP 400; confusing GEN_ with CON_ does not.

library(httr2)

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

census_key <- require_key("CENSUS_API_KEY")

if (!exists("OUT_DIR")) OUT_DIR <- na_data("census_api")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("writing to   : %s\n\n", normalizePath(OUT_DIR)))

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

if (!exists("COUNTRIES"))  COUNTRIES  <- c(can = "1220", mex = "2010")
if (!exists("YEARS"))      YEARS      <- PANEL_YEARS   # from census_config.R
if (!exists("DIRECTIONS")) DIRECTIONS <- c("exp", "imp")

FIELDS_EXP <- c(
  "E_COMMODITY",
  "ALL_VAL_MO",                                   # export value
  "QTY_1_MO", "QTY_1_MO_FLAG", "UNIT_QY1",        # primary quantity + unit
  "QTY_2_MO", "QTY_2_MO_FLAG", "UNIT_QY2",        # secondary quantity + unit
  "AIR_VAL_MO", "VES_VAL_MO", "CNT_VAL_MO",       # value split by transport mode
  "AIR_WGT_MO", "VES_WGT_MO", "CNT_WGT_MO",       # weight (air/vessel only)
  "DF",                                           # domestic vs re-export
  "MONTH"
)

FIELDS_IMP <- c(
  "I_COMMODITY",
  "GEN_VAL_MO", "CON_VAL_MO", "DUT_VAL_MO",       # general / consumption / dutiable
  "GEN_QY1_MO", "GEN_QY1_MO_FLAG",
  "CON_QY1_MO", "CON_QY1_MO_FLAG", "UNIT_QY1",
  "GEN_QY2_MO", "GEN_QY2_MO_FLAG",
  "CON_QY2_MO", "CON_QY2_MO_FLAG", "UNIT_QY2",
  "AIR_VAL_MO", "VES_VAL_MO", "CNT_VAL_MO",
  "AIR_WGT_MO", "VES_WGT_MO", "CNT_WGT_MO",
  "GEN_CIF_MO", "CON_CIF_MO",                     # value incl. freight+insurance
  "GEN_CHA_MO", "CON_CHA_MO",                     # the freight+insurance itself
  "MONTH"
)

expected_columns <- function(direction) {
  f <- if (direction == "exp") FIELDS_EXP else FIELDS_IMP
  c(f, "YEAR", "COMM_LVL", "CTY_CODE")
}

census_pull <- function(direction, cty_code, year, month = NULL) {
  endpoint <- if (direction == "exp") "exports" else "imports"
  fields   <- if (direction == "exp") FIELDS_EXP else FIELDS_IMP

  # MONTH is echoed back when used as a filter; requesting it too would
  # duplicate the column and give that one file a schema nothing else has.
  if (!is.null(month)) fields <- setdiff(fields, "MONTH")

  args <- list(
    get      = paste(fields, collapse = ","),
    YEAR     = year,
    COMM_LVL = "HS10",       # native US level - do NOT aggregate to HS6 here
    CTY_CODE = cty_code,
    key      = census_key
  )
  if (!is.null(month)) args$MONTH <- month

  req <- request(paste0("https://api.census.gov/data/timeseries/intltrade/",
                        endpoint, "/hs")) |>
    req_url_query(!!!args) |>
    req_timeout(600)

  resp <- req_perform(req)

  # 204 = the period genuinely has no data. Every pre-2010 year does this.
  if (resp_status(resp) == 204) return(NULL)

  raw <- resp_body_json(resp, simplifyVector = TRUE)
  df  <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- raw[1, ]
  df
}

# Whole year first, falling back to twelve monthly requests. Returns a status
# so "server said 204" and "every request errored" stay distinct. When both
# returned NULL, a dropped connection logged real Canadian import years as
# empty.
pull_year <- function(direction, cty_code, year) {

  res <- tryCatch(census_pull(direction, cty_code, year),
                  error = function(e) e)

  if (!inherits(res, "error")) {
    if (is.null(res)) return(list(data = NULL, status = "empty"))
    return(list(data = res, status = "ok"))
  }

  cat(sprintf("       year-level request failed (%s)\n       falling back to month-by-month\n",
              conditionMessage(res)))

  parts <- list()
  n_failed <- 0
  for (m in sprintf("%02d", 1:12)) {
    p <- tryCatch(census_pull(direction, cty_code, year, m),
                  error = function(e) {
                    cat(sprintf("       month %s failed: %s\n", m,
                                conditionMessage(e)))
                    n_failed <<- n_failed + 1
                    NULL
                  })
    if (!is.null(p) && nrow(p) > 0) parts[[m]] <- p
  }

  if (length(parts) == 0) {
    return(list(data = NULL,
                status = if (n_failed > 0) "failed" else "empty"))
  }

  list(data = do.call(rbind, parts), status = "ok")
}

# The 150-char text repeats on every
# row, ~12 MB per file. It is swept across years because HS10 codes are added
# and retired annually, so one vintage described only ~55% of the panel.
pull_code_lookup <- function(direction, year, cty_code, month = "12") {
  endpoint  <- if (direction == "exp") "exports" else "imports"
  code_var  <- if (direction == "exp") "E_COMMODITY" else "I_COMMODITY"
  desc_var  <- paste0(code_var, "_LDESC")

  req <- request(paste0("https://api.census.gov/data/timeseries/intltrade/",
                        endpoint, "/hs")) |>
    req_url_query(get = paste(code_var, desc_var, sep = ","),
                  YEAR = year, MONTH = month, COMM_LVL = "HS10",
                  CTY_CODE = cty_code, key = census_key) |>
    req_timeout(300)

  resp <- req_perform(req)
  if (resp_status(resp) == 204) return(NULL)

  raw <- resp_body_json(resp, simplifyVector = TRUE)
  df  <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- raw[1, ]
  unique(df[, c(code_var, desc_var)])
}

# colClasses = "NULL" on unwanted columns keeps this from parsing ~1 GB of
# values for a job that wants four columns.
codes_in_saved_data <- function(direction) {
  code_var <- if (direction == "exp") "E_COMMODITY" else "I_COMMODITY"
  keep     <- c(code_var, "YEAR", "CTY_CODE", "MONTH")

  files <- list.files(OUT_DIR,
                      pattern = sprintf("^census_[a-z]+_%s_[0-9]{4}\\.csv$",
                                        direction),
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)

  parts <- list()
  for (f in files) {
    hdr <- names(read.csv(f, nrows = 1, colClasses = "character"))
    cc  <- ifelse(hdr %in% keep, "character", "NULL")
    d   <- read.csv(f, colClasses = cc)
    parts[[f]] <- unique(d[, keep])
  }
  unique(do.call(rbind, parts))
}

# Seeded from the existing lookup, so a sweep interrupted by a dead
# connection can only ever add codes rather than write back fewer.
MAX_LOOKUP_FAILURES <- 5

build_code_lookup <- function(direction, present, need, seed = NULL) {
  code_var <- if (direction == "exp") "E_COMMODITY" else "I_COMMODITY"

  lk    <- seed          # start from what is already known, not from nothing
  fails <- 0

  add <- function(part) {
    if (!is.null(part) && nrow(part) > 0) lk <<- unique(rbind(lk, part))
  }
  got <- function() if (is.null(lk)) character(0) else lk[[code_var]]

  if (!is.null(seed)) {
    cat(sprintf("  seeded with %d codes already described\n", nrow(seed)))
  }

  cell_key   <- paste(present$YEAR, present$CTY_CODE, present$MONTH, sep = "|")
  cell_codes <- split(present[[code_var]], cell_key)

  missing <- setdiff(need, got())

  if (length(missing) > 0) {
    counts <- vapply(cell_codes, function(cs) sum(cs %in% missing), integer(1))
    order  <- names(sort(counts[counts > 0], decreasing = TRUE))

    cat(sprintf("  %d codes to find, across %d candidate cells\n",
                length(missing), length(order)))

    for (k in order) {
      if (length(missing) == 0) break
      if (fails >= MAX_LOOKUP_FAILURES) {
        cat(sprintf("  ABORTING SWEEP - %d requests failed in a row. This is a\n",
                    fails))
        cat("  connection problem. Everything fetched so far is kept; re-run\n")
        cat("  when the connection is back and it will continue from here.\n")
        break
      }
      if (!any(cell_codes[[k]] %in% missing)) next

      p   <- strsplit(k, "|", fixed = TRUE)[[1]]
      err <- FALSE
      part <- tryCatch(pull_code_lookup(direction, p[1], p[2], p[3]),
                       error = function(e) {
                         cat(sprintf("       lookup %s/%s/%s failed: %s\n",
                                     p[1], p[2], p[3], conditionMessage(e)))
                         err <<- TRUE
                         NULL
                       })
      fails <- if (err) fails + 1 else 0
      add(part)
      missing <- setdiff(need, got())
    }
  }

  if (length(missing) > 0) {
    cat(sprintf("  INCOMPLETE: %d of %d codes still have no description - e.g. %s\n",
                length(missing), length(need),
                paste(head(missing, 5), collapse = ", ")))
    cat("  Nothing was lost - re-run to continue from here.\n")
  } else {
    cat("  all codes in the panel have a description\n")
  }
  lk
}

# Atomic write. Writing straight to the final name can leave a file with the
# right name, a valid header and only some rows.
write_csv_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  write.csv(x, tmp, row.names = FALSE)

  if (file.exists(path) && !file.remove(path)) {
    unlink(tmp)
    stop("could not replace ", path,
         " - it may be locked by a sync client or open in another program")
  }
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("could not rename ", tmp, " to ", path)
  }
  invisible(TRUE)
}

# Without a breaker one run spent 34 hours retrying a host that would not
# resolve. Stopping early costs nothing.
MAX_CONSECUTIVE_FAILURES <- 3
consecutive_failures <- 0

for (dir_code in DIRECTIONS) {
  for (cty_name in names(COUNTRIES)) {
    for (yr in YEARS) {

      out_file <- file.path(OUT_DIR,
                            sprintf("census_%s_%s_%d.csv", cty_name, dir_code, yr))

      if (file.exists(out_file)) {
        hdr <- names(read.csv(out_file, nrows = 1, colClasses = "character"))
        if (!setequal(hdr, expected_columns(dir_code))) {
          gained <- setdiff(expected_columns(dir_code), hdr)
          lost   <- setdiff(hdr, expected_columns(dir_code))
          cat(sprintf("stale  %-28s (schema changed%s%s) - re-pulling\n",
                      basename(out_file),
                      if (length(gained)) paste0("; now also ", paste(gained, collapse = ",")) else "",
                      if (length(lost))   paste0("; no longer ", paste(lost, collapse = ",")) else ""))
        } else if (yr < END_YEAR) {
          # A middle year cannot change once written.
          cat(sprintf("skip   %s (already on disk)\n", basename(out_file)))
          next
        } else {
          # The boundary year grows as Census publishes. Existence alone
          # froze the 2026 files at 6 months, unfixable by re-running.
          want_m <- expected_month_labels(yr)
          have_m <- unique(read.csv(out_file, colClasses = "character")$MONTH)
          if (setequal(have_m, want_m)) {
            cat(sprintf("skip   %-28s (already on disk, %d/%d months)\n",
                        basename(out_file), length(have_m), length(want_m)))
            next
          }
          cat(sprintf("retry  %-28s (has %d of %d months - re-pulling)\n",
                      basename(out_file), length(have_m), length(want_m)))
        }
      }

      out <- tryCatch(
        pull_year(dir_code, COUNTRIES[[cty_name]], yr),
        error = function(e) {
          cat(sprintf("ERROR  %s_%s_%d : %s\n", cty_name, dir_code, yr,
                      conditionMessage(e)))
          list(data = NULL, status = "failed")
        }
      )

      if (identical(out$status, "failed")) {
        cat(sprintf("FAILED %s_%s_%d - all requests errored, will retry next run\n",
                    cty_name, dir_code, yr))
        consecutive_failures <- consecutive_failures + 1
        if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) {
          stop("Aborting: ", consecutive_failures, " cells failed in a row. ",
               "This is a connection problem, not a data problem - every ",
               "completed cell is already saved, so just re-run when the ",
               "connection is back and it will resume from here.")
        }
        next
      }

      if (identical(out$status, "empty")) {
        cat(sprintf("empty  %s_%s_%d (server returned no content)\n",
                    cty_name, dir_code, yr))
        consecutive_failures <- 0
        next
      }

      res <- out$data

      # All three DF rows share ONE value column, so an unfiltered sum returns
      # exactly twice the truth. The aggregate is spent as a check, then
      # dropped, which makes every later aggregation correct by construction.
      if (dir_code == "exp" && "DF" %in% names(res)) {
        code_col <- "E_COMMODITY"
        k        <- paste(res[[code_col]], res$MONTH)
        v        <- suppressWarnings(as.numeric(res$ALL_VAL_MO))

        is_agg <- res$DF == "-"
        a_val  <- tapply(v[is_agg],  k[is_agg],  sum)
        s_val  <- tapply(v[!is_agg], k[!is_agg], sum)

        common <- intersect(names(a_val), names(s_val))
        bad    <- sum(abs(a_val[common] - s_val[common]) > 0.5)
        orphan <- length(setdiff(names(a_val), names(s_val)))

        if (bad > 0 || orphan > 0) {
          cat(sprintf("MISMATCH %s_%s_%d - %d commodity-months where domestic+foreign != total, %d with no split. NOT SAVED.\n",
                      cty_name, dir_code, yr, bad, orphan))
          next
        }

        res     <- res[!is_agg, , drop = FALSE]
        res$DF  <- ifelse(res$DF == "1", "domestic", "foreign")
      }

      # Trim before checking completeness: Census keeps publishing past the
      # cutoff, and the sample must not drift with the run date.
      keep_m    <- expected_month_labels(yr)
      dropped   <- sum(!res$MONTH %in% keep_m)
      if (dropped > 0) {
        res <- res[res$MONTH %in% keep_m, , drop = FALSE]
        cat(sprintf("       (dropped %d rows past the %s-%s cutoff)\n",
                    dropped, keep_m[1], keep_m[length(keep_m)]))
      }

      n_months  <- length(unique(res$MONTH))
      want      <- expected_months(yr)
      # A SET, not a count: 01-06 plus 08 is seven months and still wrong.
      complete  <- setequal(unique(res$MONTH), keep_m)

      if (!complete) {
        # Never under the real name - resume would skip it forever, freezing
        # a partial year into the panel as a gap that looks like finished work.
        part_file <- paste0(out_file, ".partial")
        write_csv_atomic(res, part_file)

        consecutive_failures <- consecutive_failures + 1
        if (consecutive_failures >= MAX_CONSECUTIVE_FAILURES) {
          stop("Aborting: ", consecutive_failures, " cells in a row came back ",
               "INCOMPLETE. That is a connection problem, not a data problem - ",
               "every finished cell is already saved, so re-run when the ",
               "connection is stable and it will resume from here.")
        }
        cat(sprintf("PARTIAL %-27s %7d rows  %2d/%2d months - saved as .partial, will retry next run\n",
                    basename(out_file), nrow(res), n_months, want))
        next
      }

      write_csv_atomic(res, out_file)

      consecutive_failures <- 0

      stale <- paste0(out_file, ".partial")
      if (file.exists(stale)) {
        file.remove(stale)
        cat(sprintf("       (cleared stale %s)\n", basename(stale)))
      }

      cat(sprintf("saved  %-28s %7d rows  %2d months\n",
                  basename(out_file), nrow(res), n_months))
    }
  }
}

for (dir_code in DIRECTIONS) {
  lk_file  <- file.path(OUT_DIR, sprintf("code_lookup_%s.csv", dir_code))
  code_var <- if (dir_code == "exp") "E_COMMODITY" else "I_COMMODITY"

  cat(sprintf("\ncode lookup [%s]\n", dir_code))

  present <- codes_in_saved_data(dir_code)
  if (is.null(present)) {
    cat("  no data files on disk yet - skipping\n")
    next
  }
  need <- unique(present[[code_var]])
  cat(sprintf("  %d distinct commodity codes in the saved data\n", length(need)))

  if (file.exists(lk_file)) {
    have <- read.csv(lk_file, colClasses = "character")
    gap  <- setdiff(need, have[[code_var]])
    if (length(gap) == 0) {
      cat(sprintf("  keep   %-28s %7d codes (covers all %d)\n",
                  basename(lk_file), nrow(have), length(need)))
      next
    }
    cat(sprintf("  %s covers %d of %d codes - %d missing, rebuilding\n",
                basename(lk_file), length(need) - length(gap), length(need),
                length(gap)))
  }

  seed <- if (exists("FORCE_LOOKUP_REBUILD") && isTRUE(FORCE_LOOKUP_REBUILD)) {
    cat("  FORCE_LOOKUP_REBUILD set - discarding the existing lookup\n")
    NULL
  } else if (file.exists(lk_file)) have else NULL

  lk <- build_code_lookup(dir_code, present, need, seed = seed)
  write_csv_atomic(lk, lk_file)
  cat(sprintf("  saved  %-28s %7d codes\n", basename(lk_file), nrow(lk)))
}

cat("\nDone.\n")
