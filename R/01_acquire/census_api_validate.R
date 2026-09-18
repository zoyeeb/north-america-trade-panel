# census_api_validate.R
# Verifies every file census_api_pull.R produced, before any of it is used.
# Offline except the optional cross-check at the end. Safe to re-run any time.
#
# In:   $DATA/census_api/*.csv
# Out:  a report on stdout; nothing is written
# Run:  Rscript R/01_acquire/census_api_validate.R
#       RUN_CROSSCHECK <- TRUE; source(...)   # adds the network check
#
# Separate from the pull because a pull checks what it thought of when it wrote
# a file, while this re-reads everything on disk from scratch - including files
# written by earlier versions with different checks. Required fields are
# asserted; extra fields are reported, never failed.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

# Named CENSUS_DIR, not DATA_DIR, so it cannot shadow config.R's DATA_DIR.
CENSUS_DIR <- na_data("census_api")

# The two endpoints are not symmetric: QTY_1_MO exists only on exports. Both
# import value concepts are required - the related-party reconciliation needs
# CON_VAL_MO specifically.
REQUIRED_EXP <- c("E_COMMODITY", "ALL_VAL_MO",
                  "QTY_1_MO", "QTY_1_MO_FLAG", "UNIT_QY1",
                  "QTY_2_MO", "QTY_2_MO_FLAG", "UNIT_QY2",
                  "DF",
                  "MONTH", "YEAR", "COMM_LVL", "CTY_CODE")

DF_EXPECTED <- c("domestic", "foreign")

REQUIRED_IMP <- c("I_COMMODITY", "GEN_VAL_MO", "CON_VAL_MO",
                  "GEN_QY1_MO", "GEN_QY1_MO_FLAG",
                  "CON_QY1_MO", "CON_QY1_MO_FLAG", "UNIT_QY1",
                  "GEN_QY2_MO", "GEN_QY2_MO_FLAG",
                  "CON_QY2_MO", "CON_QY2_MO_FLAG", "UNIT_QY2",
                  "GEN_CIF_MO", "CON_CIF_MO",
                  "GEN_CHA_MO", "CON_CHA_MO",
                  "MONTH", "YEAR", "COMM_LVL", "CTY_CODE")

CTY_EXPECTED <- c(can = "1220", mex = "2010")

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

results <- list()
n_fail <- 0

fail <- function(file, msg) {
  results[[length(results) + 1]] <<- list(file = file, level = "FAIL", msg = msg)
  n_fail <<- n_fail + 1
}
warn <- function(file, msg) {
  results[[length(results) + 1]] <<- list(file = file, level = "WARN", msg = msg)
}

cat(strrep("=", 72), "\n")
cat("SECTION 1 - Inventory\n")
cat(strrep("=", 72), "\n")

# Inventory is checked first: a cell that was never pulled passes every
# per-file test ever written, because there is no file to test.
YEARS <- PANEL_YEARS
expected_files <- character(0)
for (d in c("exp", "imp")) {
  for (c in c("can", "mex")) {
    expected_files <- c(expected_files,
                        sprintf("census_%s_%s_%d.csv", c, d, YEARS))
  }
}

present <- basename(list.files(CENSUS_DIR, pattern = "^census_.*\\.csv$"))
missing <- setdiff(expected_files, present)
partial <- list.files(CENSUS_DIR, pattern = "\\.partial$")

cat(sprintf("expected cells : %d\n", length(expected_files)))
cat(sprintf("present        : %d\n", length(intersect(expected_files, present))))
cat(sprintf("missing        : %d\n", length(missing)))
cat(sprintf(".partial files : %d  (incomplete years awaiting retry)\n",
            length(partial)))

if (length(missing) > 0) {
  cat("\nMISSING CELLS - the panel is incomplete until these are pulled:\n")
  for (m in missing) cat("   ", m, "\n")
}
if (length(partial) > 0) {
  cat("\n.partial files present - these years are incomplete and were NOT\n")
  cat("accepted. Re-run the pull to retry them:\n")
  for (p in partial) cat("   ", p, "\n")
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 2 - Per-file structural and coherence checks\n")
cat(strrep("=", 72), "\n")

files <- sort(list.files(CENSUS_DIR, pattern = "^census_.*\\.csv$",
                         full.names = TRUE))
row_counts <- list()

for (f in files) {
  bn <- basename(f)

  parts <- strsplit(sub("^census_", "", sub("\\.csv$", "", bn)), "_")[[1]]
  f_cty <- parts[1]; f_dir <- parts[2]; f_yr <- as.integer(parts[3])

  d <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, colClasses = "character"),
    error = function(e) NULL
  )
  if (is.null(d)) { fail(bn, "file could not be parsed as CSV"); next }
  if (nrow(d) == 0) { fail(bn, "file is empty"); next }

  req <- if (f_dir == "exp") REQUIRED_EXP else REQUIRED_IMP
  miss_f <- setdiff(req, names(d))
  if (length(miss_f)) {
    fail(bn, paste("missing required field(s):", paste(miss_f, collapse = ", ")))
  }
  extra <- setdiff(names(d), req)
  if (length(extra)) {
    warn(bn, paste("extra field(s) present (fine):", paste(extra, collapse = ", ")))
  }

  code_col <- if (f_dir == "exp") "E_COMMODITY" else "I_COMMODITY"
  if (!code_col %in% names(d)) next   # already failed above

  # A file that silently came back at HS6 would look valid otherwise.
  lens <- unique(nchar(d[[code_col]]))
  if (!identical(lens, 10L)) {
    fail(bn, paste0("commodity codes are not all 10 characters; lengths found: ",
                    paste(sort(lens), collapse = ", ")))
  }
  if (!any(grepl("^0", d[[code_col]]))) {
    warn(bn, "no commodity code starts with '0' - possible leading-zero loss")
  }

  if ("COMM_LVL" %in% names(d)) {
    lv <- unique(d$COMM_LVL)
    if (!identical(lv, "HS10")) {
      fail(bn, paste("COMM_LVL is not uniformly HS10:", paste(lv, collapse = ", ")))
    }
  }

  if ("CTY_CODE" %in% names(d)) {
    cc <- unique(d$CTY_CODE)
    if (!identical(cc, CTY_EXPECTED[[f_cty]])) {
      fail(bn, sprintf("CTY_CODE %s does not match filename country '%s' (expect %s)",
                       paste(cc, collapse = ","), f_cty, CTY_EXPECTED[[f_cty]]))
    }
  }
  if ("YEAR" %in% names(d)) {
    yy <- unique(d$YEAR)
    if (!identical(yy, as.character(f_yr))) {
      fail(bn, sprintf("YEAR %s does not match filename year %d",
                       paste(yy, collapse = ","), f_yr))
    }
  }

  # The check that would have caught the 9-month file saved as complete.
  # Against the sample cutoff, not today's date, so the final year does not
  # start failing forever once the calendar moves past it.
  months <- sort(unique(d$MONTH))
  want_m <- expected_month_labels(f_yr)
  if (!identical(months, want_m)) {
    bits <- character(0)
    miss_m  <- setdiff(want_m, months)
    extra_m <- setdiff(months, want_m)
    if (length(miss_m))  bits <- c(bits, paste("missing", paste(miss_m, collapse = ",")))
    if (length(extra_m)) bits <- c(bits, paste("unexpected", paste(extra_m, collapse = ",")))
    fail(bn, sprintf("has %d month(s), sample expects %d (%s-%s); %s",
                     length(months), length(want_m),
                     want_m[1], want_m[length(want_m)],
                     paste(bits, collapse = "; ")))
  }

  # On exports the grain is (commodity, month, DF) - each commodity-month
  # legitimately appears twice, so keying without DF fails every export file.
  k <- if (f_dir == "exp" && "DF" %in% names(d)) {
         paste(d[[code_col]], d$MONTH, d$DF)
       } else {
         paste(d[[code_col]], d$MONTH)
       }
  if (anyDuplicated(k)) {
    fail(bn, sprintf("%d duplicate rows at the file's grain",
                     sum(duplicated(k))))
  }

  if (f_dir == "exp" && "DF" %in% names(d)) {
    # If the aggregate row survived into a file, every sum over it doubles.
    odd <- setdiff(unique(d$DF), DF_EXPECTED)
    if (length(odd) > 0) {
      fail(bn, sprintf("DF contains unexpected value(s): %s - the aggregate row must be dropped, or every sum double-counts",
                       paste(odd, collapse = ", ")))
    }
  }

  if (f_dir == "imp" && all(c("GEN_VAL_MO", "GEN_CHA_MO", "GEN_CIF_MO") %in% names(d))) {
    # CIF = customs value + charges, an exact identity. It tests three columns
    # against each other, so a misaligned one shows up here and nowhere else.
    gv <- suppressWarnings(as.numeric(d$GEN_VAL_MO))
    gc <- suppressWarnings(as.numeric(d$GEN_CHA_MO))
    gf <- suppressWarnings(as.numeric(d$GEN_CIF_MO))
    ok <- !is.na(gv) & !is.na(gc) & !is.na(gf)
    off <- sum(ok & abs((gv + gc) - gf) > 0.5)
    if (off > 0) {
      fail(bn, sprintf("%d rows where GEN_VAL_MO + GEN_CHA_MO != GEN_CIF_MO",
                       off))
    }
  }

  val_col <- if (f_dir == "exp") "ALL_VAL_MO" else "GEN_VAL_MO"
  if (val_col %in% names(d)) {
    v <- suppressWarnings(as.numeric(d[[val_col]]))
    if (any(is.na(v))) {
      fail(bn, sprintf("%d non-numeric values in %s", sum(is.na(v)), val_col))
    } else if (any(v < 0)) {
      fail(bn, sprintf("%d negative values in %s", sum(v < 0), val_col))
    }
  }

  q_col <- if (f_dir == "exp") "QTY_1_MO" else "GEN_QY1_MO"
  if (all(c(q_col, "UNIT_QY1") %in% names(d))) {
    q <- suppressWarnings(as.numeric(d[[q_col]]))
    has_q <- !is.na(q) & q > 0
    no_unit <- has_q & (is.na(d$UNIT_QY1) | d$UNIT_QY1 %in% c("", "-"))
    if (any(no_unit)) {
      warn(bn, sprintf("%d rows have quantity > 0 but no unit of measure",
                       sum(no_unit)))
    }
  }

  row_counts[[bn]] <- nrow(d)
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 3 - Cross-file coherence (year-over-year row counts)\n")
cat(strrep("=", 72), "\n")

# A truncated year still has 12 valid months, just fewer rows in each, so no
# per-file check can see it. Adjacent years should be similar in size.
for (d_ in c("exp", "imp")) {
  for (c_ in c("can", "mex")) {
    ser <- sprintf("census_%s_%s_%d.csv", c_, d_, YEARS)
    ser <- ser[ser %in% names(row_counts)]
    if (length(ser) < 2) next
    n <- unlist(row_counts[ser])
    cat(sprintf("\n  %s_%s : %s\n", c_, d_,
                paste(sprintf("%d", n), collapse = " ")))
    for (i in 2:length(n)) {
      yr_i <- as.integer(sub(".*_(\\d{4})\\.csv$", "\\1", ser[i]))
      # The final year is short by design and would always trip the ratio test.
      if (yr_i >= END_YEAR) next
      ratio <- n[i] / n[i - 1]
      if (ratio < 0.8 || ratio > 1.25) {
        warn(ser[i], sprintf("row count %d is %.0f%% of prior year (%d) - check for truncation",
                             n[i], 100 * ratio, n[i - 1]))
      }
    }
  }
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 4 - Commodity description coverage\n")
cat(strrep("=", 72), "\n")

for (d_ in c("exp", "imp")) {
  # This section exists because its absence let a real bug sit: the lookups
  # covered barely half the codes, and nothing failed because nothing looked.
  code_var <- if (d_ == "exp") "E_COMMODITY" else "I_COMMODITY"
  lk_file  <- file.path(CENSUS_DIR, sprintf("code_lookup_%s.csv", d_))
  bn_lk    <- basename(lk_file)

  if (!file.exists(lk_file)) {
    fail(bn_lk, "lookup file is missing - commodity codes have no descriptions")
    next
  }

  ser <- list.files(CENSUS_DIR,
                    pattern = sprintf("^census_[a-z]+_%s_[0-9]{4}\\.csv$", d_),
                    full.names = TRUE)
  if (length(ser) == 0) next

  in_data <- character(0)
  for (f in ser) {
    hdr <- names(read.csv(f, nrows = 1, colClasses = "character"))
    cc  <- ifelse(hdr == code_var, "character", "NULL")
    in_data <- union(in_data, read.csv(f, colClasses = cc)[[code_var]])
  }

  lk  <- read.csv(lk_file, stringsAsFactors = FALSE, colClasses = "character")
  gap <- setdiff(in_data, lk[[code_var]])

  cat(sprintf("\n  %s : %d codes in data, %d in lookup, %d undescribed (%.1f%%)\n",
              d_, length(in_data), nrow(lk), length(gap),
              100 * length(gap) / max(1, length(in_data))))

  if (length(gap) > 0) {
    fail(bn_lk, sprintf("%d of %d commodity codes (%.1f%%) have no description - re-run the pull to rebuild",
                        length(gap), length(in_data),
                        100 * length(gap) / length(in_data)))
  }

  dup <- unique(lk[[code_var]][duplicated(lk[[code_var]])])
  if (length(dup) > 0) {
    fail(bn_lk, sprintf("%d code(s) carry more than one description (e.g. %s) - joins would duplicate rows",
                        length(dup), paste(head(dup, 3), collapse = ", ")))
  }
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 5 - Findings\n")
cat(strrep("=", 72), "\n")

if (length(results) == 0) {
  cat("No issues found.\n")
} else {
  for (lv in c("FAIL", "WARN")) {
    sel <- Filter(function(r) r$level == lv, results)
    if (length(sel) == 0) next
    cat(sprintf("\n%s (%d):\n", lv, length(sel)))

    # Identical warnings are grouped. The first run emitted the same benign one
    # 37 times, which would have buried a genuine finding. FAILs stay
    # individual - knowing which file matters more than brevity.
    msgs <- vapply(sel, function(r) r$msg, character(1))
    if (lv == "FAIL") {
      for (r in sel) cat(sprintf("  %-30s %s\n", r$file, r$msg))
    } else {
      for (m in unique(msgs)) {
        hit <- vapply(sel, function(r) identical(r$msg, m), logical(1))
        n <- sum(hit)
        if (n == 1) {
          cat(sprintf("  %-30s %s\n", sel[hit][[1]]$file, m))
        } else {
          cat(sprintf("  [%d files] %s\n", n, m))
          if (n <= 5) {
            fs <- vapply(sel[hit], function(r) r$file, character(1))
            cat(sprintf("      %s\n", paste(fs, collapse = ", ")))
          }
        }
      }
    }
  }
}

cat(sprintf("\n%d file(s) checked, %d FAIL, %d WARN\n",
            length(files), n_fail, length(results) - n_fail))

if (length(missing) > 0 || length(partial) > 0) {
  cat("\nNOTE: the set is INCOMPLETE. Passing checks below apply only to the\n")
  cat("files that exist - see SECTION 1 for what is still missing.\n")
}

# Everything above is internal consistency: it proves the files are coherent,
# not correct. The only test of correctness is to ask the source again.
if (!exists("RUN_CROSSCHECK")) RUN_CROSSCHECK <- FALSE

if (RUN_CROSSCHECK) {
  cat("\n", strrep("=", 72), "\n", sep = "")
  cat("SECTION 6 - Independent re-query cross-check\n")
  cat(strrep("=", 72), "\n")

  library(httr2)
  key <- Sys.getenv("CENSUS_API_KEY")

  tf <- file.path(CENSUS_DIR, "census_can_exp_2015.csv")
  if (file.exists(tf) && nchar(key) > 0) {
    d <- read.csv(tf, stringsAsFactors = FALSE, colClasses = "character")
    pick <- d[d$MONTH == "06", ][1, ]

    resp <- request("https://api.census.gov/data/timeseries/intltrade/exports/hs") |>
      req_url_query(get = "E_COMMODITY,ALL_VAL_MO", YEAR = 2015, MONTH = "06",
                    COMM_LVL = "HS10", CTY_CODE = "1220",
                    E_COMMODITY = pick$E_COMMODITY, key = key) |>
      req_perform()
    raw <- resp_body_json(resp, simplifyVector = TRUE)

    fresh <- raw[-1, , drop = FALSE]
    fresh_val <- fresh[fresh[, 1] == pick$E_COMMODITY, 2][1]

    cat(sprintf("  commodity      : %s\n", pick$E_COMMODITY))
    cat(sprintf("  stored value   : %s\n", pick$ALL_VAL_MO))
    cat(sprintf("  re-queried     : %s\n", fresh_val))
    cat(sprintf("  MATCH          : %s\n",
                identical(as.character(fresh_val), pick$ALL_VAL_MO)))
  } else {
    cat("  skipped - reference file or API key not available\n")
  }
}
