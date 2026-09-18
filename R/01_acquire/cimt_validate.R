# cimt_validate.R
# Verifies every zip cimt_bulk_download.R fetched. Offline.
#
# In:   $CIMT_DIR/*.zip
# Out:  a report on stdout; nothing is written
# Run:  Rscript R/01_acquire/cimt_validate.R
#
# Checks presence, that each archive opens, and that the contents match the
# structure the pipeline assumes. It cannot check that the numbers are right -
# that would need an independent source.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

REQUIRED_YEARS <- 2002:2026

EXPECT_IMP <- c(ODPFN014 = "HS10", ODPFN015 = "HS6", ODPFN022 = "HS2")
EXPECT_EXP <- c(ODPFN017 = "HS8",  ODPFN019 = "HS6", ODPFN021 = "HS2")
EXPECT_DOM <- c(ODPFN016 = "HS8",  ODPFN018 = "HS6", ODPFN020 = "HS2")

EXPECT_NCOL <- c(Imp = 8L, Tot_Exp = 7L, Dom_Exp = 8L)

CURRENT_PARTIAL_YEAR <- 2026

results <- list(); n_fail <- 0
fail <- function(f, m) {
  results[[length(results)+1]] <<- list(f=f, lv="FAIL", m=m); n_fail <<- n_fail + 1
}
warn <- function(f, m) {
  results[[length(results)+1]] <<- list(f=f, lv="WARN", m=m)
}

cat(strrep("=", 72), "\n")
cat("SECTION 1 - Inventory\n")
cat(strrep("=", 72), "\n")

zips <- list.files(CIMT_DIR, pattern = "\\.zip$", full.names = TRUE)
cat(sprintf("zips on disk : %d   (%.1f GB)\n", length(zips),
            sum(file.size(zips)) / 1e9))

for (dir_lab in c("Imp", "Tot_Exp", "Dom_Exp")) {
  have <- integer(0)
  for (y in REQUIRED_YEARS) {
    f <- file.path(CIMT_DIR, sprintf("CIMT-CICM_%s_%d.zip", dir_lab, y))
    if (file.exists(f)) have <- c(have, y)
  }
  miss <- setdiff(REQUIRED_YEARS, have)
  cat(sprintf("\n%-8s %d/%d required years\n", dir_lab, length(have),
              length(REQUIRED_YEARS)))
  if (length(miss)) {
    cat("  MISSING: ", paste(miss, collapse = ", "), "\n")
    fail(dir_lab, paste("missing years:", paste(miss, collapse = ", ")))
  }
}

extra <- setdiff(
  as.integer(sub(".*_(\\d{4})\\.zip$", "\\1", basename(zips))),
  REQUIRED_YEARS
)
if (length(extra)) {
  cat(sprintf("\nextra years present (fine, outside panel range): %s\n",
              paste(sort(unique(extra)), collapse = ", ")))
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 2 - Archive integrity and contents\n")
cat(strrep("=", 72), "\n")

ok_count <- 0
for (z in sort(zips)) {
  bn <- basename(z)
  if (grepl("_Imp_", bn)) {
    expect <- EXPECT_IMP; dir_lab <- "Imp"
  } else if (grepl("_Dom_Exp_", bn)) {
    expect <- EXPECT_DOM; dir_lab <- "Dom_Exp"
  } else {
    expect <- EXPECT_EXP; dir_lab <- "Tot_Exp"
  }

  contents <- tryCatch(utils::unzip(z, list = TRUE), error = function(e) NULL)
  if (is.null(contents) || nrow(contents) == 0) {
    fail(bn, "archive will not open (truncated or not a zip)")
    next
  }

  names_in <- basename(contents$Name)
  for (pref in names(expect)) {
    hit <- grep(paste0("^", pref), names_in, value = TRUE)
    if (length(hit) == 0) {
      fail(bn, sprintf("no %s file (expected the %s table)", pref, expect[[pref]]))
    } else if (length(hit) > 1) {
      warn(bn, sprintf("%d files match %s: %s", length(hit), pref,
                       paste(hit, collapse = ", ")))
    }
  }
  stamp <- regmatches(names_in, regexpr("_[0-9]{6}[A-Z][.]csv$", names_in))
  if (length(stamp)) {
    yyyymm  <- substr(sub("^_", "", stamp), 1, 6)
    last_mo <- as.integer(substr(yyyymm, 5, 6))
    f_year  <- max(as.integer(substr(yyyymm, 1, 4)))
    want_mo <- if (f_year >= CURRENT_PARTIAL_YEAR) NA_integer_ else 12L

    if (!is.na(want_mo) && max(last_mo) != want_mo) {
      fail(bn, sprintf("covers only through month %02d, expected 12",
                       max(last_mo)))
    }
    if (is.na(want_mo)) {
      cat(sprintf("  %-34s in-progress year, covers through month %02d\n",
                  bn, max(last_mo)))
    }
  } else {
    warn(bn, "no dated CSV found - cannot verify month coverage")
  }

  ok_count <- ok_count + 1
}
cat(sprintf("%d archives opened successfully\n", ok_count))

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 3 - Spot-check contents (one archive per direction per era)\n")
cat(strrep("=", 72), "\n")

spot <- list(
  list(dir = "Imp",     year = 2003, pref = "ODPFN015", lvl = "HS6"),
  list(dir = "Imp",     year = 2015, pref = "ODPFN015", lvl = "HS6"),
  list(dir = "Imp",     year = 2024, pref = "ODPFN015", lvl = "HS6"),
  list(dir = "Tot_Exp", year = 2003, pref = "ODPFN019", lvl = "HS6"),
  list(dir = "Tot_Exp", year = 2015, pref = "ODPFN019", lvl = "HS6"),
  list(dir = "Tot_Exp", year = 2024, pref = "ODPFN019", lvl = "HS6"),
  list(dir = "Dom_Exp", year = 2003, pref = "ODPFN018", lvl = "HS6"),
  list(dir = "Dom_Exp", year = 2015, pref = "ODPFN018", lvl = "HS6"),
  list(dir = "Dom_Exp", year = 2024, pref = "ODPFN018", lvl = "HS6")
)

for (s in spot) {
  z <- file.path(CIMT_DIR, sprintf("CIMT-CICM_%s_%d.zip", s$dir, s$year))
  if (!file.exists(z)) next
  bn <- basename(z)

  contents <- utils::unzip(z, list = TRUE)
  target <- contents$Name[grepl(paste0("/?", s$pref), contents$Name)][1]
  if (is.na(target)) { fail(bn, paste("spot-check: no", s$pref)); next }

  con <- unz(z, target)
  d <- tryCatch(read.csv(con, nrows = 5000, stringsAsFactors = FALSE,
                         colClasses = "character"),
                error = function(e) NULL)
  if (is.null(d)) { fail(bn, paste("spot-check: could not read", target)); next }

  hs_col <- names(d)[2]          # col 1 is YearMonth, col 2 is the HS code
  code_len <- unique(nchar(d[[hs_col]]))
  ym <- unique(substr(d[[1]], 1, 4))

  lvl_ok <- identical(code_len, 6L)
  yr_ok  <- identical(ym, as.character(s$year))
  ncol_ok <- identical(ncol(d), EXPECT_NCOL[[s$dir]])

  cat(sprintf("  %-28s %-9s cols=%2d  codelen=%-6s year=%-6s %s\n",
              bn, s$lvl, ncol(d),
              paste(code_len, collapse = "/"), paste(ym, collapse = "/"),
              if (lvl_ok && yr_ok && ncol_ok) "OK" else "<-- CHECK"))
  if (!ncol_ok) fail(bn, sprintf("%s has %d columns, expected %d",
                                 s$pref, ncol(d), EXPECT_NCOL[[s$dir]]))

  if (!lvl_ok) fail(bn, sprintf("%s table has code length %s, expected 6",
                                s$pref, paste(code_len, collapse = "/")))
  if (!yr_ok)  fail(bn, sprintf("YearMonth says %s but filename says %d",
                                paste(ym, collapse = "/"), s$year))
}

cat("\n", strrep("=", 72), "\n", sep = "")
cat("SECTION 4 - Findings\n")
cat(strrep("=", 72), "\n")

if (length(results) == 0) {
  cat("No issues found.\n")
} else {
  for (lv in c("FAIL", "WARN")) {
    sel <- Filter(function(r) r$lv == lv, results)
    if (!length(sel)) next
    cat(sprintf("\n%s (%d):\n", lv, length(sel)))
    msgs <- vapply(sel, function(r) r$m, character(1))
    if (lv == "FAIL") {
      for (r in sel) cat(sprintf("  %-30s %s\n", r$f, r$m))
    } else {
      for (m in unique(msgs)) {
        hit <- vapply(sel, function(r) identical(r$m, m), logical(1))
        if (sum(hit) == 1) cat(sprintf("  %-30s %s\n", sel[hit][[1]]$f, m))
        else cat(sprintf("  [%d archives] %s\n", sum(hit), m))
      }
    }
  }
}
cat(sprintf("\n%d archives checked, %d FAIL, %d WARN\n",
            length(zips), n_fail, length(results) - n_fail))
