# rp_reconcile_denominators.R
# [PROTOCOL Steps 1 and 3] Verifies the API variable names against the live
# endpoint, harmonizes the denominators, then annualizes the monthly series and
# compares it against the annual benchmark for the same (year, direction,
# NAICS6). Any sector off by more than 1% is investigated.
#
# In:   CENSUS_API_KEY, $DATA/rp_panel/rp_share_panel_naics6.csv
# Out:  $DATA/census_naics/ (cached pulls), $DATA/rp_panel/ (reconciliation)
# Run:  Rscript R/04_related_party/rp_reconcile_denominators.R
#
# This is the step that decides whether the monthly and annual sources are
# measuring the same thing at all.


if (!requireNamespace("httr2", quietly = TRUE)) {


  stop("Package httr2 is required. install.packages(\"httr2\")")
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

census_key <- require_key("CENSUS_API_KEY")

RAW_DIR   <- na_data("census_naics")   # cached monthly pulls
OUT_DIR   <- na_data("rp_panel")       # reconciliation output
IN_PANEL  <- na_data("rp_panel/rp_share_panel_naics6.csv")
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(IN_PANEL)) {
  stop("Share panel not found. Run R/04_related_party/rp_build_share_panel.R first.")
}

# [FAILURE MODE: "wrong country aggregate" - symptom: shares implausibly close
# to national figures.] CTY_CODE pins a single partner; 1220 is Canada, 2010
if (!exists("CTY_CODE")) CTY_CODE <- "1220"
if (!exists("CTY_NAME")) CTY_NAME <- "CANADA"

CTY_SLUG <- switch(CTY_NAME, CANADA = "can", MEXICO = "mex",
                   tolower(substr(CTY_NAME, 1, 3)))

YEARS     <- 2010:2025

TOLERANCE <- 0.01

MAX_TRIES        <- 3
RETRY_PAUSE_SEC  <- 10
MAX_FAILURES     <- 5    # circuit breaker - a dead host should not burn hours
REQ_TIMEOUT_SEC  <- 600  # imports years are large and the connection is slow

cat("=========================================================\n")
cat("rp_reconcile_denominators.R  [PROTOCOL Step 1 + Step 3]\n")
cat("=========================================================\n")
cat("[Step 1] verifying variable names against the live endpoints...\n")

var_list <- function(endpoint) {
  u <- sprintf("https://api.census.gov/data/timeseries/intltrade/%s/naics/variables.json",
               endpoint)
  names(jsonlite::fromJSON(u)$variables)
}
exp_vars <- var_list("exports")
imp_vars <- var_list("imports")

REQUIRED_EXP <- c("NAICS", "ALL_VAL_MO", "DF", "MONTH", "YEAR", "COMM_LVL",
                  "CTY_CODE")
REQUIRED_IMP <- c("NAICS", "CON_VAL_MO", "GEN_VAL_MO", "MONTH", "YEAR",
                  "COMM_LVL", "CTY_CODE")

miss_e <- setdiff(REQUIRED_EXP, exp_vars)
miss_i <- setdiff(REQUIRED_IMP, imp_vars)
if (length(miss_e) || length(miss_i)) {
  stop("Live variable list does not contain the fields this script needs.\n",
       if (length(miss_e)) paste0("  exports missing: ",
                                  paste(miss_e, collapse = ", "), "\n") else "",
       if (length(miss_i)) paste0("  imports missing: ",
                                  paste(miss_i, collapse = ", "), "\n") else "",
       "Census has renamed trade fields before. Re-read the variable lists ",
       "and update REQUIRED_* above before trusting any number below.")
}
cat("  exports/naics : all ", length(REQUIRED_EXP), " fields present\n", sep = "")
cat("  imports/naics : all ", length(REQUIRED_IMP), " fields present\n", sep = "")
cat("  verified ", format(Sys.Date()), " - record this date in the codebook.\n",
    sep = "")

if ("DF" %in% imp_vars) {
  cat("  NOTE: DF now exists on imports/naics - it did not before. ",
      "Check whether the import basis needs revisiting.\n", sep = "")
}

pull_cell <- function(direction, year) {
  fields <- if (direction == "exp") REQUIRED_EXP[1:4] else REQUIRED_IMP[1:4]
  ep     <- if (direction == "exp") "exports" else "imports"
  req <- request(sprintf("https://api.census.gov/data/timeseries/intltrade/%s/naics", ep)) |>
    req_url_query(get = paste(fields, collapse = ","),
                  YEAR = year, COMM_LVL = "NA6", CTY_CODE = CTY_CODE,
                  key = census_key) |>
    req_timeout(REQ_TIMEOUT_SEC)

  for (attempt in seq_len(MAX_TRIES)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      if (attempt < MAX_TRIES) { Sys.sleep(RETRY_PAUSE_SEC); next }
      return(list(status = "failed", msg = conditionMessage(resp)))
    }
    if (resp_status(resp) == 204) return(list(status = "empty"))
    if (resp_status(resp) != 200) {
      if (attempt < MAX_TRIES) { Sys.sleep(RETRY_PAUSE_SEC); next }
      return(list(status = "failed", msg = paste("HTTP", resp_status(resp))))
    }
    m <- resp_body_json(resp, simplifyVector = TRUE)
    d <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
    names(d) <- m[1, ]
    return(list(status = "ok", data = d))
  }
}

consecutive_failures <- 0
for (direction in c("exp", "imp")) {
  for (yr in YEARS) {
    f <- file.path(RAW_DIR, sprintf("naics6_%s_%s_%d.csv", CTY_SLUG, direction, yr))
    if (file.exists(f)) next
    cat(sprintf("  pulling %s %d ... ", direction, yr))
    res <- pull_cell(direction, yr)
    if (res$status == "ok") {
      tmp <- paste0(f, ".tmp")
      write.csv(res$data, tmp, row.names = FALSE)
      if (file.exists(f)) file.remove(f)
      file.rename(tmp, f)
      cat(sprintf("%d rows\n", nrow(res$data)))
      consecutive_failures <- 0
    } else if (res$status == "empty") {
      cat("HTTP 204, no data for this year\n")
      consecutive_failures <- 0
    } else {
      cat("FAILED:", res$msg, "\n")
      consecutive_failures <- consecutive_failures + 1
      if (consecutive_failures >= MAX_FAILURES) {
        stop("Circuit breaker: ", MAX_FAILURES, " consecutive failures. ",
             "Completed cells are on disk and will be skipped on re-run.")
      }
    }
  }
}

read_dir <- function(direction) {
  fs <- list.files(RAW_DIR,
                   pattern = sprintf("^naics6_%s_%s_[0-9]{4}\\.csv$", CTY_SLUG, direction),
                   full.names = TRUE)
  if (!length(fs)) return(NULL)
  do.call(rbind, lapply(fs, function(f)
    read.csv(f, stringsAsFactors = FALSE, colClasses = c(NAICS = "character"))))
}

ex <- read_dir("exp")
im <- read_dir("imp")
if (is.null(ex) || is.null(im)) {
  stop("No cached monthly cells found in ", RAW_DIR,
       " - the pull in SECTION 3 produced nothing.")
}
ex$ALL_VAL_MO <- as.numeric(ex$ALL_VAL_MO)
im$CON_VAL_MO <- as.numeric(im$CON_VAL_MO)
im$GEN_VAL_MO <- as.numeric(im$GEN_VAL_MO)

dom <- aggregate(ALL_VAL_MO ~ NAICS + YEAR + MONTH, data = ex[ex$DF == "1", ], FUN = sum)
for_ <- aggregate(ALL_VAL_MO ~ NAICS + YEAR + MONTH, data = ex[ex$DF == "2", ], FUN = sum)
agg <- aggregate(ALL_VAL_MO ~ NAICS + YEAR + MONTH, data = ex[ex$DF == "-", ], FUN = sum)
names(dom)[4] <- "domestic"; names(for_)[4] <- "foreign"; names(agg)[4] <- "aggregate"
chk <- merge(merge(agg, dom, all.x = TRUE), for_, all.x = TRUE)
chk$domestic[is.na(chk$domestic)] <- 0
chk$foreign[is.na(chk$foreign)]   <- 0
bad <- sum(abs(chk$aggregate - (chk$domestic + chk$foreign)) > 1)
cat(sprintf("\n[Step 3] DF identity aggregate == domestic + foreign: %d violation(s) of %d rows\n",
            bad, nrow(chk)))
if (bad > 0) {
  stop("The DF aggregate does not equal domestic + foreign on ", bad,
       " rows. Do not reconcile until this is understood - the export basis ",
       "cannot be trusted.")
}

exp_dom <- aggregate(ALL_VAL_MO ~ NAICS + YEAR, data = ex[ex$DF == "1", ], FUN = sum)
names(exp_dom)[3] <- "monthly_sum"
exp_tot <- aggregate(ALL_VAL_MO ~ NAICS + YEAR, data = ex[ex$DF == "-", ], FUN = sum)
names(exp_tot)[3] <- "monthly_sum"

imp_con <- aggregate(CON_VAL_MO ~ NAICS + YEAR, data = im, FUN = sum)
names(imp_con)[3] <- "monthly_sum"
imp_gen <- aggregate(GEN_VAL_MO ~ NAICS + YEAR, data = im, FUN = sum)
names(imp_gen)[3] <- "monthly_sum"

# [PROTOCOL Step 3 verification] =============================================
# =================================
panel <- read.csv(IN_PANEL, stringsAsFactors = FALSE,
                  colClasses = c(naics6 = "character"))
bench <- panel[panel$country == CTY_NAME, c("direction", "naics6", "year", "total")]

reconcile <- function(monthly, direction, label) {
  b <- bench[bench$direction == direction & bench$total > 0, ]
  m <- merge(monthly, b, by.x = c("NAICS", "YEAR"), by.y = c("naics6", "year"))
  if (!nrow(m)) return(NULL)
  m$gap_pct <- (m$monthly_sum - m$total) / m$total
  m$within  <- abs(m$gap_pct) <= TOLERANCE
  m$basis   <- label
  m$direction <- direction
  m
}

rec_list <- list(
  reconcile(exp_dom, "exp", "domestic exports (benchmark basis)"),
  reconcile(exp_tot, "exp", "total exports (protocol text)"),
  reconcile(imp_con, "imp", "consumption imports (correct)"),
  reconcile(imp_gen, "imp", "general imports (wrong basis)"))
rec <- do.call(rbind, rec_list)

cat("\n---------------------------------------------------------\n")
cat("[Step 3] ANNUALIZED MONTHLY vs ANNUAL BENCHMARK, by basis\n")
cat("tolerance = 1% per sector, the protocol's own figure\n")
cat("---------------------------------------------------------\n")
for (lab in unique(rec$basis)) {
  s <- rec[rec$basis == lab, ]
  cat(sprintf("%-36s n=%5d  agg gap %+8.4f%%   within 1%%: %5.1f%%   median |gap| %6.3f%%\n",
              lab, nrow(s),
              100 * (sum(s$monthly_sum) - sum(s$total)) / sum(s$total),
              100 * mean(s$within),
              100 * median(abs(s$gap_pct))))
}

cat("\nper-year, correct bases only (share of sectors within 1%):\n")
cat(sprintf("%-6s %22s %22s\n", "year", "exports (domestic)", "imports (consumption)"))
for (y in sort(unique(rec$YEAR))) {
  e <- rec[rec$basis == "domestic exports (benchmark basis)" & rec$YEAR == y, ]
  i <- rec[rec$basis == "consumption imports (correct)" & rec$YEAR == y, ]
  cat(sprintf("%-6d %21.1f%% %21.1f%%\n", y,
              if (nrow(e)) 100 * mean(e$within) else NA_real_,
              if (nrow(i)) 100 * mean(i$within) else NA_real_))
}

# [FAILURE MODE: "denominator mismatch"] Name the offenders rather than
# reporting a pass rate.
fails <- rec[rec$basis %in% c("domestic exports (benchmark basis)",
                              "consumption imports (correct)") & !rec$within, ]
cat(sprintf("\nsectors outside 1%% on a correct basis: %d\n", nrow(fails)))
if (nrow(fails)) {
  f <- fails[order(-abs(fails$gap_pct)), ]
  for (i in seq_len(min(15, nrow(f)))) {
    cat(sprintf("  %s %s %d  monthly %.4g vs bench %.4g  gap %+.2f%%\n",
                f$direction[i], f$NAICS[i], f$YEAR[i],
                f$monthly_sum[i], f$total[i], 100 * f$gap_pct[i]))
  }
}

out <- file.path(OUT_DIR, sprintf("rp_reconciliation_annual_vs_monthly%s.csv",
                                  if (CTY_SLUG == "can") "" else paste0("_", CTY_SLUG)))
write.csv(rec[, c("basis", "direction", "NAICS", "YEAR", "monthly_sum",
                  "total", "gap_pct", "within")], out, row.names = FALSE)
cat(sprintf("\nsaved %s  (%d rows)\n", out, nrow(rec)))

cat("\n--- tests ---\n")
fails_n <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails_n <<- fails_n + 1 }
}

dom_rec <- rec[rec$basis == "domestic exports (benchmark basis)", ]
con_rec <- rec[rec$basis == "consumption imports (correct)", ]
tot_rec <- rec[rec$basis == "total exports (protocol text)", ]

check("exports reconcile on the DOMESTIC basis (>=99% of sectors within 1%)",
      mean(dom_rec$within) >= 0.99,
      sprintf("got %.1f%%", 100 * mean(dom_rec$within)))

check("imports reconcile on the CONSUMPTION basis (>=99% within 1%)",
      mean(con_rec$within) >= 0.99,
      sprintf("got %.1f%%", 100 * mean(con_rec$within)))

check("total-exports basis FAILS reconciliation (protocol Step 3 is in error)",
      mean(tot_rec$within) < 0.5,
      sprintf("got %.1f%% within 1%% - investigate, this should fail",
              100 * mean(tot_rec$within)))

if (CTY_SLUG == "can") {
  d15 <- dom_rec[dom_rec$YEAR == 2015, ]
  check("2015 domestic exports: 398 sectors, all exact",
        nrow(d15) == 398 && all(abs(d15$gap_pct) < 1e-9),
        sprintf("got n=%d, max |gap| %.3g", nrow(d15), max(abs(d15$gap_pct))))

  c15 <- con_rec[con_rec$YEAR == 2015, ]
  check("2015 consumption imports: 400 sectors, all exact",
        nrow(c15) == 400 && all(abs(c15$gap_pct) < 1e-9),
        sprintf("got n=%d, max |gap| %.3g", nrow(c15), max(abs(c15$gap_pct))))
} else {
  cat(sprintf("  ....  Canada-specific 2015 pins skipped (country = %s)\n",
              CTY_NAME))
}

cat(sprintf("\n%d test(s) failed.\n", fails_n))
if (fails_n > 0) {
  stop("Step 3 reconciliation did not pass. Per the protocol's own sequencing, ",
       "Phase 2 gates on this - do not build shares on these denominators ",
       "until it is understood.")
}

cat("\n=========================================================\n")
cat("[Step 3] PASSED on the correct bases.\n")
cat("Record in the codebook: monthly export denominator is ALL_VAL_MO with\n")
cat("DF == \"1\" (domestic), NOT total exports as the protocol text says;\n")
cat("monthly import denominator is CON_VAL_MO (consumption, customs value).\n")
cat("=========================================================\n")
