# rp_step5_export_gate.R
# [PROTOCOL Step 5] The Canada export-side data quality diagnostic.
#
# In:   $DATA/rp/
# Out:  $DATA/rp_panel/
# Run:  Rscript R/04_related_party/rp_step5_export_gate.R
#
# The protocol calls this a GATE, not a check: if it fails, the export
# direction is unusable and the project becomes import-only. It therefore runs
# standalone, without rebuilding the share panel.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

IN_DIR  <- na_data("rp")
OUT_DIR <- na_data("rp_panel")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

TEST_COUNTRY  <- "CANADA"
PEER_COUNTRIES <- c("MEXICO", "CHINA", "JAPAN", "GERMANY", "UNITED KINGDOM")

ELEVATED_MULTIPLE <- 2.0
PERSISTENT_FRAC   <- 0.75

COLS <- c("naics_raw", "country", "year",
          "exp_total", "exp_related", "exp_nonrelated", "exp_notreported",
          "imp_total", "imp_related", "imp_nonrelated", "imp_notreported")

files <- list.files(IN_DIR, pattern = "^rp_naics6_.*\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No rp_naics6_*.csv files found in ", IN_DIR)

keep <- c(TEST_COUNTRY, PEER_COUNTRIES)

raw_list <- list()
for (f in files) {
  d <- read.csv(f, skip = 4, header = TRUE, stringsAsFactors = FALSE,
                colClasses = "character")
  if (ncol(d) != length(COLS)) {
    stop("Unexpected column count in ", basename(f), ": got ", ncol(d))
  }
  names(d) <- COLS
  raw_list[[basename(f)]] <- d[d$country %in% keep, ]
}
rp <- do.call(rbind, raw_list)
rownames(rp) <- NULL

rp <- rp[grepl("^[0-9]{4}$", rp$year), ]
rp$year <- as.integer(rp$year)

num <- function(x) suppressWarnings(as.numeric(x))
for (v in c("exp_total", "exp_notreported", "imp_total", "imp_notreported")) {
  rp[[v]] <- num(rp[[v]])
}

other_share_by <- function(total_col, notrep_col) {
  agg_tot <- aggregate(rp[[total_col]],  by = list(country = rp$country,
                                                   year = rp$year), FUN = sum,
                       na.rm = TRUE)
  agg_oth <- aggregate(rp[[notrep_col]], by = list(country = rp$country,
                                                   year = rp$year), FUN = sum,
                       na.rm = TRUE)
  names(agg_tot)[3] <- "total"
  names(agg_oth)[3] <- "notreported"
  m <- merge(agg_tot, agg_oth)
  m$other_share <- ifelse(m$total > 0, m$notreported / m$total, NA_real_)
  m
}

exp_share <- other_share_by("exp_total", "exp_notreported")
imp_share <- other_share_by("imp_total", "imp_notreported")
exp_share$direction <- "exp"
imp_share$direction <- "imp"

gate_panel <- rbind(exp_share, imp_share)
gate_panel <- gate_panel[order(gate_panel$direction, gate_panel$year,
                               gate_panel$country), ]

out_csv <- file.path(OUT_DIR, "rp_step5_other_share_by_country_year.csv")
write.csv(gate_panel, out_csv, row.names = FALSE)

print_matrix <- function(df, label) {
  cat("\n", label, "\n", sep = "")
  yrs <- sort(unique(df$year))
  cat(sprintf("%-16s", "country"))
  for (y in yrs) cat(sprintf("%6s", substr(as.character(y), 3, 4)))
  cat("\n")
  for (ctry in c(TEST_COUNTRY, PEER_COUNTRIES)) {
    cat(sprintf("%-16s", ctry))
    for (y in yrs) {
      v <- df$other_share[df$country == ctry & df$year == y]
      if (length(v) == 0 || is.na(v[1])) cat(sprintf("%6s", "-"))
      else cat(sprintf("%6.1f", 100 * v[1]))
    }
    cat("\n")
  }
}

cat("=========================================================\n")
cat("PROTOCOL Step 5 - Canada export-side data quality gate\n")
cat("Share of trade with NO related-party flag (%), by year\n")
cat("=========================================================\n")
print_matrix(exp_share, "EXPORTS (US -> partner)  <- this is the gate")
print_matrix(imp_share, "IMPORTS (partner -> US)  <- control, not EEI-derived")

yrs <- sort(unique(exp_share$year))
ratios <- rep(NA_real_, length(yrs))
names(ratios) <- yrs

for (i in seq_along(yrs)) {
  y <- yrs[i]
  can <- exp_share$other_share[exp_share$country == TEST_COUNTRY &
                                 exp_share$year == y]
  peer <- exp_share$other_share[exp_share$country %in% PEER_COUNTRIES &
                                  exp_share$year == y]
  peer <- peer[!is.na(peer)]
  if (length(can) && !is.na(can[1]) && length(peer) && median(peer) > 0) {
    ratios[i] <- can[1] / median(peer)
  }
}

cat("\n---------------------------------------------------------\n")
cat("Canada's unflagged export share as a multiple of the peer median\n")
cat("---------------------------------------------------------\n")
for (i in seq_along(yrs)) {
  flag <- if (!is.na(ratios[i]) && ratios[i] >= ELEVATED_MULTIPLE) "  ELEVATED" else ""
  cat(sprintf("  %d : %5.2fx%s\n", yrs[i], ratios[i], flag))
}

valid     <- ratios[!is.na(ratios)]
n_elev    <- sum(valid >= ELEVATED_MULTIPLE)
frac_elev <- if (length(valid)) n_elev / length(valid) else NA_real_

cat(sprintf("\nElevated (>= %.1fx peer median) in %d of %d years (%.0f%%)\n",
            ELEVATED_MULTIPLE, n_elev, length(valid), 100 * frac_elev))
cat(sprintf("Median multiple across years: %.2fx\n", median(valid)))

gate_fails <- !is.na(frac_elev) && frac_elev >= PERSISTENT_FRAC

cat("\n=========================================================\n")
if (gate_fails) {
  cat("VERDICT: GATE FAILS\n")
  cat("Canada's export-side related-party flag is substantially and\n")
  cat("persistently under-populated relative to peers. Under the protocol's\n")
  cat("own rule the export direction should be treated as unusable and the\n")
  cat("project restricted to IMPORTS, with the reason documented.\n")
  cat("\nNOTE: the protocol also says to confirm this directly with Census's\n")
  cat("Economic Indicators Division (eid.international.trade.data@census.gov,\n")
  cat("301-763-2311) before acting on it. This diagnostic is evidence, not a\n")
  cat("final determination - a data-collection question is better answered by\n")
  cat("the people who run the collection.\n")
} else {
  cat("VERDICT: GATE PASSES\n")
  cat("Canada's unflagged export share is broadly comparable to peers.\n")
  cat("The export direction can proceed.\n")
}
cat("=========================================================\n")
cat(sprintf("\nsaved %s\n", out_csv))

cat("\n--- tests ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) {
    cat(sprintf("  PASS  %s\n", label))
  } else {
    cat(sprintf("  FAIL  %s %s\n", label, detail))
    fails <<- fails + 1
  }
}

check("all 6 countries present",
      all(c(TEST_COUNTRY, PEER_COUNTRIES) %in% exp_share$country),
      paste("missing:", paste(setdiff(c(TEST_COUNTRY, PEER_COUNTRIES),
                                      exp_share$country), collapse = ",")))

check("full 2005-2025 span covered",
      identical(range(exp_share$year), c(2005L, 2025L)),
      paste("got", paste(range(exp_share$year), collapse = "-")))

check("shares all within [0,1]",
      all(gate_panel$other_share >= 0 & gate_panel$other_share <= 1,
          na.rm = TRUE))

check("no duplicate country-year-direction rows",
      !anyDuplicated(paste(gate_panel$country, gate_panel$year,
                           gate_panel$direction)))

pooled <- function(ctry) {
  s <- rp[rp$country == ctry & rp$year >= 2011 & rp$year <= 2015, ]
  100 * sum(s$exp_notreported, na.rm = TRUE) / sum(s$exp_total, na.rm = TRUE)
}
expected <- c(CANADA = 7.96, MEXICO = 3.64, CHINA = 0.66,
              JAPAN = 1.15, GERMANY = 2.53, `UNITED KINGDOM` = 3.02)
for (ctry in names(expected)) {
  got <- pooled(ctry)
  check(sprintf("regression %-15s expect %.2f%%", ctry, expected[[ctry]]),
        abs(got - expected[[ctry]]) < 0.01,
        sprintf("got %.2f%%", got))
}

cat(sprintf("\n%d test(s) failed.\n", fails))
if (fails > 0) {
  stop("Tests failed - do not rely on the verdict above until resolved.")
}
