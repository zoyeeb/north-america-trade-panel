# rp_build_share_panel.R
# Builds the related-party share panel from the four Census benchmark files
# (NAICS6, 2005-2025). Offline - no network.
#
# In:   $DATA/rp/   the four downloaded Census benchmark files
# Out:  $DATA/rp_panel/rp_share_panel_naics6.csv, rp_vintage_audit.csv, ...
# Run:  Rscript R/04_related_party/rp_build_share_panel.R
#
# Implements protocol steps 2, 4, 6 and 7: extract the annual benchmark, build
# all three "Other" treatments, audit NAICS vintage breaks, and build the panel
# with its stability statistics.
#
# DELIBERATELY DOES NOT implement step 8, applying a threshold and freezing the
# sector list. That depends on an open design question, and choosing after
# seeing results is a researcher degree of freedom. Building the panel is safe
# under all the options; freezing a list is not.
#
# Comment markers below: [PROTOCOL Step N] a step being carried out,
# [FAILURE MODE ...] a guard against a known failure, [DEVIATION] a place the
# protocol cannot be followed as written, with the reason.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

IN_DIR  <- na_data("rp")          # the four downloaded Census benchmark files
OUT_DIR <- na_data("rp_panel")    # everything this script produces

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# [FAILURE MODE: "wrong country aggregate" - symptom: shares implausibly close
# to national figures.] The protocol's prevention is to verify the country
KEEP_COUNTRIES <- c("CANADA", "MEXICO")

COLS <- c("naics_raw", "country", "year",
          "exp_total", "exp_related", "exp_nonrelated", "exp_notreported",
          "imp_total", "imp_related", "imp_nonrelated", "imp_notreported")

files <- list.files(IN_DIR, pattern = "^rp_naics6_.*\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No rp_naics6_*.csv files found in ", IN_DIR)

cat("Reading", length(files), "benchmark files...\n")

raw_list <- list()
for (f in files) {

  d <- read.csv(f, skip = 4, header = TRUE, stringsAsFactors = FALSE,
                colClasses = "character")

  if (ncol(d) != length(COLS)) {
    stop("Unexpected column count in ", basename(f), ": got ", ncol(d),
         ", expected ", length(COLS))
  }

  names(d) <- COLS
  d <- d[d$country %in% KEEP_COUNTRIES, ]   # drop the other ~242 partners
  raw_list[[basename(f)]] <- d

  cat(sprintf("  %-34s %7d rows kept (Canada/Mexico)\n", basename(f), nrow(d)))
}

rp <- do.call(rbind, raw_list)
rownames(rp) <- NULL

rp <- rp[grepl("^[0-9]{4}$", rp$year), ]
rp$year <- as.integer(rp$year)

rp$naics6     <- sub("^([A-Za-z0-9]+)\\s.*$", "\\1", rp$naics_raw)
rp$naics_desc <- sub("^[A-Za-z0-9]+\\s+", "", rp$naics_raw)
rp$naics_raw  <- NULL   # drop the combined field now that it is split

num <- function(x) suppressWarnings(as.numeric(x))

exp_part <- data.frame(
  country     = rp$country,
  direction   = "exp",                    # US -> partner
  naics6      = rp$naics6,
  naics_desc  = rp$naics_desc,
  year        = rp$year,
  total       = num(rp$exp_total),
  related     = num(rp$exp_related),
  nonrelated  = num(rp$exp_nonrelated),
  notreported = num(rp$exp_notreported),  # "Other" - flag missing, see SECTION 4
  stringsAsFactors = FALSE
)

imp_part <- data.frame(
  country     = rp$country,
  direction   = "imp",                    # partner -> US
  naics6      = rp$naics6,
  naics_desc  = rp$naics_desc,
  year        = rp$year,
  total       = num(rp$imp_total),
  related     = num(rp$imp_related),
  nonrelated  = num(rp$imp_nonrelated),
  notreported = num(rp$imp_notreported),
  stringsAsFactors = FALSE
)

panel <- rbind(exp_part, imp_part)

dup_key <- paste(panel$country, panel$direction, panel$naics6, panel$year,
                 sep = "|")
if (anyDuplicated(dup_key)) {
  dups <- unique(dup_key[duplicated(dup_key)])
  stop("Duplicate observations found - ", length(dups),
       " (country, direction, naics6, year) combinations appear more than ",
       "once. This usually means the input folder holds two copies of the ",
       "same benchmark file. Check ", IN_DIR, " for duplicates. Examples: ",
       paste(utils::head(dups, 3), collapse = "; "))
}

denom_all <- panel$related + panel$nonrelated + panel$notreported  # R+N+O
denom_rep <- panel$related + panel$nonrelated                      # R+N

# [FAILURE MODE: "suppressed cells read as zero" - symptom: shares collapse to
# 0 in scattered years.] Where there is no trade at all the share is
panel$s_lower <- ifelse(denom_all > 0, panel$related / denom_all, NA_real_)
panel$s_excl  <- ifelse(denom_rep > 0, panel$related / denom_rep, NA_real_)
panel$s_upper <- ifelse(denom_all > 0,
                        (panel$related + panel$notreported) / denom_all,
                        NA_real_)

panel$other_share <- ifelse(denom_all > 0, panel$notreported / denom_all,
                            NA_real_)

# [DEVIATION from PROTOCOL Step 2.] The protocol requires suppressed cells to
# be recorded explicitly as missing and never as zero, and asks for a
panel <- panel[order(panel$country, panel$direction, panel$naics6,
                     panel$year), ]

out_panel <- file.path(OUT_DIR, "rp_share_panel_naics6.csv")
write.csv(panel, out_panel, row.names = FALSE)
cat(sprintf("\nsaved %s  (%d rows)\n", out_panel, nrow(panel)))

# [FAILURE MODE: "NAICS vintage break" - symptom: discontinuity at 2012/2017/
# 2022.] NAICS is revised on a five-year cycle; codes split, merge and retire.
all_years <- sort(unique(panel$year))
span <- range(all_years)

cov <- aggregate(year ~ country + direction + naics6,
                 data = panel[!is.na(panel$total) & panel$total > 0, ],
                 FUN = function(y) length(unique(y)))
names(cov)[4] <- "n_years"

first_last <- aggregate(year ~ country + direction + naics6,
                        data = panel[!is.na(panel$total) & panel$total > 0, ],
                        FUN = function(y) paste0(min(y), "-", max(y)))
names(first_last)[4] <- "year_range"

vintage <- merge(cov, first_last)
vintage$full_coverage <- vintage$n_years == length(all_years)

out_vint <- file.path(OUT_DIR, "rp_vintage_audit.csv")
write.csv(vintage[order(vintage$country, vintage$direction, vintage$naics6), ],
          out_vint, row.names = FALSE)

cat(sprintf("saved %s  (%d code-country-direction combinations)\n",
            out_vint, nrow(vintage)))
cat(sprintf("  full %d-%d coverage: %d;  partial: %d\n",
            span[1], span[2], sum(vintage$full_coverage),
            sum(!vintage$full_coverage)))

stab_one <- function(d) {
  s <- d$s_lower[!is.na(d$s_lower)]
  if (length(s) == 0) return(NULL)
  yr <- d$year[!is.na(d$s_lower)]

  get_yr <- function(y) if (y %in% yr) s[match(y, yr)] else NA_real_

  data.frame(
    n_years    = length(s),               # usable years - Step 8 reliability weight
    mean_s     = mean(s),                 # central tendency
    min_s      = min(s),                  # the binding constraint for Step 8
    max_s      = max(s),
    sd_s       = if (length(s) > 1) sd(s) else NA_real_,  # sd() is NA for n=1
    range_s    = max(s) - min(s),         # worst-case excursion
    d_2009     = get_yr(2009) - get_yr(2008),   # financial crisis
    d_2020     = get_yr(2020) - get_yr(2019),   # pandemic
    mean_other = mean(d$other_share, na.rm = TRUE),  # exposure to the Step 4 choice
    stringsAsFactors = FALSE
  )
}

key <- paste(panel$country, panel$direction, panel$naics6, sep = "|")
stab_list <- list()
for (k in unique(key)) {
  d <- panel[key == k, ]
  s <- stab_one(d)
  if (is.null(s)) next
  parts <- strsplit(k, "|", fixed = TRUE)[[1]]   # fixed=TRUE: "|" is regex alternation
  s$country    <- parts[1]
  s$direction  <- parts[2]
  s$naics6     <- parts[3]
  s$naics_desc <- d$naics_desc[1]
  stab_list[[k]] <- s
}

stab <- do.call(rbind, stab_list)
stab <- stab[, c("country", "direction", "naics6", "naics_desc",
                 "n_years", "mean_s", "min_s", "max_s", "sd_s", "range_s",
                 "d_2009", "d_2020", "mean_other")]
stab <- stab[order(stab$country, stab$direction, -stab$mean_s), ]

out_stab <- file.path(OUT_DIR, "rp_stability_stats.csv")
write.csv(stab, out_stab, row.names = FALSE)
cat(sprintf("saved %s  (%d rows)\n", out_stab, nrow(stab)))

# [PROTOCOL Step 4] Answers, from the panel rather than from memory, the
# question SECTION 4 raises: does the choice among s_lower / s_excl / s_upper
MATERIALITY_BANDS <- c(0, 1e7, 1e8, 1e9)   # USD, mean annual trade
band_label <- function(x) if (x == 0) "no floor" else
  paste0(">$", format(x / 1e6, big.mark = ",", trim = TRUE), "M")

sector_means <- function(ctry, dir) {
  d <- panel[panel$country == ctry & panel$direction == dir, ]
  a <- aggregate(cbind(total, other_share, s_lower, s_excl, s_upper) ~ naics6,
                 data = d, FUN = mean, na.rm = TRUE, na.action = na.pass)
  a$naics_desc <- d$naics_desc[match(a$naics6, d$naics6)]
  a$country <- ctry
  a$direction <- dir
  a[!is.na(a$total), ]          # UNFILTERED - bands are applied at report time
}

unflagged_share <- function(ctry, dir, yr = NULL) {
  d <- panel[panel$country == ctry & panel$direction == dir, ]
  if (!is.null(yr)) d <- d[d$year == yr, ]
  sum(d$notreported) / sum(d$related + d$nonrelated + d$notreported)
}

mat <- do.call(rbind, lapply(c("CANADA", "MEXICO"), function(ctry)
  do.call(rbind, lapply(c("imp", "exp"), function(dir) sector_means(ctry, dir)))))
mat <- mat[, c("country", "direction", "naics6", "naics_desc", "total",
               "other_share", "s_lower", "s_excl", "s_upper")]
names(mat)[names(mat) == "total"] <- "mean_total"
mat <- mat[order(mat$country, mat$direction, -mat$other_share), ]

cat("\n---------------------------------------------------------------------\n")
cat('[Step 4] HOW MUCH THE "OTHER" TREATMENT MATTERS\n')
cat("  NO size floor is set - every band reported side by side, reader picks\n")
cat("---------------------------------------------------------------------\n")
cat(sprintf("%-8s %-4s %10s %8s %10s %11s %13s\n",
            "country", "dir", "size band", "sectors", "other>5%", "other>20%",
            "unflagged $%"))
for (ctry in c("CANADA", "MEXICO")) {
  for (dir in c("imp", "exp")) {
    sl <- mat[mat$country == ctry & mat$direction == dir, ]
    uw <- 100 * unflagged_share(ctry, dir)
    for (fl in MATERIALITY_BANDS) {
      b <- sl[sl$mean_total > fl, ]
      cat(sprintf("%-8s %-4s %10s %8d %10d %11d %12.1f%%\n", ctry, dir,
                  band_label(fl), nrow(b), sum(b$other_share > 0.05),
                  sum(b$other_share > 0.20), uw))
    }
    cat("\n")
  }
}
cat("  'unflagged $%' is DOLLAR-WEIGHTED across the WHOLE slice and does not\n")
cat("  depend on the band - it is repeated so each block reads on its own.\n")
cat("  Where a count changes across bands, the band is part of the claim.\n")

cat("\n---------------------------------------------------------------------\n")
cat("  SECTORS WHERE THE THREE MEASURES DIVERGE (mean Other > 5%, NO floor)\n")
cat("---------------------------------------------------------------------\n")
for (ctry in c("CANADA", "MEXICO")) {
  for (dir in c("imp", "exp")) {
    s <- mat[mat$country == ctry & mat$direction == dir &
               mat$other_share > 0.05, ]
    cat(sprintf("\n%s %s - %d sector(s) above 5%%\n", ctry, dir, nrow(s)))
    if (nrow(s) == 0) { cat("    (none)\n"); next }
    shown <- s[order(-s$mean_total), ][seq_len(min(8, nrow(s))), ]
    cat(sprintf("  %-7s %-42s %8s %9s %7s %7s %7s\n",
                "naics6", "description", "$bn/yr", "other", "lower",
                "excl", "upper"))
    for (i in seq_len(nrow(shown))) {
      r <- shown[i, ]
      cat(sprintf("  %-7s %-42s %8.2f %8.1f%% %7.3f %7.3f %7.3f\n",
                  r$naics6, substr(r$naics_desc, 1, 42), r$mean_total / 1e9,
                  100 * r$other_share, r$s_lower, r$s_excl, r$s_upper))
    }
    if (nrow(s) > 8) {
      cat(sprintf("    ... and %d more, largest-first; all of them are in %s\n",
                  nrow(s) - 8, "rp_other_materiality.csv"))
    }
  }
}

cat("\n---------------------------------------------------------------------\n")
cat("  UNFLAGGED SHARE OF DOLLARS BY YEAR (dollar-weighted)\n")
cat("---------------------------------------------------------------------\n")
cat(sprintf("  %-6s %10s %10s %10s %10s\n", "year", "CAN imp", "CAN exp",
            "MEX imp", "MEX exp"))
for (y in range(panel$year)[1]:range(panel$year)[2]) {
  cat(sprintf("  %-6d %9.1f%% %9.1f%% %9.1f%% %9.1f%%\n", y,
              100 * unflagged_share("CANADA", "imp", y),
              100 * unflagged_share("CANADA", "exp", y),
              100 * unflagged_share("MEXICO", "imp", y),
              100 * unflagged_share("MEXICO", "exp", y)))
}

out_mat <- file.path(OUT_DIR, "rp_other_materiality.csv")
write.csv(mat, out_mat, row.names = FALSE)
cat(sprintf("\nsaved %s  (%d rows)\n", out_mat, nrow(mat)))

cat("\n--- tests ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails <<- fails + 1 }
}

check("panel has 35,228 rows", nrow(panel) == 35228,
      paste("got", nrow(panel)))
check("21 years, 2005-2025, contiguous",
      identical(sort(unique(panel$year)), 2005:2025))
check("two countries, two directions, 582 codes",
      setequal(panel$country, c("CANADA", "MEXICO")) &&
        setequal(panel$direction, c("exp", "imp")) &&
        length(unique(panel$naics6)) == 582)
check("total == related + nonrelated + notreported on every row",
      max(abs(panel$total - (panel$related + panel$nonrelated +
                               panel$notreported))) == 0)
check("all four shares within [0,1]",
      all(vapply(c("s_lower", "s_excl", "s_upper", "other_share"),
                 function(m) all(panel[[m]] >= 0 & panel[[m]] <= 1,
                                 na.rm = TRUE), logical(1))))
check("s_lower <= s_excl <= s_upper on every row",
      all(panel$s_lower <= panel$s_excl + 1e-12, na.rm = TRUE) &&
        all(panel$s_excl <= panel$s_upper + 1e-12, na.rm = TRUE))
check("s_upper - s_lower == other_share",
      max(abs(panel$s_upper - (panel$s_lower + panel$other_share)),
          na.rm = TRUE) < 1e-12)
# [FAILURE MODE: "suppressed cells read as zero"] - no-trade rows must be NA,
# never 0, and every row with trade must have a share.
check("826 zero-trade rows, and they are the ONLY NA shares",
      sum(panel$total == 0) == 826 &&
        sum(is.na(panel$s_lower)) == 826 &&
        !any(panel$total > 0 & is.na(panel$s_lower)))
check("11 rows are 100% unflagged, so s_excl alone is NA there",
      sum(is.na(panel$s_excl) & !is.na(panel$s_lower)) == 11)

big_sectors <- function(ctry, dir, fl = 0) {
  s <- mat[mat$country == ctry & mat$direction == dir & mat$mean_total > fl, ]
  names(s)[names(s) == "mean_total"] <- "total"
  s
}

band_expect <- list(
  list("CANADA","imp", c(581,5,3), c(503,3,3), c(312,3,3), c( 68,3,3)),
  list("CANADA","exp", c(573,382,159), c(530,352,145), c(361,233,76), c(67,38,8)),
  list("MEXICO","imp", c(579,2,2), c(455,1,1), c(277,1,1), c( 76,1,1)),
  list("MEXICO","exp", c(574,6,2), c(477,1,1), c(283,1,1), c( 55,1,1)))
for (e in band_expect) {
  ctry <- e[[1]]; dir <- e[[2]]
  for (j in seq_along(MATERIALITY_BANDS)) {
    fl <- MATERIALITY_BANDS[j]; want <- e[[2 + j]]
    b <- big_sectors(ctry, dir, fl)
    got <- c(nrow(b), sum(b$other_share > 0.05), sum(b$other_share > 0.20))
    check(sprintf("%s %s %-8s: %d sectors, %d above 5%%, %d above 20%%",
                  ctry, dir, band_label(fl), want[1], want[2], want[3]),
          all(got == want),
          sprintf("got %d / %d / %d", got[1], got[2], got[3]))
  }
}

ci <- big_sectors("CANADA", "imp", 1e8)
check("CANADA imp above $100M: the 3 divergent sectors are 990000/211130/211111",
      setequal(ci$naics6[ci$other_share > 0.05],
               c("990000", "211130", "211111")),
      paste("got", paste(ci$naics6[ci$other_share > 0.05], collapse = ", ")))
ci0 <- big_sectors("CANADA", "imp", 0)
check("with NO floor it is those 3 plus only 212234 and 212291, both under $10M",
      setequal(ci0$naics6[ci0$other_share > 0.05],
               c("990000", "211130", "211111", "212234", "212291")) &&
        all(ci0$total[ci0$naics6 %in% c("212234", "212291")] < 1e7),
      paste("got", paste(ci0$naics6[ci0$other_share > 0.05], collapse = ", ")))
for (spec in list(list("990000", 0.967, 5.85), list("211130", 0.746, 10.52),
                  list("211111", 0.246, 60.46))) {
  r <- ci[ci$naics6 == spec[[1]], ]
  check(sprintf("CANADA imp %s: mean Other %.1f%%, $%.2fbn/yr",
                spec[[1]], spec[[2]] * 100, spec[[3]]),
        abs(r$other_share - spec[[2]]) < 0.0005 &&
          abs(r$total / 1e9 - spec[[3]]) < 0.005,
        sprintf("got %.4f and $%.3fbn", r$other_share, r$total / 1e9))
}

ng <- panel[panel$country == "CANADA" & panel$direction == "imp" &
              panel$naics6 == "211130" & panel$year == 2024, ]
check("211130 in 2024 spans 0.208 / 0.596 / 0.859 across the three measures",
      abs(ng$s_lower - 0.208) < 0.001 && abs(ng$s_excl - 0.596) < 0.001 &&
        abs(ng$s_upper - 0.859) < 0.001,
      sprintf("got %.3f / %.3f / %.3f", ng$s_lower, ng$s_excl, ng$s_upper))

check("MEXICO above $100M: exactly one divergent sector per direction",
      sum(big_sectors("MEXICO", "imp", 1e8)$other_share > 0.05) == 1 &&
        sum(big_sectors("MEXICO", "exp", 1e8)$other_share > 0.05) == 1)
check("MEXICO with NO floor: 2 imp / 6 exp, all the extras under $10M",
      sum(big_sectors("MEXICO", "imp", 0)$other_share > 0.05) == 2 &&
        sum(big_sectors("MEXICO", "exp", 0)$other_share > 0.05) == 6)

unflag <- unflagged_share   # the function SECTION 7 printed Table 3 from
check("CANADA exports: unflagged 7.4% of dollars in 2005, 14.0% in 2025",
      abs(unflag("CANADA", "exp", 2005) - 0.074) < 0.0005 &&
        abs(unflag("CANADA", "exp", 2025) - 0.140) < 0.0005,
      sprintf("got %.4f and %.4f", unflag("CANADA", "exp", 2005),
              unflag("CANADA", "exp", 2025)))
check("MEXICO is clean: 3.75% of export dollars, 0.76% of import dollars",
      abs(unflag("MEXICO", "exp") - 0.0375) < 0.0002 &&
        abs(unflag("MEXICO", "imp") - 0.0076) < 0.0002,
      sprintf("got %.4f and %.4f", unflag("MEXICO", "exp"),
              unflag("MEXICO", "imp")))

for (spec in list(list("336390", 0.30), list("332722", 0.41),
                  list("337127", 0.46), list("315250", 0.91))) {
  d <- panel[panel$country == "CANADA" & panel$direction == "exp" &
               panel$naics6 == spec[[1]], ]
  got <- sum(d$notreported) / sum(d$total)
  check(sprintf("CANADA exp %s is ~%.0f%% unflagged", spec[[1]],
                spec[[2]] * 100),
        abs(round(got, 2) - spec[[2]]) < 0.005, sprintf("got %.3f", got))
}

# Optional cross-check against a separately supplied copy of the 2011-2015
# benchmark. Skipped when the file is absent, which is the normal case.
ADV <- na_data("rp_crosscheck", "rp_naics6_2011.csv")
if (file.exists(ADV)) {
  a <- read.csv(ADV, colClasses = "character")
  names(a)[1:3] <- c("naics_raw", "excl", "lower")
  a$naics6 <- sub("^([A-Za-z0-9]+)\\s.*$", "\\1", a$naics_raw)
  a$excl  <- suppressWarnings(as.numeric(a$excl))
  a$lower <- suppressWarnings(as.numeric(a$lower))
  hit <- function(dir) {
    q <- panel[panel$country == "CANADA" & panel$direction == dir &
                 panel$year == 2011, c("naics6", "s_excl", "s_lower")]
    m <- merge(a, q, by = "naics6")
    both_na  <- is.na(m$excl) & is.na(m$s_excl)
    both_num <- !is.na(m$excl) & !is.na(m$s_excl)
    sum(both_na | (both_num & abs(m$excl - m$s_excl) < 1e-8))
  }
  check("advisor's 2011 file matches CANADA IMPORTS on all 456 codes",
        nrow(a) == 456 && hit("imp") == 456,
        sprintf("%d codes, %d match imports, %d match exports",
                nrow(a), hit("imp"), hit("exp")))
  check("...and matches the EXPORT side on only 5 of them",
        hit("exp") == 5, paste("got", hit("exp")))
} else {
  cat("  SKIP  advisor-file direction check (", ADV, " not found)\n", sep = "")
}

tau_list <- function(min_years) {
  d <- panel[panel$country == "CANADA" & panel$direction == "imp" &
               !is.na(panel$s_lower), ]
  n   <- tapply(d$s_lower, d$naics6, length)
  mn  <- tapply(d$s_lower, d$naics6, min)
  sort(names(mn)[mn >= 0.90 & n >= min_years])
}
check("tau=0.90 unstitched: no coverage floor gives 5 sectors",
      setequal(tau_list(1), c("336111", "336110", "326211", "332214",
                              "331319")),
      paste("got", paste(tau_list(1), collapse = ", ")))
check("tau=0.90 unstitched: >=8 years gives autos + tires",
      setequal(tau_list(8), c("336111", "326211")),
      paste("got", paste(tau_list(8), collapse = ", ")))
check("tau=0.90 unstitched: full 21-year window gives tires ONLY",
      setequal(tau_list(21), "326211"),
      paste("got", paste(tau_list(21), collapse = ", ")))

cat(sprintf("\n%d test(s) failed.\n", fails))
if (fails > 0) {
  stop("Tests failed - the panel above was written, but a figure quoted in ",
       "this script's comments no longer holds. Fix the comment or the code ",
       "before quoting either.")
}

# [FAILURE MODE: "threshold tuned post hoc" - symptom: the sector list changes
# after seeing results. Prevention: freeze the list, date-stamped.] Step 8 is
cat("\nDone. Threshold application (Step 8) deliberately NOT run here -\n")
cat("it depends on the advisor's answer on threshold vs. continuous design.\n")
