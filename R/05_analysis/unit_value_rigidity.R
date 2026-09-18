# unit_value_rigidity.R
# How often do the prices of goods crossing the border actually change?
# Frequency-of-price-change statistics in the sticky-price tradition
# (Nakamura-Steinsson 2008; Gopinath-Rigobon 2008).
#
# In:   $DATA/panel/ via panel_open.R
# Out:  results on stdout
# Run:  Rscript R/05_analysis/unit_value_rigidity.R
#
# A UNIT VALUE IS NOT A PRICE. It is value divided by quantity, summed over
# every transaction in an HS10 code in a month - different firms, different
# contract terms, different exact products inside one tariff line. When the mix
# shifts the unit value moves although no seller changed any price. What this
# measures is an UPPER BOUND on the frequency of price change, and therefore a
# LOWER BOUND on duration. MIN_CHANGE blunts that, and because it is a
# judgement the script reports the whole frequency-vs-threshold curve rather
# than one number.
#
# THE DEFAULT WINDOW IS ONE INTER-REVISION REGIME. HS10 codes are only stable
# between WCO revisions (2007, 2012, 2017, 2022): across one, codes carrying
# real trade disappear at 10-20x the baseline rate, and there is no official
# over-time HS10 concordance to bridge them. Within a regime, 94-99% of
# material codes survive. Set YEARS to another regime to check robustness; do
# NOT set it to span one.

suppressMessages(library(vroom))

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

na_source("R/02_panel/panel_open.R")

if (!exists("COUNTRY"))    COUNTRY    <- "CA"        # "CA" or "MX"
if (!exists("DIRECTION"))  DIRECTION  <- "imp"       # "imp" = partner -> US (the DCP flow)
if (!exists("YEARS"))      YEARS      <- 2017:2021   # ONE inter-revision regime
if (!exists("MIN_MONTHS")) MIN_MONTHS <- 48L         # of 60; how complete a series must be
if (!exists("MIN_CHANGE")) MIN_CHANGE <- 0.05        # |dlog| below this = "no change"
if (!exists("THRESHOLDS")) THRESHOLDS <- c(0, 0.01, 0.025, 0.05, 0.10, 0.20)
if (!exists("MIN_VALUE"))  MIN_VALUE  <- 1e5         # drop trivial code-months

fmt <- function(x) format(x, big.mark = ",")
hdr <- function(s) cat("\n", s, "\n", strrep("-", nchar(s)), "\n", sep = "")

cat(strrep("=", 78), "\n")
cat(sprintf("UNIT VALUE RIGIDITY   %s %s   %d-%d\n", COUNTRY,
            if (DIRECTION == "imp") "-> US" else "<- US", min(YEARS), max(YEARS)))
cat(strrep("=", 78), "\n")

d <- panel_slice(reporter = "US", partner = COUNTRY, direction = DIRECTION,
                 years = YEARS,
                 cols = c("period", "hs_code", "basis", "value",
                          "quantity_1", "unit_1"))

if (DIRECTION == "exp") {
  d <- aggregate(cbind(value, quantity_1) ~ period + hs_code + unit_1, d, sum)
  cat("exports: domestic and foreign summed to one series per code-month\n")
}

n0 <- nrow(d)

drop_x   <- sum(!is.na(d$unit_1) & d$unit_1 == "X")
drop_na  <- sum(is.na(d$unit_1))
drop_q0  <- sum(!is.na(d$quantity_1) & d$quantity_1 <= 0)
drop_v   <- sum(d$value < MIN_VALUE)

d <- d[!is.na(d$unit_1) & d$unit_1 != "X" &
       !is.na(d$quantity_1) & d$quantity_1 > 0 &
       !is.na(d$value) & d$value >= MIN_VALUE, ]

hdr("1. FILTERING")
cat(sprintf("  rows in slice                         %10s\n", fmt(n0)))
cat(sprintf("  unit is 'X' (no quantity collected)   -%9s\n", fmt(drop_x)))
cat(sprintf("  unit missing                          -%9s\n", fmt(drop_na)))
cat(sprintf("  quantity zero or negative             -%9s\n", fmt(drop_q0)))
cat(sprintf("  value below $%-11s         -%9s\n",
            formatC(MIN_VALUE, format = "d", big.mark = ","), fmt(drop_v)))
cat(sprintf("  usable code-months                    %10s\n", fmt(nrow(d))))

u <- unique(d[, c("hs_code", "unit_1")])
multi <- unique(u$hs_code[duplicated(u$hs_code)])
if (length(multi)) d <- d[!d$hs_code %in% multi, ]
cat(sprintf("\n  codes whose unit_1 changed mid-window  -%9s (dropped)\n",
            fmt(length(multi))))

d$uv <- d$value / d$quantity_1
d$t  <- as.integer(substr(d$period, 1, 4)) * 12L + as.integer(substr(d$period, 5, 6))

cnt <- table(d$hs_code)
keep <- names(cnt)[cnt >= MIN_MONTHS]
d <- d[d$hs_code %in% keep, ]
nmo <- length(unique(d$period))
hdr("2. PANEL")
cat(sprintf("  months in window                      %10d\n", nmo))
cat(sprintf("  codes with >= %d months                %10s\n", MIN_MONTHS, fmt(length(keep))))
cat(sprintf("  code-months retained                  %10s\n", fmt(nrow(d))))
cat(sprintf("  balancedness                          %9.1f%%\n",
            100 * nrow(d) / (length(keep) * nmo)))
if (!nrow(d)) stop("nothing survived the filters - loosen MIN_MONTHS or MIN_VALUE")

d <- d[order(d$hs_code, d$t), ]
same <- c(FALSE, d$hs_code[-1] == d$hs_code[-nrow(d)])
adj  <- c(FALSE, diff(d$t) == 1L) & same
dl   <- c(NA, diff(log(d$uv)))
ch   <- data.frame(hs_code = d$hs_code[adj], t = d$t[adj], dlog = dl[adj])
cat(sprintf("  adjacent-month pairs                  %10s\n", fmt(nrow(ch))))
cat(sprintf("  pairs lost to gaps                    %10s\n",
            fmt(sum(same) - sum(adj))))

exact0 <- mean(abs(ch$dlog) < 1e-10)
hdr("3. CAN THIS BE READ AS A PRICE AT ALL?")
cat(sprintf("  changes of EXACTLY zero               %9.2f%%\n", 100 * exact0))
cat(sprintf("  changes under 1%%                      %9.2f%%\n",
            100 * mean(abs(ch$dlog) < 0.01)))
cat(sprintf("  median |change|, all pairs            %9.2f%%\n",
            100 * median(abs(ch$dlog))))
if (exact0 < 0.05) {
  cat("\n  *** WARNING - almost nothing repeats exactly. ***\n")
  cat("  Transaction-price data shows a large spike at zero; this does not.\n")
  cat("  The series is dominated by within-code composition change, so the\n")
  cat("  frequencies below measure the product mix moving, NOT sellers\n")
  cat("  repricing. Treat them as an upper bound so loose it is close to\n")
  cat("  uninformative, and do not quote the implied duration as a stickiness\n")
  cat("  estimate. Measuring that properly needs transaction- or firm-level\n")
  cat("  data (BLS IPP, or LFTTD), which is why Gopinath-Rigobon (2008) used\n")
  cat("  the BLS micro data rather than customs unit values.\n")
} else {
  cat("\n  A meaningful share of changes are exactly zero, so the frequency\n")
  cat("  statistics below carry some price signal.\n")
}

hdr("4. FREQUENCY OF PRICE CHANGE vs THRESHOLD")
cat("  |dlog| >    freq      implied duration (months)\n")
for (th in THRESHOLDS) {
  f <- mean(abs(ch$dlog) > th)
  dur <- if (f > 0 && f < 1) -1 / log(1 - f) else NA_real_
  cat(sprintf("  %6.3f   %7.2f%%   %s%s\n", th, 100 * f,
              if (is.na(dur)) "  n/a" else sprintf("%7.1f", dur),
              if (isTRUE(all.equal(th, MIN_CHANGE))) "   <- MIN_CHANGE" else ""))
}

big <- abs(ch$dlog) > MIN_CHANGE
f <- mean(big)
hdr(sprintf("5. AT MIN_CHANGE = %.3f", MIN_CHANGE))
cat(sprintf("  frequency of change                   %9.2f%% per month\n", 100 * f))
cat(sprintf("  implied duration                      %9.1f months\n", -1 / log(1 - f)))
cat(sprintf("  median |change| when it changes       %9.2f%%\n",
            100 * median(abs(ch$dlog[big]))))
cat(sprintf("  share of changes that are increases   %9.1f%%\n",
            100 * mean(ch$dlog[big] > 0)))
cat(sprintf("  median increase / decrease            %8.2f%% / %.2f%%\n",
            100 * median(ch$dlog[big & ch$dlog > 0]),
            100 * median(ch$dlog[big & ch$dlog < 0])))

pc <- tapply(abs(ch$dlog) > MIN_CHANGE, ch$hs_code, mean)
hdr("6. DISPERSION ACROSS CODES (per-code frequency)")
print(round(quantile(pc, c(.1, .25, .5, .75, .9)), 4))
cat(sprintf("\n  codes that NEVER move more than %.0f%%:  %s of %s (%.1f%%)\n",
            100 * MIN_CHANGE, fmt(sum(pc == 0)), fmt(length(pc)),
            100 * mean(pc == 0)))

hdr("7. BY HS CHAPTER GROUP (top 8 by value)")
ch$ch2 <- substr(ch$hs_code, 1, 2)
val <- tapply(d$value, substr(d$hs_code, 1, 2), sum)
top <- names(sort(val, decreasing = TRUE))[1:8]
cat(sprintf("  %-4s %10s %10s %12s\n", "ch", "freq", "dur(mo)", "value $bn"))
for (c2 in top) {
  s <- ch$dlog[ch$ch2 == c2]
  if (length(s) < 100) next
  f2 <- mean(abs(s) > MIN_CHANGE)
  cat(sprintf("  %-4s %9.2f%% %10.1f %12.1f\n", c2, 100 * f2,
              if (f2 > 0 && f2 < 1) -1 / log(1 - f2) else NA_real_,
              val[[c2]] / 1e9))
}

hdr("CAVEAT")
cat("  These are unit values, not transaction prices. Composition shifts\n")
cat("  inside an HS10 code move the unit value with no price change, so the\n")
cat("  frequency above is an UPPER BOUND and the duration a LOWER BOUND.\n")
cat("  Re-run with a different YEARS regime to check the window is not\n")
cat("  driving the result; do not span an HS revision (2007/2012/2017/2022).\n\n")
