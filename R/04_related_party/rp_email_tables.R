# rp_email_tables.R
# Reproduces every number in the related-party summary tables and tests each
# against a pinned expected value, so a change to the panel underneath fails
# loudly rather than quietly printing something different.
#
# In:   $DATA/rp_panel/rp_share_panel_naics6.csv, rp_vintage_map.csv
# Out:  $DATA/rp_panel/
# Run:  Rscript R/04_related_party/rp_email_tables.R

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

IN_PANEL <- na_data("rp_panel/rp_share_panel_naics6.csv")
IN_MAP   <- na_data("rp_panel/rp_vintage_map.csv")
OUT_DIR  <- na_data("rp_panel")

for (f in c(IN_PANEL, IN_MAP)) {
  if (!file.exists(f)) {
    stop("Missing input: ", normalizePath(f, mustWork = FALSE), "\n",
         "  Run R/04_related_party/rp_build_share_panel.R then R/04_related_party/rp_apply_threshold.R first.")
  }
}

DIRECTION   <- "imp"      # US-perspective imports = partner -> US = the flow
MEASURE     <- "s_lower"  # protocol Step 4 primary: unflagged trade counted as
COUNTRIES   <- c("CANADA", "MEXICO")
TAUS        <- c(0.95, 0.90, 0.85, 0.80, 0.70)
TAU_REPORT  <- 0.90       # the threshold the named sector lists describe

MIN_PEAK_VALUE <- 1e8     # USD, peak annual trade  (NOT from the protocol)
REQUIRE_FULL_COVERAGE <- TRUE   # this half IS protocol Step 8

STRUCTURAL_EXCLUSIONS <- c("990000")

WINDOWS <- c(2005, 2010)

panel <- read.csv(IN_PANEL, stringsAsFactors = FALSE,
                  colClasses = c(naics6 = "character"))
vmap  <- read.csv(IN_MAP, stringsAsFactors = FALSE,
                  colClasses = c(naics6 = "character", lineage = "character"))

d <- merge(panel[panel$direction == DIRECTION, ],
           vmap[, c("naics6", "lineage")], by = "naics6", all.x = TRUE)

d$lineage[is.na(d$lineage)] <- d$naics6[is.na(d$lineage)]
d <- d[!(d$lineage %in% STRUCTURAL_EXCLUSIONS), ]

lab_src <- d[!is.na(d$total), ]
lab_src <- lab_src[order(-lab_src$total), ]
LABEL <- setNames(lab_src$naics_desc[!duplicated(lab_src$lineage)],
                  lab_src$lineage[!duplicated(lab_src$lineage)])

series_stats <- function(start_year) {
  s <- d[d$year >= start_year, ]
  n_window <- length(unique(s$year))

  agg <- aggregate(cbind(total, related, nonrelated, notreported) ~
                     country + lineage + year, data = s, FUN = sum)

  den <- agg$related + agg$nonrelated + agg$notreported
  agg$share <- switch(MEASURE,
    s_lower = ifelse(den > 0, agg$related / den, NA_real_),
    s_excl  = ifelse(agg$related + agg$nonrelated > 0,
                     agg$related / (agg$related + agg$nonrelated), NA_real_),
    s_upper = ifelse(den > 0, (agg$related + agg$notreported) / den, NA_real_),
    stop("MEASURE must be one of s_lower, s_excl, s_upper"))

  agg <- agg[!is.na(agg$share) & agg$total > 0, ]

  key   <- paste(agg$country, agg$lineage, sep = "|")
  parts <- do.call(rbind, strsplit(unique(key), "|", fixed = TRUE))
  out <- data.frame(country = parts[, 1], lineage = parts[, 2],
                    stringsAsFactors = FALSE)
  out$n_years  <- tapply(agg$year,  key, function(y) length(unique(y)))[unique(key)]
  out$peak     <- tapply(agg$total, key, max)[unique(key)]
  out$min_s    <- tapply(agg$share, key, min)[unique(key)]
  out$mean_s   <- tapply(agg$share, key, mean)[unique(key)]
  out$desc     <- LABEL[out$lineage]

  out$eligible <- out$peak > MIN_PEAK_VALUE &
    (if (REQUIRE_FULL_COVERAGE) out$n_years == n_window else out$n_years >= 8)

  attr(out, "n_window")   <- n_window
  attr(out, "start_year") <- start_year
  out
}

STATS <- lapply(WINDOWS, series_stats)
names(STATS) <- as.character(WINDOWS)

count_row <- function(st, ctry) {
  e <- st[st$eligible, ]
  if (!is.null(ctry)) e <- e[e$country == ctry, ]
  vapply(TAUS, function(t) sum(e$min_s >= t), integer(1))
}

cat("=========================================================\n")
cat("rp_email_tables.R - reproducing the 2026-09-02 advisor email\n")
cat("=========================================================\n")
cat(sprintf("direction = %s (partner -> US)   measure = %s   rule = every year\n",
            DIRECTION, MEASURE))
cat(sprintf("eligible  = peak annual trade > $%s and %s\n",
            format(MIN_PEAK_VALUE, big.mark = ",", scientific = FALSE),
            if (REQUIRE_FULL_COVERAGE) "present in EVERY year of the window"
            else "present in >= 8 years"))
cat(sprintf("excluded  = %s\n", paste(STRUCTURAL_EXCLUSIONS, collapse = ", ")))

for (w in as.character(WINDOWS)) {
  st <- STATS[[w]]
  cat(sprintf("\n%s-2025  (%d years)\n", w, attr(st, "n_window")))
  cat(sprintf("%-10s", ""))
  for (t in TAUS) cat(sprintf("%9s", paste0("t=", format(t, nsmall = 2))))
  cat("\n")
  for (ctry in c(COUNTRIES, "Pooled")) {
    r <- count_row(st, if (ctry == "Pooled") NULL else ctry)
    cat(sprintf("%-10s", ctry))
    for (v in r) cat(sprintf("%9d", v))
    cat("\n")
  }
}

pass_list <- function(st, ctry, tau) {
  e <- st[st$eligible & st$country == ctry & st$min_s >= tau, ]
  e[order(-e$peak), ]
}

cat(sprintf("\n---------------------------------------------------------\n"))
cat(sprintf("QUALIFYING SECTORS at tau = %.2f\n", TAU_REPORT))
cat("---------------------------------------------------------\n")
for (w in as.character(WINDOWS)) {
  cat(sprintf("\n%s-2025\n", w))
  for (ctry in COUNTRIES) {
    pl <- pass_list(STATS[[w]], ctry, TAU_REPORT)
    cat(sprintf("  %s (%d):\n", ctry, nrow(pl)))
    if (nrow(pl) == 0) cat("    (none)\n")
    for (i in seq_len(nrow(pl))) {
      cat(sprintf("    %-9s %-46s min %.3f  mean %.3f  $%6.2fB\n",
                  pl$lineage[i], substr(pl$desc[i], 1, 44),
                  pl$min_s[i], pl$mean_s[i], pl$peak[i] / 1e9))
    }
  }
}

pet <- panel[panel$direction == DIRECTION & panel$country == "CANADA", ]
pet_stat <- function(code) {
  s <- pet[pet$naics6 == code & !is.na(pet[[MEASURE]]), ]
  c(n = nrow(s), mean = mean(s[[MEASURE]]), max = max(s[[MEASURE]]),
    max_year = s$year[which.max(s[[MEASURE]])])
}
p_old <- pet_stat("211111")
p_new <- pet_stat("211120")

r11 <- pet[pet$naics6 == "211111" & pet$year == 2011, ]
his_number <- r11$related / (r11$related + r11$nonrelated)

cat("\n---------------------------------------------------------\n")
cat("PETROLEUM (Canada, s_lower unless noted)\n")
cat("---------------------------------------------------------\n")
cat(sprintf("  211111 crude+gas, %d yrs : mean %.4f   peak %.4f (%d)\n",
            p_old["n"], p_old["mean"], p_old["max"], p_old["max_year"]))
cat(sprintf("  211120 crude alone, %d yrs: mean %.4f   peak %.4f (%d)\n",
            p_new["n"], p_new["mean"], p_new["max"], p_new["max_year"]))
cat(sprintf("  211111 in 2011, s_excl    : %.4f  <- the figure in his file\n",
            his_number))

parts <- panel[panel$direction == DIRECTION & panel$country == "CANADA" &
                 substr(panel$naics6, 1, 4) == "3363" &
                 !is.na(panel[[MEASURE]]), ]
pk <- aggregate(list(max_s = parts[[MEASURE]]), by = list(naics6 = parts$naics6),
                FUN = max)
pk <- pk[order(-pk$max_s), ]
best_year <- parts$year[parts$naics6 == pk$naics6[1] &
                          parts[[MEASURE]] == pk$max_s[1]][1]

cat("\nAUTO PARTS (NAICS 3363, Canada) - highest annual share ever reached\n")
for (i in seq_len(min(4, nrow(pk)))) {
  cat(sprintf("  %-8s max %.4f\n", pk$naics6[i], pk$max_s[i]))
}
cat(sprintf("  -> %d of %d parts codes ever reach 0.90;  best = %.4f (%s, %d)\n",
            sum(pk$max_s >= 0.90), nrow(pk), pk$max_s[1], pk$naics6[1], best_year))

cat("\n--- tests: each expected value is a figure in the sent email ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails <<- fails + 1 }
}
eq <- function(a, b, tol = 5e-4) isTRUE(abs(a - b) < tol)

EXPECTED <- list(
  "2005" = list(CANADA = c(1, 2, 2, 3, 4),
                MEXICO = c(1, 2, 3, 4, 16),
                Pooled = c(2, 4, 5, 7, 20)),
  "2010" = list(CANADA = c(2, 3, 3, 3, 4),
                MEXICO = c(2, 5, 6, 8, 23),
                Pooled = c(4, 8, 9, 11, 27)))

for (w in names(EXPECTED)) {
  for (row in names(EXPECTED[[w]])) {
    got  <- count_row(STATS[[w]], if (row == "Pooled") NULL else row)
    want <- EXPECTED[[w]][[row]]
    check(sprintf("%s-2025 %-6s  %s", w, row, paste(want, collapse = " ")),
          identical(as.integer(got), as.integer(want)),
          paste("got", paste(got, collapse = " ")))
  }
}

p05 <- count_row(STATS[["2005"]], NULL)[which(TAUS == 0.90)]
p10 <- count_row(STATS[["2010"]], NULL)[which(TAUS == 0.90)]
check("headline: pooled tau=0.90 goes 4 -> 8", p05 == 4 && p10 == 8,
      sprintf("got %d -> %d", p05, p10))

check("2005 Canada tau=.90 == {336110+, 326211}",
      setequal(pass_list(STATS[["2005"]], "CANADA", 0.90)$lineage,
               c("336110+", "326211")))
check("2005 Mexico tau=.90 == {336120, 333991}",
      setequal(pass_list(STATS[["2005"]], "MEXICO", 0.90)$lineage,
               c("336120", "333991")))
check("2010 Canada tau=.90 == {336110+, 326211, 331315}",
      setequal(pass_list(STATS[["2010"]], "CANADA", 0.90)$lineage,
               c("336110+", "326211", "331315")))
check("2010 Mexico tau=.90 == {336110+, 336120, 335220+, 333991, 336612}",
      setequal(pass_list(STATS[["2010"]], "MEXICO", 0.90)$lineage,
               c("336110+", "336120", "335220+", "333991", "336612")))

check("petroleum: 211111 mean = 0.558", eq(p_old["mean"], 0.558),
      sprintf("got %.4f", p_old["mean"]))
check("petroleum: 211111 peak = 0.716", eq(p_old["max"], 0.716),
      sprintf("got %.4f", p_old["max"]))
check("petroleum: 211120 mean = 0.832", eq(p_new["mean"], 0.832),
      sprintf("got %.4f", p_new["mean"]))
check("petroleum: 211120 peak = 0.850", eq(p_new["max"], 0.850),
      sprintf("got %.4f", p_new["max"]))
check("petroleum: his file's 0.6968 is s_excl for 2011",
      eq(his_number, 0.6968, 1e-4), sprintf("got %.4f", his_number))

check("auto parts: NO 3363 code reaches 0.90 in any year",
      sum(pk$max_s >= 0.90) == 0,
      sprintf("%d code(s) do", sum(pk$max_s >= 0.90)))
check("auto parts: best is 0.833", eq(pk$max_s[1], 0.833),
      sprintf("got %.4f", pk$max_s[1]))
check("auto parts: best year is 2009", best_year == 2009,
      sprintf("got %d", best_year))

check("990000 excluded from every window",
      !any(unlist(lapply(STATS, function(s) "990000" %in% s$lineage))))
for (w in as.character(WINDOWS)) {
  y <- as.integer(w)
  raw <- d[d$year >= y & !is.na(d$total), ]
  s   <- d[d$year >= y, ]
  agg <- aggregate(cbind(total) ~ country + lineage + year, data = s, FUN = sum)
  check(sprintf("%s-2025 stitching preserves total value", w),
        eq(sum(agg$total), sum(raw$total), 1),
        sprintf("raw %.4g vs stitched %.4g", sum(raw$total), sum(agg$total)))
}

cat(sprintf("\n%d test(s) failed.\n", fails))
if (fails > 0) {
  stop("The email contains at least one figure this script cannot reproduce. ",
       "Do not re-send any of these numbers until resolved.")
}
cat("\nAll figures in the 2026-09-02 email reproduce exactly.\n")
