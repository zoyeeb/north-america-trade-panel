# rp_apply_threshold.R
# [PROTOCOL Steps 6 and 8] The NAICS vintage audit carried through to a
# stitched series, with the threshold tradeoff curve computed on top of it.
#
# In:   $DATA/rp_panel/rp_share_panel_naics6.csv, rp_vintage_audit.csv,
#       $DATA/naics_concordance/naics_concordance_edges.csv
# Out:  $DATA/rp_panel/
# Run:  Rscript R/04_related_party/rp_apply_threshold.R
#
# The tradeoff curve (how many sectors survive at each tau) is a reported
# figure, so it is computed here rather than interactively, and the script pins
# the values it produces.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

IN_PANEL <- na_data("rp_panel/rp_share_panel_naics6.csv")
OUT_DIR  <- na_data("rp_panel")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(IN_PANEL)) {
  stop("Share panel not found at ", normalizePath(IN_PANEL, mustWork = FALSE),
       "\n  Run R/04_related_party/rp_build_share_panel.R first.")
}

COUNTRY   <- "CANADA"
DIRECTION <- "imp"

# [PROTOCOL Step 8] The thresholds to report.
TAU_GRID    <- c(0.95, 0.90, 0.85, 0.80, 0.70, 0.50)
TAU_PRIMARY <- 0.90

# [PROTOCOL Step 4] The three defensible treatments of "Other" (trade whose
# related-party indicator is missing on the record): s_lower = R / (R+N+O)
SHARE_MEASURES <- c("s_lower", "s_excl", "s_upper")
SHARE_PRIMARY  <- "s_lower"

ELIGIBILITY_GRID <- list(
  protocol = list(
    label      = "Step 8 as written - full window, no size floor",
    value_stat = NA, value_min = NA, min_years = "full"),
  peak100M_full = list(
    label      = "peak > $100M + full window",
    value_stat = "max", value_min = 1e8, min_years = "full"),
  n8_nosize = list(
    label      = ">= 8 years, no size floor",
    value_stat = NA, value_min = NA, min_years = 8),
  peak100M_n8 = list(
    label      = "peak > $100M + >= 8 years  (this project's convention to 2026-09-03)",
    value_stat = "max", value_min = 1e8, min_years = 8),
  no_screen = list(
    label      = "no screen at all - every series with a computable share",
    value_stat = NA, value_min = NA, min_years = 1)
)

ELIG_NARRATIVE <- "protocol"

STITCH_PRIMARY <- TRUE

STRUCTURAL_EXCLUSIONS <- c("990000", "980000", "930000", "920000", "910000")

panel <- read.csv(IN_PANEL, stringsAsFactors = FALSE,
                  colClasses = c(naics6 = "character"))

need <- c("country", "direction", "naics6", "naics_desc", "year", "total",
          "related", "nonrelated", "notreported", "s_lower", "s_excl",
          "s_upper", "other_share")
missing <- setdiff(need, names(panel))
if (length(missing)) {
  stop("Share panel is missing column(s): ", paste(missing, collapse = ", "),
       "\n  Expected the output of rp_build_share_panel.R.")
}

ALL_YEARS  <- sort(unique(panel$year))
FULL_SPAN  <- length(ALL_YEARS)

cat("=========================================================\n")
cat("rp_apply_threshold.R  [PROTOCOL Step 6 + Step 8]\n")
cat("=========================================================\n")
cat(sprintf("panel      : %d rows, %d years (%d-%d)\n",
            nrow(panel), FULL_SPAN, min(ALL_YEARS), max(ALL_YEARS)))
cat(sprintf("cut        : %s, direction=%s (US perspective)\n",
            COUNTRY, DIRECTION))
cat("eligibility: NOT SET - every regime reported side by side\n")
for (nm in names(ELIGIBILITY_GRID)) {
  cat(sprintf("             %-14s %s\n", nm, ELIGIBILITY_GRID[[nm]]$label))
}

pos <- panel[!is.na(panel$total) & panel$total > 0, ]

VINT <- na_data("rp_panel/rp_vintage_audit.csv")
if (!file.exists(VINT)) {
  stop("Vintage audit not found at ", normalizePath(VINT, mustWork = FALSE),
       "
  Run:  Rscript R/04_related_party/rp_build_share_panel.R
",
       "  This script no longer recomputes the year ranges - it reads them, ",
       "so that the audit a human reads and the map this builds cannot drift ",
       "apart.")
}
rng <- read.csv(VINT, stringsAsFactors = FALSE, colClasses = c(naics6 = "character"))
rng$y0 <- as.integer(sub("-.*$", "", rng$year_range))
rng$y1 <- as.integer(sub("^.*-", "", rng$year_range))
rng$n  <- rng$n_years
rng <- rng[, c("country", "direction", "naics6", "y0", "y1", "n")]

CONC <- na_data("naics_concordance/naics_concordance_edges.csv")
if (!file.exists(CONC)) {
  stop("Official NAICS concordance not found at ",
       normalizePath(CONC, mustWork = FALSE), "\n",
       "  Run:  Rscript code/naics_concordance/naics_concordance_fetch.R\n",
       "  Refusing to fall back to the heuristic alone - protocol Step 6.2 ",
       "requires the official tables, and the heuristic alone over-merges.")
}
conc <- read.csv(CONC, stringsAsFactors = FALSE,
                 colClasses = c(old_code = "character", new_code = "character"))
official_pairs <- unique(conc[conc$code_changed == 1,
                              c("old_code", "new_code")])
names(official_pairs) <- c("pred", "succ")

official_codes <- unique(c(conc$old_code, conc$new_code))

heur_pairs <- data.frame(pred = character(0), succ = character(0),
                         stringsAsFactors = FALSE)
n_rejected <- 0

blocked_pairs <- data.frame(pred = character(0), succ = character(0),
                            stringsAsFactors = FALSE)

pooled <- aggregate(year ~ naics6, data = pos,
                    FUN = function(y) c(min(y), max(y)))
pooled <- data.frame(naics6 = pooled$naics6,
                     py0 = pooled$year[, 1], py1 = pooled$year[, 2],
                     stringsAsFactors = FALSE)
retired_pooled <- pooled$naics6[pooled$py1 < max(ALL_YEARS)]
appeared_pooled <- pooled$naics6[pooled$py0 > min(ALL_YEARS)]

for (grp in split(rng, list(rng$country, rng$direction), drop = TRUE)) {
  ends   <- grp[grp$y1 < max(ALL_YEARS) & grp$n < FULL_SPAN &
                grp$naics6 %in% retired_pooled, ]
  starts <- grp[grp$y0 > min(ALL_YEARS) & grp$n < FULL_SPAN &
                grp$naics6 %in% appeared_pooled, ]
  if (!nrow(ends) || !nrow(starts)) next
  for (i in seq_len(nrow(ends))) {
    hit <- starts$naics6 != ends$naics6[i] &
           substr(starts$naics6, 1, 4) == substr(ends$naics6[i], 1, 4) &
           starts$y0 == ends$y1[i] + 1
    if (!any(hit)) next
    cand <- data.frame(pred = ends$naics6[i], succ = starts$naics6[hit],
                       stringsAsFactors = FALSE)
    keep <- !(cand$pred %in% official_codes) & !(cand$succ %in% official_codes)
    n_rejected <- n_rejected + sum(!keep)
    if (any(keep)) heur_pairs <- rbind(heur_pairs, cand[keep, , drop = FALSE])

    orphan <- !(cand$pred %in% official_codes) & (cand$succ %in% official_codes)
    if (any(orphan)) {
      blocked_pairs <<- rbind(blocked_pairs, cand[orphan, , drop = FALSE])
    }
  }
}
heur_pairs <- unique(heur_pairs)
blocked_pairs <- unique(blocked_pairs)

in_panel <- unique(rng$naics6)
official_pairs <- official_pairs[official_pairs$pred %in% in_panel &
                                 official_pairs$succ %in% in_panel, ]

HEUR_APPLY <- FALSE

pairs <- if (HEUR_APPLY) rbind(official_pairs, heur_pairs) else official_pairs
pairs$source <- if (HEUR_APPLY) {
  c(rep("concordance", nrow(official_pairs)), rep("heuristic", nrow(heur_pairs)))
} else {
  rep("concordance", nrow(official_pairs))
}
pairs <- unique(pairs)

cat(sprintf("\n[Step 6.2] official concordance: %d changed-code edges, %d within this panel\n",
            nrow(unique(conc[conc$code_changed == 1, c("old_code", "new_code")])),
            nrow(official_pairs)))
cat(sprintf("           fallback scan (REPORT-ONLY, applied to nothing): %d candidate(s)\n",
            nrow(heur_pairs)))
if (nrow(heur_pairs)) {
  for (i in seq_len(nrow(heur_pairs))) {
    cat(sprintf("             %s -> %s  (not joined - map is official tables only)\n",
                heur_pairs$pred[i], heur_pairs$succ[i]))
  }
}
cat(sprintf("           %d further candidate(s) rejected: the official tables\n", n_rejected))
cat("           cover those codes and take precedence\n")

if (nrow(blocked_pairs)) {
  desc1 <- function(k) {
    d <- panel$naics_desc[panel$naics6 == k]
    if (length(d)) substr(d[1], 1, 34) else ""
  }
  cat(sprintf("\n           KNOWN GAP - %d pseudo-code handoff(s) blocked by that rule:\n",
              nrow(blocked_pairs)))
  for (i in seq_len(nrow(blocked_pairs))) {
    pr <- blocked_pairs$pred[i]; sc <- blocked_pairs$succ[i]
    same <- identical(desc1(pr), desc1(sc))
    cat(sprintf("             %s -> %s  %-34s %s\n", pr, sc, desc1(sc),
                if (same) "<- SAME DESCRIPTION" else ""))
  }
  cat("           These stay UNBRIDGED. Neither the concordance nor the\n")
  cat("           fallback reaches them; closing the gap needs a rule the\n")
  cat("           advisor has not been asked for. See SECTION 3b.\n")
}

lineage <- as.list(setNames(unique(rng$naics6), unique(rng$naics6)))
lineage <- setNames(as.list(names(lineage)), names(lineage))  # code -> members
repeat {
  changed <- FALSE
  for (i in seq_len(nrow(pairs))) {
    a <- pairs$pred[i]; b <- pairs$succ[i]
    ka <- names(lineage)[vapply(lineage, function(m) a %in% m, logical(1))]
    kb <- names(lineage)[vapply(lineage, function(m) b %in% m, logical(1))]
    if (length(ka) && length(kb) && ka[1] != kb[1]) {
      lineage[[ka[1]]] <- sort(unique(c(lineage[[ka[1]]], lineage[[kb[1]]])))
      lineage[[kb[1]]] <- NULL
      changed <- TRUE
    }
  }
  if (!changed) break
}

map_code <- character(0); map_lin <- character(0)
for (members in lineage) {
  id <- if (length(members) == 1) members else paste0(min(members), "+")
  map_code <- c(map_code, members)
  map_lin  <- c(map_lin, rep(id, length(members)))
}
vintage_map <- data.frame(naics6 = map_code, lineage = map_lin,
                          stringsAsFactors = FALSE)
vintage_map$n_members <- ave(vintage_map$lineage, vintage_map$lineage,
                             FUN = length)
vintage_map$n_members <- as.integer(vintage_map$n_members)

lin_source <- vapply(split(vintage_map$naics6, vintage_map$lineage), function(mem) {
  if (length(mem) == 1) return("single code - no merge")
  rel <- pairs[pairs$pred %in% mem & pairs$succ %in% mem, ]
  s <- unique(rel$source)
  if (!length(s)) "unknown" else if (length(s) == 1) s else "mixed"
}, character(1))
vintage_map$lineage_source <- lin_source[vintage_map$lineage]

desc1 <- aggregate(naics_desc ~ naics6, data = pos,
                   FUN = function(x) x[1])
vintage_map <- merge(vintage_map, desc1, by = "naics6", all.x = TRUE)
span <- aggregate(year ~ naics6, data = pos,
                  FUN = function(y) paste0(min(y), "-", max(y)))
names(span)[2] <- "years_observed"
vintage_map <- merge(vintage_map, span, by = "naics6", all.x = TRUE)
vintage_map <- vintage_map[order(vintage_map$lineage, vintage_map$naics6), ]

n_merged_lineages <- length(unique(vintage_map$lineage[vintage_map$n_members > 1]))
n_merged_codes    <- sum(vintage_map$n_members > 1)

cat(sprintf("\n[Step 6] vintage map:\n"))
cat(sprintf("  %d codes -> %d lineages;  %d codes merged into %d multi-code lineages\n",
            nrow(vintage_map), length(unique(vintage_map$lineage)),
            n_merged_codes, n_merged_lineages))
srcs <- table(vintage_map$lineage_source[vintage_map$n_members > 1])
for (s in names(srcs)) {
  cat(sprintf("    %-14s %3d code(s) in %d lineage(s)\n", s, srcs[[s]],
              length(unique(vintage_map$lineage[
                vintage_map$n_members > 1 &
                vintage_map$lineage_source == s]))))
}

if (nrow(pairs)) {
  brk <- table(rng$y0[rng$naics6 %in% pairs$succ & rng$y0 > min(ALL_YEARS)])
  cat("  successor start years (should cluster at NAICS revision vintages):\n    ")
  cat(paste(names(brk), as.integer(brk), sep = ":", collapse = "  "), "\n")
}

out_map <- file.path(OUT_DIR, "rp_vintage_map.csv")
write.csv(vintage_map, out_map, row.names = FALSE)
cat(sprintf("  saved %s\n", out_map))

cut <- panel[panel$country == COUNTRY & panel$direction == DIRECTION, ]
if (!nrow(cut)) {
  stop("No rows for country=", COUNTRY, " direction=", DIRECTION,
       ". Check SECTION 1 against the panel's own values.")
}

# [FAILURE MODE: "suppressed cells read as zero".] Where a series has no trade
# the share is undefined, not zero.
add_shares <- function(d) {
  den_all <- d$related + d$nonrelated + d$notreported
  den_rep <- d$related + d$nonrelated
  d$s_lower <- ifelse(den_all > 0, d$related / den_all, NA_real_)
  d$s_excl  <- ifelse(den_rep > 0, d$related / den_rep, NA_real_)
  d$s_upper <- ifelse(den_all > 0, (d$related + d$notreported) / den_all,
                      NA_real_)
  d
}

unst <- cut[, c("naics6", "naics_desc", "year", "total", "related",
                "nonrelated", "notreported")]
names(unst)[1] <- "series"
unst <- add_shares(unst)

st <- merge(cut, vintage_map[, c("naics6", "lineage")], by = "naics6",
            all.x = TRUE)
st$lineage[is.na(st$lineage)] <- st$naics6[is.na(st$lineage)]
agg <- aggregate(cbind(total, related, nonrelated, notreported) ~ lineage + year,
                 data = st, FUN = sum, na.rm = TRUE)
names(agg)[1] <- "series"
lab_src <- st[!is.na(st$total), ]
lab <- do.call(rbind, lapply(split(lab_src, lab_src$lineage), function(d) {
  by_code <- aggregate(total ~ naics6 + naics_desc, data = d, FUN = max)
  by_code <- by_code[order(-by_code$total), ]
  data.frame(series = d$lineage[1], naics_desc = by_code$naics_desc[1],
             stringsAsFactors = FALSE)
}))
agg <- merge(agg, lab, by = "series", all.x = TRUE)
stit <- add_shares(agg)

summarise_series <- function(d) {
  d <- d[!is.na(d$total) & d$total > 0, ]
  keys <- unique(d$series)
  out <- data.frame(series = keys, stringsAsFactors = FALSE)
  out$naics_desc <- vapply(keys, function(k) d$naics_desc[d$series == k][1],
                           character(1))
  out$n_years <- vapply(keys, function(k) length(unique(d$year[d$series == k])),
                        integer(1))
  out$value_max  <- vapply(keys, function(k) max(d$total[d$series == k]),  numeric(1))
  out$value_mean <- vapply(keys, function(k) mean(d$total[d$series == k]), numeric(1))
  out$value_min  <- vapply(keys, function(k) min(d$total[d$series == k]),  numeric(1))
  for (m in SHARE_MEASURES) {
    out[[paste0("min_", m)]] <- vapply(keys, function(k) {
      s <- d[[m]][d$series == k]; s <- s[!is.na(s)]
      if (length(s)) min(s) else NA_real_
    }, numeric(1))
    out[[paste0("mean_", m)]] <- vapply(keys, function(k) {
      s <- d[[m]][d$series == k]; s <- s[!is.na(s)]
      if (length(s)) mean(s) else NA_real_
    }, numeric(1))
  }
  excl_lin <- unique(c(STRUCTURAL_EXCLUSIONS,
                       vintage_map$lineage[vintage_map$naics6 %in%
                                             STRUCTURAL_EXCLUSIONS]))
  out$structural_exclusion <- out$series %in% excl_lin
  rownames(out) <- NULL
  out
}

eligible_under <- function(sm, reg_name) {
  reg <- ELIGIBILITY_GRID[[reg_name]]
  if (is.null(reg)) stop("unknown eligibility regime: ", reg_name)
  ok <- !sm$structural_exclusion
  if (!is.na(reg$value_stat)) {
    ok <- ok & sm[[paste0("value_", reg$value_stat)]] > reg$value_min
  }
  min_y <- if (identical(reg$min_years, "full")) FULL_SPAN else reg$min_years
  ok & sm$n_years >= min_y
}

sum_unst <- summarise_series(unst)
sum_stit <- summarise_series(stit)

cat("\neligible series by regime (unstitched / stitched):\n")
for (nm in names(ELIGIBILITY_GRID)) {
  cat(sprintf("  %-14s %4d / %4d   %s\n", nm,
              sum(eligible_under(sum_unst, nm)),
              sum(eligible_under(sum_stit, nm)),
              ELIGIBILITY_GRID[[nm]]$label))
}
cat(sprintf("  (%d / %d series excluded structurally in every regime)\n",
            sum(sum_unst$structural_exclusion),
            sum(sum_stit$structural_exclusion)))

curve_for <- function(sm, measure, tau, reg) {
  e <- sm[eligible_under(sm, reg), ]
  c(every = sum(e[[paste0("min_",  measure)]] >= tau, na.rm = TRUE),
    mean  = sum(e[[paste0("mean_", measure)]] >= tau, na.rm = TRUE))
}

print_curve <- function(sm, label, reg) {
  cat("\n", label, "\n", sep = "")
  cat(sprintf("%-6s", "tau"))
  for (m in SHARE_MEASURES) cat(sprintf("%16s", m))
  cat("\n")
  cat(sprintf("%-6s", ""))
  for (m in SHARE_MEASURES) cat(sprintf("%16s", "every / mean"))
  cat("\n")
  for (tau in TAU_GRID) {
    cat(sprintf("%-6.2f", tau))
    for (m in SHARE_MEASURES) {
      cc <- curve_for(sm, m, tau, reg)
      cat(sprintf("%16s", sprintf("%d / %d", cc["every"], cc["mean"])))
    }
    cat("\n")
  }
}

cat("\n---------------------------------------------------------\n")
cat("[Step 8] SECTOR COUNT BY THRESHOLD - all three Step 4 treatments\n")
cat("'every' = share >= tau in every year (protocol rule)\n")
cat("'mean'  = mean annual share >= tau  (relaxed alternative)\n")
cat("---------------------------------------------------------")
for (nm in names(ELIGIBILITY_GRID)) {
  cat(sprintf("\n\n=== ELIGIBILITY: %s - %s ===\n", nm,
              ELIGIBILITY_GRID[[nm]]$label))
  cat(sprintf("    (%d unstitched / %d stitched series eligible)\n",
              sum(eligible_under(sum_unst, nm)),
              sum(eligible_under(sum_stit, nm))))
  print_curve(sum_unst, "UNSTITCHED (one series per NAICS6 code)", nm)
  print_curve(sum_stit, "STITCHED   (vintage lineages aggregated - Step 6 option c)", nm)
}

cat("\nNOTE: s_lower is the protocol's primary measure. s_upper is a BOUND,\n")
cat("not an alternative specification - reading a sector count off s_upper\n")
cat("and a different one off s_lower is what produced the unreproducible\n")
cat("2026-08-21 curve.\n")

pass_list <- function(sm, m, tau, rule = c("every", "mean"), reg) {
  rule <- match.arg(rule)
  e <- sm[eligible_under(sm, reg), ]
  col <- paste0(if (rule == "every") "min_" else "mean_", m)
  e <- e[!is.na(e[[col]]) & e[[col]] >= tau, ]
  e[order(-e[[col]]), c("series", "naics_desc", "n_years", "value_max",
                        paste0("min_", m), paste0("mean_", m))]
}

SM       <- if (STITCH_PRIMARY) sum_stit else sum_unst
SM_LABEL <- if (STITCH_PRIMARY) "STITCHED" else "UNSTITCHED"

cat("\n---------------------------------------------------------\n")
cat(sprintf("PASS LISTS - %s, %s, tau = %.2f, rule = every year\n",
            SM_LABEL, SHARE_PRIMARY, TAU_PRIMARY))
cat("  shown under EVERY eligibility regime - none is preferred here\n")
cat("---------------------------------------------------------\n")
for (nm in names(ELIGIBILITY_GRID)) {
  pl_r <- pass_list(SM, SHARE_PRIMARY, TAU_PRIMARY, "every", nm)
  cat(sprintf("\n  [%s] %s\n", nm, ELIGIBILITY_GRID[[nm]]$label))
  if (nrow(pl_r) == 0) {
    cat("      (no sector qualifies)\n")
  } else {
    for (i in seq_len(nrow(pl_r))) {
      cat(sprintf("      %-8s %-48s n=%2d  min=%.4f  mean=%.4f  peak $%.1fB\n",
                  pl_r$series[i], substr(pl_r$naics_desc[i], 1, 48),
                  pl_r$n_years[i],
                  pl_r[[paste0("min_",  SHARE_PRIMARY)]][i],
                  pl_r[[paste0("mean_", SHARE_PRIMARY)]][i],
                  pl_r$value_max[i] / 1e9))
    }
  }
}

pl <- pass_list(SM, SHARE_PRIMARY, TAU_PRIMARY, "every", ELIG_NARRATIVE)
cat(sprintf("\n  --- the notes below use eligibility = %s ---\n", ELIG_NARRATIVE))

extra <- setdiff(pass_list(SM, SHARE_PRIMARY, TAU_PRIMARY, "mean",
                           ELIG_NARRATIVE)$series, pl$series)
cat(sprintf("\n  relaxing 'every year' to 'mean' at tau=%.2f adds %d sector(s)%s\n",
            TAU_PRIMARY, length(extra),
            if (length(extra)) paste0(": ", paste(extra, collapse = ", ")) else ""))

for (m in setdiff(SHARE_MEASURES, SHARE_PRIMARY)) {
  d <- setdiff(pass_list(SM, m, TAU_PRIMARY, "every", ELIG_NARRATIVE)$series,
               pl$series)
  cat(sprintf("  switching %s -> %-7s adds %d sector(s)%s\n",
              SHARE_PRIMARY, m, length(d),
              if (length(d)) paste0(": ", paste(d, collapse = ", ")) else ""))
}

ELIG_N <- eligible_under(SM, ELIG_NARRATIVE)
elig_means <- SM[[paste0("mean_", SHARE_PRIMARY)]][ELIG_N]
elig_means <- elig_means[!is.na(elig_means)]
qs <- quantile(elig_means, c(.10, .25, .50, .75, .90, .95, .99))
cat(sprintf("\n  distribution of mean %s across %d eligible series (%s):\n",
            SHARE_PRIMARY, length(elig_means), ELIG_NARRATIVE))
cat("     10%    25%    50%    75%    90%    95%    99%    max\n  ")
cat(sprintf("%7.2f", c(qs, max(elig_means))), "\n")

elig_min <- SM[[paste0("min_", SHARE_PRIMARY)]][ELIG_N]
gap <- (elig_means - elig_min[!is.na(SM[[paste0("mean_", SHARE_PRIMARY)]][ELIG_N])])
cat(sprintf("  mean-minus-min gap: median %.2f, 75th %.2f, 90th %.2f\n",
            median(gap, na.rm = TRUE), quantile(gap, .75, na.rm = TRUE),
            quantile(gap, .90, na.rm = TRUE)))

merged_in_pass <- pl$series[grepl("\\+$", pl$series)]
cat(sprintf("\n  of the %d passing series, %d are merged vintage lineages%s\n",
            nrow(pl), length(merged_in_pass),
            if (length(merged_in_pass))
              paste0(" (", paste(merged_in_pass, collapse = ", "), ")") else ""))

curve_rows <- list()
for (reg in names(ELIGIBILITY_GRID)) {
  for (stitched in c(FALSE, TRUE)) {
    sm <- if (stitched) sum_stit else sum_unst
    n_el <- sum(eligible_under(sm, reg))
    for (m in SHARE_MEASURES) {
      for (tau in TAU_GRID) {
        cc <- curve_for(sm, m, tau, reg)
        curve_rows[[length(curve_rows) + 1]] <- data.frame(
          country = COUNTRY, direction = DIRECTION,
          eligibility = reg, eligibility_label = ELIGIBILITY_GRID[[reg]]$label,
          stitched = stitched, share_measure = m, tau = tau,
          n_eligible = n_el,
          n_pass_every_year = as.integer(cc["every"]),
          n_pass_mean       = as.integer(cc["mean"]),
          stringsAsFactors = FALSE)
      }
    }
  }
}
curve_df <- do.call(rbind, curve_rows)
out_curve <- file.path(OUT_DIR, "rp_threshold_curve_NOT_FROZEN.csv")
write.csv(curve_df, out_curve, row.names = FALSE)

out_sect <- file.path(OUT_DIR, "rp_sector_stats_NOT_FROZEN.csv")
SM_out <- SM
for (reg in names(ELIGIBILITY_GRID)) {
  SM_out[[paste0("elig_", reg)]] <- eligible_under(SM, reg)
}
SM_out$stitched <- STITCH_PRIMARY
SM_out$country <- COUNTRY
SM_out$direction <- DIRECTION
write.csv(SM_out, out_sect, row.names = FALSE)

cat(sprintf("\nsaved %s  (%d rows)\n", out_curve, nrow(curve_df)))
cat(sprintf("saved %s  (%d rows)\n", out_sect, nrow(SM_out)))

cat("\n--- tests ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails <<- fails + 1 }
}

check("panel spans 2005-2025",
      identical(range(ALL_YEARS), c(2005L, 2025L)),
      paste("got", paste(range(ALL_YEARS), collapse = "-")))

check("total == related + nonrelated + notreported on every row",
      {
        d <- panel[!is.na(panel$total), ]
        max(abs(d$total - (d$related + d$nonrelated + d$notreported))) == 0
      })

check("all shares within [0,1]",
      all(unlist(lapply(SHARE_MEASURES, function(m)
        c(unst[[m]], stit[[m]]))) %in% c(NA_real_) |
        (unlist(lapply(SHARE_MEASURES, function(m) c(unst[[m]], stit[[m]]))) >= 0 &
         unlist(lapply(SHARE_MEASURES, function(m) c(unst[[m]], stit[[m]]))) <= 1),
        na.rm = TRUE))

check("s_lower <= s_excl and s_lower <= s_upper by construction",
      all(unst$s_lower <= unst$s_excl + 1e-12, na.rm = TRUE) &&
      all(unst$s_lower <= unst$s_upper + 1e-12, na.rm = TRUE))

check("stitching never loses value",
      abs(sum(stit$total) - sum(unst$total)) < 1)

check("every vintage lineage member maps to exactly one lineage",
      !any(duplicated(vintage_map$naics6)))

# [PROTOCOL Step 6.2] the derived map against the OFFICIAL tables -------- The
# two lineages the headline result depends on must be EXACT matches to the
lineage_of <- function(code) {
  m <- vintage_map$lineage[vintage_map$naics6 == code]
  if (length(m)) m[1] else NA_character_
}
members_of <- function(code) {
  l <- lineage_of(code)
  if (is.na(l)) character(0) else sort(vintage_map$naics6[vintage_map$lineage == l])
}
check("autos lineage == official concordance {336110, 336111, 336112}",
      setequal(members_of("336111"), c("336110", "336111", "336112")),
      paste("got", paste(members_of("336111"), collapse = ", ")))

check("petroleum lineage == official {211111, 211112, 211120, 211130}",
      setequal(members_of("211120"),
               c("211111", "211112", "211120", "211130")),
      paste("got", paste(members_of("211120"), collapse = ", ")))

check("tires 326211 is a lineage of one (its code never changed)",
      identical(members_of("326211"), "326211"),
      paste("got", paste(members_of("326211"), collapse = ", ")))

check("no multi-code lineage has an unknown source",
      !any(vintage_map$lineage_source == "unknown"),
      paste(sum(vintage_map$lineage_source == "unknown"), "code(s) affected"))

n_conc <- length(unique(vintage_map$lineage[
  vintage_map$lineage_source == "concordance"]))
n_heur <- length(unique(vintage_map$lineage[
  vintage_map$lineage_source == "heuristic"]))
check("official concordance supplies most multi-code lineages",
      n_conc > n_heur,
      sprintf("concordance %d vs heuristic %d", n_conc, n_heur))

expect_unst <- list("0.95" = c(1, 2), "0.9" = c(2, 3), "0.85" = c(2, 5),
                    "0.8" = c(4, 9), "0.7" = c(6, 20), "0.5" = c(23, 63))

n_size_cov <- sum(sum_unst$value_max > 1e8 & sum_unst$n_years >= 8)
check("[peak100M_n8] series clearing size+coverage == 297 (matches pandas recount)",
      n_size_cov == 297, paste("got", n_size_cov))
check("[peak100M_n8] eligible after structural exclusion == 293",
      sum(eligible_under(sum_unst, "peak100M_n8")) == 293,
      paste("got", sum(eligible_under(sum_unst, "peak100M_n8"))))
rng_recomputed <- aggregate(year ~ country + direction + naics6, data = pos,
                            FUN = function(y) c(min(y), max(y), length(unique(y))))
rng_recomputed <- data.frame(rng_recomputed[, 1:3],
                             y0 = rng_recomputed$year[, 1],
                             y1 = rng_recomputed$year[, 2],
                             n  = rng_recomputed$year[, 3],
                             stringsAsFactors = FALSE)
rkey <- function(d) paste(d$country, d$direction, d$naics6, d$y0, d$y1, d$n)
check("vintage audit file == a recompute from the panel (single source of truth)",
      nrow(rng) == nrow(rng_recomputed) &&
        setequal(rkey(rng), rkey(rng_recomputed)),
      sprintf("audit %d rows, recompute %d rows, %d differ",
              nrow(rng), nrow(rng_recomputed),
              length(setdiff(rkey(rng_recomputed), rkey(rng)))))

check("5 pseudo-code handoffs remain blocked (known, documented gap)",
      nrow(blocked_pairs) == 5, paste("got", nrow(blocked_pairs)))
check("the blocked set is exactly the 31511X and 33631X handoffs",
      setequal(unique(blocked_pairs$pred), c("31511X", "33631X")),
      paste("got", paste(unique(blocked_pairs$pred), collapse = ", ")))
check("33631X and 336310 are therefore still SEPARATE lineages",
      vintage_map$lineage[vintage_map$naics6 == "33631X"] !=
        vintage_map$lineage[vintage_map$naics6 == "336310"])

jn <- pos[pos$country == "CANADA" & pos$direction == "imp" &
            pos$naics6 %in% c("33631X", "336310"), ]
jy <- aggregate(cbind(related, nonrelated, notreported) ~ year, data = jn,
                FUN = sum)
js <- jy$related / (jy$related + jy$nonrelated + jy$notreported)
check("joining 33631X+336310 spans 21 years and still peaks below 0.90",
      nrow(jy) == 21 && max(js) < 0.90 && abs(max(js) - 0.8334) < 0.001,
      sprintf("got %d years, max %.4f", nrow(jy), max(js)))

check("all five non-industry codes are structurally excluded",
      all(vapply(c("990000", "980000", "930000", "920000", "910000"),
                 function(k) isTRUE(sum_unst$structural_exclusion[
                                      sum_unst$series == k]), logical(1))))

for (tau in TAU_GRID) {
  want <- expect_unst[[as.character(tau)]]
  got  <- curve_for(sum_unst, "s_lower", tau, "peak100M_n8")
  check(sprintf("[peak100M_n8] unstitched s_lower tau=%.2f expect %d/%d",
                tau, want[1], want[2]),
        got["every"] == want[1] && got["mean"] == want[2],
        sprintf("got %d/%d", got["every"], got["mean"]))
}

pl_by_reg <- lapply(names(ELIGIBILITY_GRID), function(nm)
  sort(pass_list(sum_unst, "s_lower", TAU_PRIMARY, "every", nm)$series))
names(pl_by_reg) <- names(ELIGIBILITY_GRID)
check("size screen changes nothing at FULL coverage (protocol == peak100M_full)",
      setequal(pl_by_reg$protocol, pl_by_reg$peak100M_full),
      sprintf("protocol={%s} peak100M_full={%s}",
              paste(pl_by_reg$protocol, collapse = ","),
              paste(pl_by_reg$peak100M_full, collapse = ",")))
check("size screen changes nothing at >=8yr coverage (n8_nosize == peak100M_n8)",
      setequal(pl_by_reg$n8_nosize, pl_by_reg$peak100M_n8),
      sprintf("n8_nosize={%s} peak100M_n8={%s}",
              paste(pl_by_reg$n8_nosize, collapse = ","),
              paste(pl_by_reg$peak100M_n8, collapse = ",")))
check("COVERAGE, by contrast, IS substantive (protocol != n8_nosize)",
      !setequal(pl_by_reg$protocol, pl_by_reg$n8_nosize),
      "the coverage floor stopped mattering - re-check before reporting")

check("[protocol] Step 8 as written yields tires ONLY - autos fails on coverage",
      setequal(pl_by_reg$protocol, "326211"),
      sprintf("got {%s}", paste(pl_by_reg$protocol, collapse = ",")))
check("[peak100M_n8] relaxing coverage to >=8yrs restores autos",
      setequal(pl_by_reg$peak100M_n8, c("326211", "336111")),
      sprintf("got {%s}", paste(pl_by_reg$peak100M_n8, collapse = ",")))

pl_stit_proto <- sort(pass_list(sum_stit, "s_lower", TAU_PRIMARY,
                                "every", "protocol")$series)
check("[protocol + stitched] autos SURVIVES Step 8 as written once lineages join",
      setequal(pl_stit_proto, c("326211", "336110+")),
      sprintf("got {%s}", paste(pl_stit_proto, collapse = ",")))

check("[peak100M_n8] unstitched pass list at tau=0.90 is exactly {326211, 336111}",
      setequal(pass_list(sum_unst, "s_lower", 0.90, "every", "peak100M_n8")$series,
               c("326211", "336111")),
      paste("got", paste(pass_list(sum_unst, "s_lower", 0.90, "every",
                                   "peak100M_n8")$series,
                         collapse = ", ")))

check("990000 is structurally excluded",
      isTRUE(sum_unst$structural_exclusion[sum_unst$series == "990000"]))

aut <- sum_stit[grepl("^33611", sum_stit$series), ]
check("autos lineage present in all 21 years after stitching",
      nrow(aut) == 1 && aut$n_years == FULL_SPAN,
      if (nrow(aut) == 1) paste("got n_years =", aut$n_years) else
        paste("matched", nrow(aut), "series"))
check("autos lineage min s_lower >= 0.90 in every year",
      nrow(aut) == 1 && aut$min_s_lower >= 0.90,
      if (nrow(aut) == 1) sprintf("got %.4f", aut$min_s_lower) else "")

cat(sprintf("\n%d test(s) failed.\n", fails))
if (fails > 0) {
  stop("Tests failed - do not rely on the curve above until resolved.")
}

cat("\n=========================================================\n")
cat("NOT FROZEN. Step 8's deliverable is\n")
cat("  sector_list_tau090_FROZEN_YYYYMMDD.csv, with no subsequent edits.\n")
cat("Freezing is a commitment and it is still blocked on the advisor's\n")
cat("answer: threshold vs. coarser NAICS vs. continuous share. The curve\n")
cat("above is an INPUT to that decision, not the decision.\n")
cat("\nWhat the curve says, for that conversation:\n")
cat(sprintf("  - relaxing the 'every year' rule at tau=%.2f gains %d sector(s);\n",
            TAU_PRIMARY, length(extra)))
cat("    LOWERING tau is the only lever that produces a cross-section.\n")
cat("  - the vintage map is built from the OFFICIAL Census NAICS concordances\n")
cat("    (protocol Step 6.2), with a heuristic fallback only for the Census\n")
cat("    trade-specific collapsed codes no concordance covers. The two\n")
cat("    lineages the result depends on are exact matches to the official\n")
cat("    tables and are pinned by tests.\n")
cat("=========================================================\n")
