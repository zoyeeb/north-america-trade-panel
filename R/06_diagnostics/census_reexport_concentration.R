# census_reexport_concentration.R
# Read-only. Is re-export value spread evenly across products, or concentrated?
#
# In:   $DATA/census_api/
# Out:  $DATA/census_analysis/
# Run:  Rscript R/06_diagnostics/census_reexport_concentration.R
#
# Re-exports contaminate the exporter-pricing question because a re-exported
# good was priced by a foreign producer. The headline figure is 16.6% of
# full-year 2015 US->Canada export value, but 16.6% spread evenly is a very
# different problem from 16.6% sitting on a handful of products.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

IN_DIR  <- na_data("census_api")
OUT_DIR <- na_data("census_analysis")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

files <- list.files(IN_DIR, pattern = "^census_(can|mex)_exp_[0-9]{4}[.]csv$",
                    full.names = TRUE)

parts <- list(); skipped <- character(0)

for (f in files) {
  bn <- basename(f)
  d <- read.csv(f, colClasses = "character", stringsAsFactors = FALSE)

  if (!"DF" %in% names(d)) { skipped <- c(skipped, bn); next }

  bits    <- strsplit(sub("[.]csv$", "", bn), "_")[[1]]
  partner <- if (bits[2] == "can") "CA" else "MX"
  yr      <- as.integer(bits[4])

  val <- suppressWarnings(as.numeric(d$ALL_VAL_MO))
  val[is.na(val)] <- 0

  g <- data.frame(partner = partner, year = yr,
                  code = trimws(d$E_COMMODITY),
                  df   = trimws(d$DF),
                  value = val, stringsAsFactors = FALSE)

  a <- aggregate(value ~ partner + year + code + df, data = g, FUN = sum)
  parts[[length(parts) + 1]] <- a
  cat(sprintf("  %-28s %7d rows  %s\n", bn, nrow(d),
              paste(sort(unique(g$df)), collapse = "/")))
}
if (length(skipped))
  cat("\nskipped (old schema, no DF - these are total exports):\n  ",
      paste(skipped, collapse = "\n  "), "\n")

all <- do.call(rbind, parts)

wide <- reshape(all, idvar = c("partner", "year", "code"),
                timevar = "df", direction = "wide")
names(wide) <- sub("^value[.]", "", names(wide))
for (nm in c("domestic", "foreign"))
  if (!nm %in% names(wide)) wide[[nm]] <- 0
wide$domestic[is.na(wide$domestic)] <- 0
wide$foreign[is.na(wide$foreign)]   <- 0
wide$total <- wide$domestic + wide$foreign

pooled <- aggregate(cbind(domestic, foreign, total) ~ partner + code,
                    data = wide, FUN = sum)
pooled$reexport_share <- ifelse(pooled$total > 0,
                                100 * pooled$foreign / pooled$total, NA_real_)

lk <- read.csv(file.path(IN_DIR, "code_lookup_exp.csv"),
               colClasses = "character", stringsAsFactors = FALSE)
pooled$desc <- lk$E_COMMODITY_LDESC[match(pooled$code, lk$E_COMMODITY)]

write.csv(pooled[order(pooled$partner, -pooled$foreign), ],
          file.path(OUT_DIR, "census_reexport_by_code.csv"), row.names = FALSE)
write.csv(wide[order(wide$partner, wide$year, -wide$foreign), ],
          file.path(OUT_DIR, "census_reexport_by_code_year.csv"), row.names = FALSE)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("RE-EXPORT SHARE OF US EXPORT VALUE, by partner and year\n")
cat(strrep("=", 78), "\n")
yr_tab <- aggregate(cbind(domestic, foreign) ~ partner + year, data = wide, FUN = sum)
yr_tab$share <- 100 * yr_tab$foreign / (yr_tab$domestic + yr_tab$foreign)
for (p in unique(yr_tab$partner)) {
  s <- yr_tab[yr_tab$partner == p, ]
  s <- s[order(s$year), ]
  cat("\n", p, ":\n", sep = "")
  cat(sprintf("  %d %5.1f%%", s$year, s$share), sep = "\n")
}

cat("\n", strrep("=", 78), "\n", sep = "")
cat("CONCENTRATION (pooled 2010-2025)\n")
cat(strrep("=", 78), "\n")

for (p in unique(pooled$partner)) {
  x <- pooled[pooled$partner == p, ]
  x <- x[order(-x$foreign), ]
  tot_f <- sum(x$foreign); tot_v <- sum(x$total)

  cat(sprintf("\n%s  (%d codes, re-export is %.1f%% of all export value)\n",
              p, nrow(x), 100 * tot_f / tot_v))
  for (k in c(1, 5, 10, 25, 50, 100)) {
    if (k > nrow(x)) break
    cat(sprintf("   top %3d codes: %5.1f%% of all re-export value\n",
                k, 100 * sum(x$foreign[1:k]) / tot_f))
  }

  for (thr in c(50, 75, 90)) {
    hit <- x[!is.na(x$reexport_share) & x$reexport_share >= thr, ]
    cat(sprintf("   codes >=%d%% re-export: %5d codes, %5.1f%% of total export value\n",
                thr, nrow(hit), 100 * sum(hit$total) / tot_v))
  }

  cat("   largest re-export flows:\n")
  for (i in seq_len(min(8, nrow(x)))) {
    cat(sprintf("     %-11s %5.1f%% re-exp  %4.1f%% of all re-exp  %s\n",
                x$code[i], x$reexport_share[i],
                100 * x$foreign[i] / tot_f,
                substr(ifelse(is.na(x$desc[i]), "(no description)", x$desc[i]), 1, 44)))
  }
}
cat("\nwrote ", file.path(OUT_DIR, "census_reexport_by_code.csv"), "\n", sep = "")
