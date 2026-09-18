# cimt_quantity_sweep.R
# Measures how completely quantity and unit of measure are populated across
# every CIMT archive on disk. Offline.
#
# In:   $CIMT_DIR/*.zip
# Out:  findings on stdout
# Run:  Rscript R/06_diagnostics/cimt_quantity_sweep.R
#
# A unit value is value / quantity, so a row with no quantity is unusable for
# the price exercise no matter how good its value figure is.

suppressMessages(library(vroom))

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

OUT_DIR  <- na_data("cimt")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

NATIVE <- list(
  Imp     = list(pref = "ODPFN014", lvl = "HS10", ncol = 8L),
  Tot_Exp = list(pref = "ODPFN017", lvl = "HS8",  ncol = 7L),
  Dom_Exp = list(pref = "ODPFN016", lvl = "HS8",  ncol = 8L)
)

PARTNERS <- c("US", "MX")

if (!exists("ONLY")) ONLY <- NULL

zips <- sort(list.files(CIMT_DIR, pattern = "[.]zip$", full.names = TRUE))
if (!is.null(ONLY)) zips <- zips[basename(zips) %in% ONLY]

rows <- list(); unit_tally <- list()

for (z in zips) {
  bn <- basename(z)

  yr <- as.integer(regmatches(bn, regexpr("[0-9]{4}", bn)))
  if (is.na(yr) || yr < 2002) next

  dir_lab <- if (grepl("_Imp_", bn)) "Imp" else
             if (grepl("_Dom_Exp_", bn)) "Dom_Exp" else "Tot_Exp"
  spec <- NATIVE[[dir_lab]]

  contents <- tryCatch(utils::unzip(z, list = TRUE), error = function(e) NULL)
  if (is.null(contents)) { message("SKIP (will not open): ", bn); next }
  target <- contents$Name[grepl(spec$pref, basename(contents$Name))][1]
  if (is.na(target)) { message("SKIP (no ", spec$pref, "): ", bn); next }

  t0 <- Sys.time()

  d <- tryCatch(
    vroom(unz(z, target), delim = ",", col_types = cols(.default = "c"),
          progress = FALSE, altrep = FALSE),
    error = function(e) NULL)
  if (is.null(d)) { message("SKIP (unreadable): ", bn); next }

  if (ncol(d) != spec$ncol) {
    message(sprintf("SKIP %s: %d columns, expected %d", bn, ncol(d), spec$ncol))
    next
  }

  n   <- nrow(d)
  cty <- d[[3]]
  val <- suppressWarnings(as.numeric(d[[ncol(d) - 2]]))
  qty <- suppressWarnings(as.numeric(d[[ncol(d) - 1]]))
  uom <- d[[ncol(d)]]

  uom_norm <- trimws(ifelse(is.na(uom), "", uom))
  uom_miss <- uom_norm == "" | toupper(uom_norm) == "N/A"

  qty_usable  <- !is.na(qty) & qty >  0
  qty_zero    <- !is.na(qty) & qty == 0
  qty_missing <- is.na(qty)

  is_partner <- cty %in% PARTNERS

  rows[[length(rows) + 1]] <- data.frame(
    archive        = bn,
    direction      = dir_lab,
    year           = yr,
    level          = spec$lvl,
    rows_total     = n,
    value_total    = sum(val, na.rm = TRUE),
    value_usable   = sum(val[qty_usable], na.rm = TRUE),
    val_usable_pct = round(100 * sum(val[qty_usable], na.rm = TRUE) /
                           sum(val, na.rm = TRUE), 2),
    partner_value_total  = sum(val[is_partner], na.rm = TRUE),
    partner_value_usable = sum(val[qty_usable & is_partner], na.rm = TRUE),
    partner_val_usable = if (any(is_partner))
                           round(100 * sum(val[qty_usable & is_partner], na.rm = TRUE) /
                                 sum(val[is_partner], na.rm = TRUE), 2) else NA_real_,
    qty_usable_pct = round(100 * sum(qty_usable)  / n, 2),
    qty_zero_pct   = round(100 * sum(qty_zero)    / n, 2),
    qty_miss_pct   = round(100 * sum(qty_missing) / n, 2),
    uom_miss_pct   = round(100 * sum(uom_miss)    / n, 2),
    n_units        = length(unique(uom_norm[!uom_miss])),
    partner_rows       = sum(is_partner),
    partner_qty_usable = if (any(is_partner))
                           round(100 * sum(qty_usable & is_partner) /
                                 sum(is_partner), 2) else NA_real_,
    partner_uom_miss   = if (any(is_partner))
                           round(100 * sum(uom_miss & is_partner) /
                                 sum(is_partner), 2) else NA_real_,
    secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
    stringsAsFactors = FALSE
  )

  tb <- table(uom_norm[!uom_miss])
  if (length(tb)) {
    unit_tally[[length(unit_tally) + 1]] <- data.frame(
      archive = bn, direction = dir_lab, year = yr,
      unit = names(tb), n = as.integer(tb), stringsAsFactors = FALSE)
  }

  cat(sprintf("%-30s %-5s %9d rows  qty_ok %5.1f%%  VAL_ok %5.1f%%  uom_miss %5.1f%%  (%.0fs)\n",
              bn, spec$lvl, n,
              rows[[length(rows)]]$qty_usable_pct,
              rows[[length(rows)]]$val_usable_pct,
              rows[[length(rows)]]$uom_miss_pct,
              rows[[length(rows)]]$secs))

  rm(d, cty, val, qty, uom, uom_norm, uom_miss)
  invisible(gc(verbose = FALSE))
}

summary_df <- do.call(rbind, rows)
write.csv(summary_df, file.path(OUT_DIR, "cimt_quantity_summary.csv"),
          row.names = FALSE)
if (length(unit_tally)) {
  write.csv(do.call(rbind, unit_tally),
            file.path(OUT_DIR, "cimt_unit_tally.csv"), row.names = FALSE)
}

cat("\n", strrep("=", 78), "\n", sep = "")
cat("SUMMARY BY DIRECTION (row-weighted across years)\n")
cat(strrep("=", 78), "\n")
for (dl in unique(summary_df$direction)) {
  s <- summary_df[summary_df$direction == dl, ]
  cat(sprintf("%-9s %2d archives %11d rows   rows_qty %5.1f%%   VALUE_qty %5.1f%%   uom_miss %5.1f%%   US+MX rows %5.1f%%   US+MX VALUE %5.1f%%\n",
              dl, nrow(s), sum(s$rows_total),
              sum(s$rows_total * s$qty_usable_pct) / sum(s$rows_total),
              100 * sum(s$value_usable) / sum(s$value_total),
              sum(s$rows_total * s$uom_miss_pct)   / sum(s$rows_total),
              sum(s$partner_rows * s$partner_qty_usable, na.rm = TRUE) /
                sum(s$partner_rows),
              100 * sum(s$partner_value_usable) / sum(s$partner_value_total)))
}
cat("\nwrote ", file.path(OUT_DIR, "cimt_quantity_summary.csv"), "\n", sep = "")
