# extract_cimt.R
# The CIMT archives into the shared panel schema. Reads straight from the zips -
# nothing is extracted to disk.
#
# In:   $CIMT_DIR/*.zip
# Out:  $DATA/panel/cimt_core.csv.gz
# Run:  Rscript R/02_panel/extract_cimt.R

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

na_source("R/02_panel/panel_schema.R")
suppressMessages(library(vroom))

OUT_DIR  <- na_data("panel")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!exists("KEEP_GEOGRAPHY")) KEEP_GEOGRAPHY <- FALSE
if (!exists("YEARS_ONLY"))     YEARS_ONLY     <- NULL

SPEC <- list(
  Imp     = list(pref = "ODPFN014", lvl = "HS10", ncol = 8L,
                 flow = "import", basis = NA_character_),
  Tot_Exp = list(pref = "ODPFN017", lvl = "HS8",  ncol = 7L,
                 flow = "export", basis = "total"),
  Dom_Exp = list(pref = "ODPFN016", lvl = "HS8",  ncol = 8L,
                 flow = "export", basis = "domestic")
)
PARTNERS <- c("US", "MX")

zips <- sort(list.files(CIMT_DIR, pattern = "[.]zip$", full.names = TRUE))
parts <- list(); unit_conflicts <- list()

for (z in zips) {
  bn <- basename(z)
  yr <- as.integer(regmatches(bn, regexpr("[0-9]{4}", bn)))
  if (is.na(yr) || yr < 2002) next
  if (!is.null(YEARS_ONLY) && !(yr %in% YEARS_ONLY)) next

  dir_lab <- if (grepl("_Imp_", bn)) "Imp" else
             if (grepl("_Dom_Exp_", bn)) "Dom_Exp" else "Tot_Exp"
  spec <- SPEC[[dir_lab]]

  contents <- tryCatch(utils::unzip(z, list = TRUE), error = function(e) NULL)
  if (is.null(contents)) { message("SKIP (will not open): ", bn); next }
  target <- contents$Name[grepl(spec$pref, basename(contents$Name))][1]
  if (is.na(target)) { message("SKIP (no ", spec$pref, "): ", bn); next }

  d <- tryCatch(vroom(unz(z, target), delim = ",",
                      col_types = cols(.default = "c"),
                      progress = FALSE, altrep = FALSE),
                error = function(e) NULL)
  if (is.null(d)) { message("SKIP (unreadable): ", bn); next }

  if (ncol(d) != spec$ncol)
    stop(bn, ": ", ncol(d), " columns, expected ", spec$ncol,
         " - layout changed, positional selection is no longer safe")

  keep <- d[[3]] %in% PARTNERS
  if (!any(keep)) { rm(d); invisible(gc(verbose = FALSE)); next }

  period <- trimws(d[[1]][keep])
  code   <- trimws(d[[2]][keep])
  cty    <- d[[3]][keep]
  val    <- suppressWarnings(as.numeric(d[[ncol(d) - 2]][keep]))
  qty    <- suppressWarnings(as.numeric(d[[ncol(d) - 1]][keep]))
  uom    <- trimws(d[[ncol(d)]][keep])

  uom <- ifelse(is.na(uom) | uom == "" | toupper(uom) == "N/A", NA_character_, uom)
  val[is.na(val)] <- 0

  g <- data.frame(period = period, hs_code = code, partner = cty,
                  value = val, quantity = qty, unit = uom,
                  stringsAsFactors = FALSE)

  if (KEEP_GEOGRAPHY) {
    g$province <- if (spec$ncol == 8L) d[[4]][keep] else NA_character_
    g$state    <- d[[ncol(d) - 3]][keep]
    agg <- g
  } else {
    u <- unique(g[!is.na(g$unit), c("period", "hs_code", "partner", "unit")])
    dup <- u[duplicated(u[, c("period", "hs_code", "partner")]) |
             duplicated(u[, c("period", "hs_code", "partner")], fromLast = TRUE), ]
    if (nrow(dup)) {
      dup$source <- bn
      unit_conflicts[[length(unit_conflicts) + 1]] <- dup
    }

    agg <- aggregate(cbind(value, quantity) ~ period + hs_code + partner,
                     data = g, FUN = function(x) sum(x, na.rm = TRUE),
                     na.action = na.pass)
    ukey <- g[!is.na(g$unit), ]
    ukey <- ukey[!duplicated(ukey[, c("period", "hs_code", "partner")]), ]
    agg$unit <- ukey$unit[match(paste(agg$period, agg$hs_code, agg$partner),
                                paste(ukey$period, ukey$hs_code, ukey$partner))]
  }

  n <- nrow(agg)
  reporter <- rep("CA", n)
  flow     <- rep(spec$flow, n)

  core <- data.frame(
    period        = agg$period,
    reporter      = reporter,
    partner       = agg$partner,
    flow_reporter = flow,
    direction     = to_us_direction(reporter, agg$partner, flow),
    basis         = rep(spec$basis, n),
    hs_code       = agg$hs_code,
    hs_level      = rep(spec$lvl, n),
    hs6           = to_hs6(agg$hs_code),
    value         = agg$value,
    currency      = rep("CAD", n),
    quantity_1    = agg$quantity,
    unit_1        = agg$unit,
    quantity_2    = rep(NA_real_, n),
    unit_2        = rep(NA_character_, n),
    source        = rep(bn, n),
    stringsAsFactors = FALSE
  )
  parts[[length(parts) + 1]] <- core

  message(sprintf("  %-30s %-8s %9s rows -> %8s  %s", bn, dir_lab,
                  format(sum(keep), big.mark = ","),
                  format(n, big.mark = ","),
                  ifelse(is.na(spec$basis), "imports", spec$basis)))
  rm(d, g, agg, period, code, cty, val, qty, uom)
  invisible(gc(verbose = FALSE))
}

core <- do.call(rbind, parts)

core <- core[order(core$partner, core$flow_reporter, core$basis,
                   core$period, core$hs_code, method = "radix"), ]
rownames(core) <- NULL

if (length(unit_conflicts)) {
  uc <- do.call(rbind, unit_conflicts)
  write.csv(uc, file.path(OUT_DIR, "cimt_unit_conflicts.csv"), row.names = FALSE)
  message("\n*** UNIT CONFLICTS: ", format(nrow(uc), big.mark = ","),
          " (period, code, partner) groups carry MORE THAN ONE unit.")
  message("*** Summing quantity across province/state is NOT valid for those.")
  message("*** Written to data/panel/cimt_unit_conflicts.csv - review before use.")
} else {
  message("\nunit assertion held: no (period, code, partner) group carries ",
          "more than one unit, so the geographic aggregation is well defined.")
}

check_schema(core, "cimt")
write.csv(core, gzfile(file.path(OUT_DIR, "cimt_core.csv.gz")), row.names = FALSE)

message("\nCIMT CORE SUMMARY")
print(table(core$partner, core$direction, useNA = "ifany"))
message("\nbasis:")
print(table(core$basis, core$flow_reporter, useNA = "ifany"))
message("\nperiods: ", min(core$period), " to ", max(core$period))
message("wrote ", file.path(OUT_DIR, "cimt_core.csv.gz"))
