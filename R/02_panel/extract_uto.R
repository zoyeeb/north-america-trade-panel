# extract_uto.R
# The reshaped USA Trade Online cells into the shared panel schema. Offline.
# This is the 2002-2009 half of the US side; extract_census.R is 2010-2026, and
# the two write the same columns so they stack.
#
# In:   $DATA/uto/uto_long_{can,mex}_{exp,imp}_<year>.csv.gz
# Out:  $DATA/panel/uto_core.csv.gz, uto_extras.csv.gz
# Run:  Rscript R/02_panel/extract_uto.R

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

na_source("R/02_panel/panel_schema.R")

IN_DIR  <- na_data("uto")
OUT_DIR <- na_data("panel")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!exists("DRY_RUN")) DRY_RUN <- FALSE

TOL <- 1e-12

agrees <- function(a, b) abs(a - b) <= TOL * pmax(abs(b), 1)

num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

col_or_na <- function(d, nm) if (nm %in% names(d)) num(d[[nm]]) else rep(NA_real_, nrow(d))

uto_unit <- function(desc) {
  m   <- regexpr("[(][^()]*[)][ ]*$", desc)
  out <- rep(NA_character_, length(desc))
  s   <- trimws(regmatches(desc, m))
  out[m > 0] <- toupper(substr(s, 2L, nchar(s) - 1L))
  out
}

CTY_TAG <- c(can = "CA", mex = "MX")
CTY_WORD <- c(can = "Canada", mex = "Mexico")

files <- list.files(IN_DIR, pattern = "^uto_long_(can|mex)_(exp|imp)_[0-9]{4}[.]csv[.]gz$",
                    full.names = TRUE)
files <- sort(files)
message("cells to read: ", length(files))
if (!length(files)) stop("no uto_long_* files in ", IN_DIR, " - run code/uto/uto_reshape.R first")

core_parts <- list(); extra_parts <- list(); desc_parts <- list()
n_recon <- 0L; n_zero <- 0L; max_gap <- 0; missing_qty <- character(0)

for (f in files) {
  bn   <- basename(f)
  bits <- strsplit(sub("[.]csv[.]gz$", "", bn), "_")[[1]]  # uto,long,can|mex,exp|imp,year
  cty_tag <- bits[3]; dir_tag <- bits[4]
  partner <- unname(CTY_TAG[cty_tag])
  is_exp  <- dir_tag == "exp"

  d <- read.csv(gzfile(f), colClasses = "character", stringsAsFactors = FALSE,
                check.names = FALSE)
  if (!nrow(d)) { message("EMPTY, skipped: ", bn); next }

  seen <- unique(trimws(d$country))
  seen <- seen[!is.na(seen) & nzchar(seen)]
  if (length(seen) != 1 || seen != CTY_WORD[cty_tag])
    stop(bn, ": filename says ", CTY_WORD[cty_tag], " but the country column says ",
         paste(seen, collapse = ", "))

  val_col <- if (is_exp) "value_us_default_member" else "customs_value_gen_us_default_member"
  q1_col  <- if (is_exp) "quantity_1"              else "quantity_1_gen"
  q2_col  <- if (is_exp) "quantity_2"              else "quantity_2_gen"

  if (!val_col %in% names(d))
    stop(bn, ": no '", val_col, "' column - that cell was pulled without the ",
         "value measure and cannot enter the panel")

  d$.v <- num(d[[val_col]])
  d$.basis <- if (is_exp) {
    b <- rep(NA_character_, nrow(d))
    b[d$domestic_foreign == "Domestic Exports"] <- "domestic"
    b[d$domestic_foreign == "Foreign Exports"]  <- "foreign"
    if (any(is.na(b)))
      stop(bn, ": unrecognised DomesticForeign value(s): ",
           paste(unique(d$domestic_foreign[is.na(b)]), collapse = ", "))
    b
  } else {
    rep(NA_character_, nrow(d))
  }

  lvl <- ifelse(is.na(d$hs_code), 0L, nchar(d$hs_code))
  h10 <- d[lvl == 10L, ]
  h06 <- d[lvl ==  6L, ]
  tot <- d[lvl ==  0L & d$hs_desc == "All Commodities", ]

  if (!nrow(h10)) stop(bn, ": no HS10 rows at all")

  gk10 <- paste(h10$period, h10$.basis)
  gkto <- paste(tot$period, tot$.basis)
  a <- tapply(h10$.v, gk10, sum, na.rm = TRUE)
  b <- tapply(tot$.v, gkto, sum, na.rm = TRUE)
  if (!setequal(names(a), names(b)))
    stop(bn, ": the HS10 rows and the All Commodities rows do not cover the ",
         "same period/basis cells - ",
         paste(setdiff(union(names(a), names(b)), intersect(names(a), names(b))),
               collapse = ", "))
  a <- a[names(b)]
  bad <- which(!agrees(a, b))
  if (length(bad))
    stop(bn, ": HS10 does not sum to All Commodities in ", length(bad),
         " cell(s). Worst: ", names(b)[bad[which.max(abs(a[bad] - b[bad]))]],
         " HS10 ", format(a[bad[1]], scientific = FALSE),
         " vs total ", format(b[bad[1]], scientific = FALSE))
  n_recon <- n_recon + length(b)
  max_gap <- max(max_gap, max(abs(a - b) / pmax(abs(b), 1)))

  if (nrow(h06)) {
    p10 <- tapply(h10$.v, paste(h10$period, h10$.basis, substr(h10$hs_code, 1, 6)),
                  sum, na.rm = TRUE)
    p06 <- tapply(h06$.v, paste(h06$period, h06$.basis, h06$hs_code), sum, na.rm = TRUE)
    common <- intersect(names(p10), names(p06))
    orphan <- setdiff(names(p10), names(p06))
    if (length(orphan))
      stop(bn, ": ", length(orphan), " HS10 group(s) roll up to an HS6 that is ",
           "not in the file, e.g. ", orphan[1], " - the code column is mis-parsed")
    x <- p10[common]; y <- p06[common]
    bad6 <- which(!agrees(x, y))
    if (length(bad6))
      stop(bn, ": HS10 does not nest into HS6 in ", length(bad6),
           " group(s). Worst: ", common[bad6[which.max(abs(x[bad6] - y[bad6]))]],
           " HS10 ", format(x[bad6[1]], scientific = FALSE),
           " vs HS6 ", format(y[bad6[1]], scientific = FALSE))
    n_recon <- n_recon + length(common)
    n_zero  <- n_zero + sum(y == 0)   # kept in the count, not filtered out
    max_gap <- max(max_gap, max(abs(x - y) / pmax(abs(y), 1)))
  }

  q1 <- col_or_na(h10, q1_col); q2 <- col_or_na(h10, q2_col)

  u1 <- uto_unit(h10$hs_desc)
  if (any(is.na(u1)))
    stop(bn, ": ", sum(is.na(u1)), " HS10 row(s) whose description carries no ",
         "trailing unit token, e.g. ", h10$hs_desc[which(is.na(u1))[1]],
         " - the label format changed and the unit can no longer be parsed")
  if (!q1_col %in% names(h10) || !q2_col %in% names(h10))
    missing_qty <- c(missing_qty, bn)

  reporter <- rep("US", nrow(h10))
  partner_v <- rep(partner, nrow(h10))
  flow <- rep(if (is_exp) "export" else "import", nrow(h10))

  core <- data.frame(
    period        = h10$period,
    reporter      = reporter,
    partner       = partner_v,
    flow_reporter = flow,
    direction     = to_us_direction(reporter, partner_v, flow),
    basis         = h10$.basis,
    hs_code       = h10$hs_code,
    hs_level      = rep("HS10", nrow(h10)),
    hs6           = to_hs6(h10$hs_code),
    value         = h10$.v,
    currency      = rep("USD", nrow(h10)),
    quantity_1    = q1,
    unit_1        = u1,
    quantity_2    = q2,
    unit_2        = rep(NA_character_, nrow(h10)),  # one token only - see header
    source        = rep(bn, nrow(h10)),
    stringsAsFactors = FALSE
  )
  core_parts[[length(core_parts) + 1]] <- core

  for (k in c("country_sub_code", "rate_provision", "district")) {
    if (!k %in% names(h10)) next
    u <- unique(h10[[k]])
    if (length(u) != 1L)
      stop(bn, ": '", k, "' is not constant (",
           paste(utils::head(u, 4), collapse = ", "),
           ") - it is a real dimension in this cell and must not be dropped")
  }
  drop <- c(val_col, q1_col, q2_col, "hs_code", "hs_desc", "period",
            "domestic_foreign", "country", "district",
            "country_sub_code", "rate_provision", ".v", ".basis")
  keep <- setdiff(names(h10), drop)
  if (length(keep)) {
    ex <- data.frame(source  = bn,
                     period  = h10$period,
                     hs_code = h10$hs_code,
                     basis   = h10$.basis,
                     stringsAsFactors = FALSE)
    for (k in keep) ex[[k]] <- h10[[k]]
    extra_parts[[length(extra_parts) + 1]] <- ex
  }

  desc_parts[[length(desc_parts) + 1]] <-
    unique(data.frame(flow = if (is_exp) "exp" else "imp",
                      hs_code = h10$hs_code, hs_desc = h10$hs_desc,
                      stringsAsFactors = FALSE))

  message(sprintf("  %-30s %8s HS10 rows  basis=%s%s", bn,
                  format(nrow(core), big.mark = ","),
                  paste(sort(unique(ifelse(is.na(core$basis), "NA", core$basis))),
                        collapse = "/"),
                  if (bn %in% missing_qty) "   [NO QUANTITY - see header]" else ""))
}

core <- do.call(rbind, core_parts)

core <- core[order(core$partner, core$flow_reporter, core$period,
                   core$hs_code, core$basis, method = "radix"), ]
rownames(core) <- NULL

check_schema(core, "uto")

if (min(core$period) < "200201" || max(core$period) > "200912")
  stop("UTO periods run ", min(core$period), " to ", max(core$period),
       " - outside the 200201-200912 era this source is supposed to cover")
if (length(unique(core$period)) != 96L)
  stop("expected 96 months (2002-2009), got ", length(unique(core$period)))

if (!DRY_RUN)
  write.csv(core, gzfile(file.path(OUT_DIR, "uto_core.csv.gz")), row.names = FALSE)

for (side in c("exp", "imp")) {
  sel <- Filter(function(e) grepl(paste0("_", side, "_"), e$source[1]), extra_parts)
  if (!length(sel)) next
  ex <- rbind_align(sel)
  ex <- ex[order(ex$source, ex$period, ex$hs_code, method = "radix"), ]
  rownames(ex) <- NULL
  if (!DRY_RUN)
    write.csv(ex, gzfile(file.path(OUT_DIR, paste0("uto_extras_", side, ".csv.gz"))),
              row.names = FALSE)
  message("extras ", side, ": ", format(nrow(ex), big.mark = ","), " rows, ",
          ncol(ex), " cols")
}

desc <- unique(do.call(rbind, desc_parts))
desc <- desc[order(desc$flow, desc$hs_code, method = "radix"), ]
rownames(desc) <- NULL
if (!DRY_RUN)
  write.csv(desc, file.path(OUT_DIR, "uto_code_lookup.csv"), row.names = FALSE)

message("\nUTO CORE SUMMARY")
print(table(core$partner, core$direction, useNA = "ifany"))
message("\nbasis (exports only):")
print(table(core$basis[core$flow_reporter == "export"], useNA = "ifany"))
message("\nreconciliation: ", format(n_recon, big.mark = ","),
        " per-cell comparisons (", format(n_zero, big.mark = ","),
        " of them against an expected value of exactly zero, which are compared",
        " and NOT skipped), worst gap ", format(max_gap, scientific = TRUE))
message("periods: ", min(core$period), " to ", max(core$period),
        " (", length(unique(core$period)), " months)")
message("code lookup: ", format(nrow(desc), big.mark = ","), " code-description pairs")

if (length(missing_qty)) {
  message("\n*** QUANTITY MISSING in ", length(missing_qty), " cell(s) - VALUE IS COMPLETE ***")
  for (m in missing_qty)
    message("      ", m, "  (the download omitted the quantity measures; the ",
            "consumption-basis quantities are in uto_extras_imp.csv.gz)")
}

if (DRY_RUN) {
  message("\nDRY RUN - every check above ran against the real 32 cells and ",
          "NOTHING was written. Re-run without DRY_RUN to write the outputs.")
} else {
  message("\nwrote ", file.path(OUT_DIR, "uto_core.csv.gz"))
}
