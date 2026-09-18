# extract_census.R
# The 68 raw Census API cells into the shared panel schema. Offline.
#
# In:   $DATA/census_api/*.csv
# Out:  $DATA/panel/census_core.csv.gz    the CORE_COLS schema
#       $DATA/panel/census_extras.csv.gz  fields the core schema has no slot for
# Run:  Rscript R/02_panel/extract_census.R
#
# Extras are written separately rather than padded into the core with NAs: the
# three sources have genuinely different extra fields, so a union schema would
# be mostly empty.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

na_source("R/02_panel/panel_schema.R")

IN_DIR  <- na_data("census_api")
OUT_DIR <- na_data("panel")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

blank_dash <- function(x) {
  x <- trimws(as.character(x))
  ifelse(is.na(x) | x == "" | x == "-", NA_character_, x)
}
num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))

files <- list.files(IN_DIR, pattern = "^census_(can|mex)_(exp|imp)_[0-9]{4}[.]csv$",
                    full.names = TRUE)
message("cells to read: ", length(files))

core_parts <- list(); extra_parts <- list()

for (f in files) {
  bn <- basename(f)
  bits <- strsplit(sub("[.]csv$", "", bn), "_")[[1]]   # census, can|mex, exp|imp, year
  cty_tag <- bits[2]; dir_tag <- bits[3]

  partner <- if (cty_tag == "can") "CA" else "MX"
  is_exp  <- dir_tag == "exp"

  d <- read.csv(f, colClasses = "character", stringsAsFactors = FALSE,
                check.names = FALSE)
  if (!nrow(d)) { message("EMPTY, skipped: ", bn); next }

  if ("CTY_CODE" %in% names(d)) {
    seen <- unique(CENSUS_CTY[unique(d$CTY_CODE)])
    seen <- seen[!is.na(seen)]
    if (length(seen) != 1 || seen != partner)
      stop(bn, ": filename says ", partner, " but CTY_CODE says ",
           paste(unique(d$CTY_CODE), collapse = ", "))
  }

  code_col <- if (is_exp) "E_COMMODITY" else "I_COMMODITY"
  val_col  <- if (is_exp) "ALL_VAL_MO"  else "GEN_VAL_MO"
  q1_col   <- if (is_exp) "QTY_1_MO"    else "GEN_QY1_MO"
  q2_col   <- if (is_exp) "QTY_2_MO"    else "GEN_QY2_MO"

  if (is_exp) {
    basis <- if ("DF" %in% names(d)) blank_dash(d$DF) else rep("total", nrow(d))
  } else {
    basis <- rep(NA_character_, nrow(d))
  }

  reporter <- rep("US", nrow(d))
  flow     <- rep(if (is_exp) "export" else "import", nrow(d))

  core <- data.frame(
    period        = paste0(d$YEAR, sprintf("%02d", as.integer(d$MONTH))),
    reporter      = reporter,
    partner       = rep(partner, nrow(d)),
    flow_reporter = flow,
    direction     = to_us_direction(reporter, rep(partner, nrow(d)), flow),
    basis         = basis,
    hs_code       = trimws(d[[code_col]]),
    hs_level      = if ("COMM_LVL" %in% names(d)) trimws(d$COMM_LVL) else "HS10",
    hs6           = to_hs6(d[[code_col]]),
    value         = num(d[[val_col]]),
    currency      = rep("USD", nrow(d)),
    quantity_1    = num(d[[q1_col]]),
    unit_1        = blank_dash(d$UNIT_QY1),
    quantity_2    = num(d[[q2_col]]),
    unit_2        = blank_dash(d$UNIT_QY2),
    source        = rep(bn, nrow(d)),
    stringsAsFactors = FALSE
  )
  core_parts[[length(core_parts) + 1]] <- core

  drop <- c(code_col, val_col, q1_col, q2_col, "UNIT_QY1", "UNIT_QY2",
            "MONTH", "YEAR", "COMM_LVL", "CTY_CODE", "DF")
  keep <- setdiff(names(d), drop)
  if (length(keep)) {
    ex <- data.frame(source  = bn,
                     period  = core$period,
                     hs_code = core$hs_code,
                     basis   = core$basis,
                     stringsAsFactors = FALSE)
    for (k in keep) ex[[k]] <- d[[k]]
    extra_parts[[length(extra_parts) + 1]] <- ex
  }

  message(sprintf("  %-28s %8s rows  basis=%s", bn,
                  format(nrow(core), big.mark = ","),
                  paste(sort(unique(ifelse(is.na(basis), "NA", basis))), collapse = "/")))
}

core <- do.call(rbind, core_parts)

core <- core[order(core$partner, core$flow_reporter, core$period,
                   core$hs_code, core$basis, method = "radix"), ]
rownames(core) <- NULL

check_schema(core, "census")

write.csv(core, gzfile(file.path(OUT_DIR, "census_core.csv.gz")), row.names = FALSE)

for (side in c("exp", "imp")) {
  sel <- Filter(function(e) grepl(paste0("_", side, "_"), e$source[1]), extra_parts)
  if (!length(sel)) next
  ex <- rbind_align(sel, side)
  write.csv(ex, gzfile(file.path(OUT_DIR, paste0("census_extras_", side, ".csv.gz"))),
            row.names = FALSE)
  message("extras ", side, ": ", format(nrow(ex), big.mark = ","), " rows, ",
          ncol(ex), " cols")
}

message("\nCENSUS CORE SUMMARY")
print(table(core$partner, core$direction, useNA = "ifany"))
message("\nbasis (exports only):")
print(table(core$basis[core$flow_reporter == "export"], useNA = "ifany"))
message("\nperiods: ", min(core$period), " to ", max(core$period))
message("wrote ", file.path(OUT_DIR, "census_core.csv.gz"))
