# uto_merge.R
# Merges the raw USA Trade Online measure sets for one cell into a single long
# CSV using the same column vocabulary as the Census API files.
#
# In:   $DATA/uto/ raw downloads
# Out:  $DATA/uto/ merged long CSVs
# Run:  Rscript R/01_acquire/uto_merge.R
#
# UTO exports a cross-tab, not a table: ~10 rows by ~536,000 columns, six of
# which describe each column's dimensions and the rest hold one measure each.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

if (!exists("COUNTRY"))   COUNTRY   <- "can"
if (!exists("DIRECTION")) DIRECTION <- "imp"
if (!exists("YEAR"))      YEAR      <- 2004

if (!exists("KEEP_LEVELS")) KEEP_LEVELS <- "HS10"

UTO_DIR <- na_data("uto")

CTY_CODES <- c(can = "1220", mex = "2010")

stopifnot(COUNTRY %in% names(CTY_CODES), DIRECTION %in% c("exp", "imp"))

stem     <- sprintf("uto_%s_%s_%d", COUNTRY, DIRECTION, YEAR)
out_path <- file.path(UTO_DIR, paste0(stem, ".csv"))

set_paths <- sort(Sys.glob(file.path(UTO_DIR, paste0(stem, "_*.csv"))))

if (!length(set_paths)) {
  stop("no raw files found matching ", file.path(UTO_DIR, paste0(stem, "_*.csv")))
}

message("merging ", length(set_paths), " measure set(s): ",
        paste(basename(set_paths), collapse = " + "))

read_crosstab <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) < 3) stop("file has too few rows to be a UTO cross-tab: ", path)

  split_row <- function(l) {
    scan(text = l, what = "", sep = ",", quote = "\"",
         quiet = TRUE, blank.lines.skip = FALSE)
  }

  banner <- lines[1]
  rows   <- lapply(lines[-1], split_row)
  labels <- vapply(rows, function(r) if (length(r)) r[1] else "", character(1))
  names(rows) <- labels

  list(banner = banner, rows = rows, labels = labels)
}

SETS <- lapply(set_paths, read_crosstab)
names(SETS) <- basename(set_paths)

DIM_NAMES <- c("Commodity", "DomesticForeign", "Country", "CountrySubCode",
               "RateProvision", "District", "Time")

dims_of <- function(X) X$rows[intersect(DIM_NAMES, X$labels)]

measures_of <- function(X) {
  keep <- !(X$labels %in% DIM_NAMES) & X$labels != "Measures" & nzchar(X$labels)
  X$rows[keep]
}

dims_all <- lapply(SETS, dims_of)
meas_all <- lapply(SETS, measures_of)

for (nm in names(SETS)) {
  message("  ", nm, ": ", paste(names(meas_all[[nm]]), collapse = " | "))
}

dims_a <- dims_all[[1]]

ref <- names(SETS)[1]
for (nm in names(SETS)[-1]) {
  d <- dims_all[[nm]]
  if (!identical(names(d), names(dims_a))) {
    stop(nm, " has different dimensions from ", ref, ": ",
         paste(names(d), collapse = ","), " vs ",
         paste(names(dims_a), collapse = ","))
  }
  for (dn in names(dims_a)) {
    if (!identical(dims_a[[dn]], d[[dn]])) {
      stop("dimension '", dn, "' differs between ", ref, " and ", nm,
           " - these pulls are not the same grid and cannot be merged positionally")
    }
  }
}
message("  guard passed: all ", length(SETS),
        " file(s) describe an identical column grid")

commodity <- dims_a[["Commodity"]][-1]
time_lab  <- dims_a[["Time"]][-1]
n_col     <- length(commodity)

if ("DomesticForeign" %in% names(dims_a)) {
  df_lab <- dims_a[["DomesticForeign"]][-1]
} else {
  df_lab <- rep(NA_character_, n_col)
}

code  <- sub(" .*$", "", commodity)
is_hs <- grepl("^[0-9]+$", code)
code[!is_hs] <- "ALL"

desc  <- sub("^[^ ]+ ", "", commodity)
desc[!is_hs] <- commodity[!is_hs]

comm_lvl <- ifelse(!is_hs, "TOTAL", paste0("HS", nchar(code)))

unit <- rep(NA_character_, n_col)
has_unit <- grepl("\\([^()]*\\)[[:space:]]*$", commodity)
unit[has_unit] <- sub("^.*\\(([^()]*)\\)[[:space:]]*$", "\\1", commodity[has_unit])
unit[has_unit] <- toupper(unit[has_unit])
desc[has_unit] <- trimws(sub("\\([^()]*\\)[[:space:]]*$", "", desc[has_unit]))

month_names <- c("January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December")
first_word  <- sub(" .*$", "", time_lab)
mi          <- match(first_word, month_names)
is_annual   <- is.na(mi)
month_num   <- ifelse(is_annual, NA_character_, sprintf("%02d", mi))
year_num    <- ifelse(is_annual, time_lab, sub("^[^ ]+ ", "", time_lab))

api_name <- function(label, direction) {
  cons <- grepl("(Cons)", label, fixed = TRUE)   # imports-for-consumption basis
  if (direction == "exp") {
    if (grepl("Unit Value", label, fixed = TRUE)) return("UNIT_VAL_MO")
    if (grepl("Card Count", label, fixed = TRUE)) return("CARD_COUNT")
    if (grepl("Quantity 1", label, fixed = TRUE)) return("QTY_1_MO")
    if (grepl("Quantity 2", label, fixed = TRUE)) return("QTY_2_MO")
    if (grepl("Value",      label, fixed = TRUE)) return("ALL_VAL_MO")
    stop("unrecognised UTO export measure: ", label)
  }
  if (grepl("Card Count",     label, fixed = TRUE)) return("CARD_COUNT")
  if (grepl("Unit Value",     label, fixed = TRUE)) return("UNIT_VAL_MO")
  if (grepl("Dutiable Value", label, fixed = TRUE)) return("DUT_VAL_MO")
  if (grepl("Calculated Duty",label, fixed = TRUE)) return("CAL_DUT_MO")
  if (grepl("CIF Value",      label, fixed = TRUE)) return(if (cons) "CON_CIF_MO" else "GEN_CIF_MO")
  if (grepl("Quantity 1",     label, fixed = TRUE)) return(if (cons) "CON_QY1_MO" else "GEN_QY1_MO")
  if (grepl("Quantity 2",     label, fixed = TRUE)) return(if (cons) "CON_QY2_MO" else "GEN_QY2_MO")
  if (grepl("Value",          label, fixed = TRUE)) return(if (cons) "CON_VAL_MO" else "GEN_VAL_MO")
  stop("unrecognised UTO import measure: ", label)
}

measures <- unlist(meas_all, recursive = FALSE, use.names = FALSE)
names(measures) <- unlist(lapply(meas_all, names), use.names = FALSE)
if (anyDuplicated(names(measures))) {
  stop("the same measure appears in more than one set: ",
       paste(unique(names(measures)[duplicated(names(measures))]), collapse = ", "))
}
meas_cols <- list()
for (nm in names(measures)) {
  meas_cols[[api_name(nm, DIRECTION)]] <- measures[[nm]][-1]
}
message("  measure columns: ", paste(names(meas_cols), collapse = ", "))

val_col <- Find(function(x) x %in% names(meas_cols),
                c("CON_VAL_MO", "GEN_VAL_MO", "ALL_VAL_MO"))
if (is.null(val_col)) stop("no value measure found in either file - cannot check totals")
message("  checks run on: ", val_col)
vals    <- suppressWarnings(as.numeric(meas_cols[[val_col]]))
vals[is.na(vals)] <- 0

key <- paste(commodity, df_lab, sep = "\r")
ann <- tapply(vals[is_annual],  key[is_annual],  sum)
mon <- tapply(vals[!is_annual], key[!is_annual], sum)
common <- intersect(names(ann), names(mon))
diffs  <- abs(ann[common] - mon[common])
worst  <- if (length(diffs)) max(diffs) else 0

message(sprintf("  annual control: %d series checked, largest month-sum vs annual gap = %.2f",
                length(common), worst))
if (worst > 1) {
  warning("monthly values do not sum to the annual control for at least one ",
          "series (largest gap ", format(worst, big.mark = ","), "). ",
          "The download may be incomplete - investigate before using this file.")
} else {
  message("  annual control PASSED - every series' 12 months sum to its annual figure")
}

grp      <- paste(month_num, df_lab, sep = "\r")
hs10_sum <- tapply(vals[!is_annual & comm_lvl == "HS10"],
                   grp[!is_annual & comm_lvl == "HS10"], sum)
tot_sum  <- tapply(vals[!is_annual & comm_lvl == "TOTAL"],
                   grp[!is_annual & comm_lvl == "TOTAL"], sum)

both <- intersect(names(hs10_sum), names(tot_sum))
if (!length(both)) {
  warning("no 'All Commodities' column found - the HS10 completeness check ",
          "could not run, so this file's detail is unverified")
} else {
  gap     <- hs10_sum[both] - tot_sum[both]
  worst_g <- max(abs(gap))
  message(sprintf("  completeness: %d (month, basis) cells checked, largest HS10-vs-total gap = %.2f",
                  length(both), worst_g))
  if (worst_g > 1) {
    warning("the HS10 lines do NOT sum to the reported total in at least one ",
            "cell (largest gap ", format(worst_g, big.mark = ","), "). ",
            "The tariff-line detail is incomplete - do not filter to HS10 until ",
            "this is understood, because the aggregates are the only record of ",
            "what is missing.")
  } else {
    message("  completeness PASSED - HS10 accounts for 100% of reported value, ",
            "so dropping the aggregate levels loses nothing")
  }
}

populated <- rep(FALSE, n_col)
for (v in meas_cols) populated <- populated | nzchar(v)

keep <- !is_annual & populated & comm_lvl %in% KEEP_LEVELS

message(sprintf("  columns: %d total -> %d monthly and populated -> %d at level(s) %s",
                n_col, sum(!is_annual & populated), sum(keep),
                paste(KEEP_LEVELS, collapse = "/")))
if (!sum(keep)) stop("no columns survived the level filter - check KEEP_LEVELS")

out <- data.frame(
  COMMODITY = code[keep],
  stringsAsFactors = FALSE
)

names(out)[1] <- if (DIRECTION == "imp") "I_COMMODITY" else "E_COMMODITY"

for (nm in names(meas_cols)) out[[nm]] <- meas_cols[[nm]][keep]

derive_cha <- function(cif, val) {
  ok <- nzchar(cif) & nzchar(val)
  out <- rep("", length(cif))
  out[ok] <- format(suppressWarnings(as.numeric(cif[ok])) -
                    suppressWarnings(as.numeric(val[ok])),
                    scientific = FALSE, trim = TRUE)
  out
}
for (b in c("GEN", "CON")) {
  cifn <- paste0(b, "_CIF_MO"); valn <- paste0(b, "_VAL_MO")
  if (all(c(cifn, valn) %in% names(out))) {
    out[[paste0(b, "_CHA_MO")]] <- derive_cha(out[[cifn]], out[[valn]])
    message("  derived ", b, "_CHA_MO = ", cifn, " - ", valn)
  }
}

out$UNIT_QY1 <- unit[keep]

if (!all(is.na(df_lab))) {
  out$DF <- ifelse(grepl("^Domestic", df_lab[keep]), "domestic",
            ifelse(grepl("^Foreign",  df_lab[keep]), "foreign", NA_character_))
}

out$MONTH    <- month_num[keep]
out$YEAR     <- year_num[keep]
out$COMM_LVL <- comm_lvl[keep]
out$CTY_CODE <- CTY_CODES[[COUNTRY]]

out$SOURCE <- paste(basename(set_paths), collapse = "+")

for (nm in names(out)) out[[nm]][is.na(out[[nm]])] <- ""

write.csv(out, out_path, row.names = FALSE, quote = TRUE, na = "")

message("wrote ", out_path, " - ", format(nrow(out), big.mark = ","), " rows, ",
        ncol(out), " columns")

print(table(out$COMM_LVL, useNA = "ifany"))
