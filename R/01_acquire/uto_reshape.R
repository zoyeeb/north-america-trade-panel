# uto_reshape.R
# USA Trade Online exports (US 2002-2009) from their wide, transposed shape
# into the long format the rest of the pipeline uses.
#
# In:   $UTO_IN_DIR/*.csv  (manual USA Trade Online downloads - see README)
# Out:  $DATA/uto/uto_long_{can,mex}_{exp,imp}_<year>.csv.gz
# Run:  Rscript R/01_acquire/uto_reshape.R
#
# A UTO export is about 9 rows by 600,000 COLUMNS: each column is one
# observation and the rows are the fields, so a single data point reads
# vertically. Nothing downstream can read that shape.

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

if (!exists("UTO_OUT_DIR")) UTO_OUT_DIR <- na_data("uto")

dir.create(UTO_OUT_DIR, recursive = TRUE, showWarnings = FALSE)

GZIP_OUTPUT <- TRUE

DROP_BLANK <- TRUE

read_uto_wide <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) stop("Empty file: ", path)

  parse_line <- function(ln) {
    scan(text = ln, what = "", sep = ",", quote = "\"",
         quiet = TRUE, strip.white = FALSE)
  }

  out <- list()
  for (ln in lines) {
    if (!nzchar(ln)) next
    f <- parse_line(ln)
    if (!length(f)) next
    key <- trimws(f[1])
    if (!nzchar(key)) next
    if (grepl("^Source:", key)) { out[["__source__"]] <- key; next }
    out[[key]] <- if (length(f) > 1) f[-1] else character(0)
  }
  out
}

split_rows <- function(w) {
  nm <- setdiff(names(w), "__source__")
  i <- match("Measures", nm)
  if (is.na(i)) {
    stop("No 'Measures' separator row found - the export layout has changed.")
  }
  list(dims = nm[seq_len(i - 1L)], measures = nm[-seq_len(i)])
}

split_code <- function(x) {
  has_code <- grepl("^[0-9]+[[:space:]]", x)
  code <- rep(NA_character_, length(x))
  desc <- x
  code[has_code] <- sub("^([0-9]+)[[:space:]].*$", "\\1", x[has_code])
  desc[has_code] <- sub("^[0-9]+[[:space:]]+", "", x[has_code])
  list(code = code, desc = desc)
}

MONTHS <- c(January = "01", February = "02", March = "03", April = "04",
            May = "05", June = "06", July = "07", August = "08",
            September = "09", October = "10", November = "11", December = "12")

parse_time <- function(x) {
  is_annual <- grepl("^[0-9]{4}$", x)
  period <- rep(NA_character_, length(x))
  mth <- sub("^([A-Za-z]+)[[:space:]]+[0-9]{4}$", "\\1", x)
  yr  <- sub("^[A-Za-z]+[[:space:]]+([0-9]{4})$", "\\1", x)
  ok  <- !is_annual & mth %in% names(MONTHS)
  period[ok] <- paste0(yr[ok], MONTHS[mth[ok]])
  list(period = period, is_annual = is_annual,
       year = ifelse(is_annual, x, yr))
}

files <- list.files(UTO_IN_DIR, pattern = "^uto_[a-z]+_[a-z]+_[0-9]{4}_spec[ABC]\\.csv$",
                    full.names = FALSE)
if (!length(files)) {
  stop("No UTO files found in ", UTO_IN_DIR, "\n",
       "  Expected names like uto_can_exp_2002_specA.csv")
}

meta <- data.frame(
  file    = files,
  country = sub("^uto_([a-z]+)_.*$", "\\1", files),
  dirn    = sub("^uto_[a-z]+_([a-z]+)_.*$", "\\1", files),
  year    = sub("^uto_[a-z]+_[a-z]+_([0-9]{4})_.*$", "\\1", files),
  spec    = sub("^.*_spec([ABC])\\.csv$", "\\1", files),
  stringsAsFactors = FALSE)
meta$cell <- paste(meta$country, meta$dirn, meta$year, sep = "_")

cat("=========================================================\n")
cat("uto_reshape.R - wide/transposed UTO exports -> long format\n")
cat("=========================================================\n")
cat(sprintf("input : %s\n", normalizePath(UTO_IN_DIR, mustWork = FALSE)))
cat(sprintf("output: %s\n", normalizePath(UTO_OUT_DIR, mustWork = FALSE)))
cat(sprintf("found : %d file(s) across %d cell(s)\n\n",
            nrow(meta), length(unique(meta$cell))))

reshape_cell <- function(cell) {
  m <- meta[meta$cell == cell, ]
  out_path <- file.path(UTO_OUT_DIR,
                        paste0("uto_long_", cell, ".csv",
                               if (GZIP_OUTPUT) ".gz" else ""))
  if (file.exists(out_path)) {
    cat(sprintf("  %-18s skip (already on disk)\n", cell)); return(invisible(NULL))
  }

  wides <- list()
  for (i in seq_len(nrow(m))) {
    wides[[m$spec[i]]] <- read_uto_wide(file.path(UTO_IN_DIR, m$file[i]))
  }
  if (!"A" %in% names(wides)) {
    cat(sprintf("  %-18s SKIP - no specA (Value) file\n", cell)); return(invisible(NULL))
  }
  base <- wides[["A"]]

  sr_a  <- split_rows(base)
  keyof <- function(w, dims) do.call(paste, c(lapply(dims, function(k) w[[k]]),
                                              list(sep = "\r")))
  key_a <- keyof(base, sr_a$dims)
  if (anyDuplicated(key_a)) {
    stop(cell, ": specA key is not unique over (",
         paste(sr_a$dims, collapse = ", "), ") - no join is well defined.")
  }

  cm  <- split_code(base$Commodity)
  tm  <- parse_time(base$Time)

  d <- data.frame(
    hs_code   = cm$code,
    hs_desc   = cm$desc,
    period    = tm$period,
    is_annual = tm$is_annual,
    stringsAsFactors = FALSE)

  tidy_name <- function(x) {
    nm <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", x))
    nm <- tolower(gsub("[^A-Za-z0-9]+", "_", nm))
    sub("_+$", "", sub("_default_member$", "", nm))
  }
  for (dm in setdiff(sr_a$dims, c("Commodity", "Time"))) {
    d[[tidy_name(dm)]] <- base[[dm]]
  }

  num <- function(x) suppressWarnings(as.numeric(x))

  for (mr in sr_a$measures) d[[tidy_name(mr)]] <- num(base[[mr]])

  for (spec in setdiff(names(wides), "A")) {
    w  <- wides[[spec]]
    sr_w <- split_rows(w)
    if (!identical(sr_w$dims, sr_a$dims)) {
      stop(cell, ": spec", spec, " has different dimensions (",
           paste(sr_w$dims, collapse = ", "), ") than specA (",
           paste(sr_a$dims, collapse = ", "), ").")
    }
    kb <- keyof(w, sr_w$dims)
    if (anyDuplicated(kb)) {
      stop(cell, ": spec", spec, " key is not unique - cannot align.")
    }
    idx <- match(key_a, kb)
    n_missing <- sum(is.na(idx))
    n_extra   <- sum(!(kb %in% key_a))
    if (n_missing || n_extra) {
      cat(sprintf("       spec%s: %d of %d rows absent (all-blank columns UTO omitted)%s\n",
                  spec, n_missing, length(key_a),
                  if (n_extra) sprintf(", and %d row(s) present only in spec%s",
                                       n_extra, spec) else ""))
    }
    if (n_extra > 0) {
      stop(cell, ": spec", spec, " has ", n_extra, " observation(s) missing from ",
           "specA. specA defines the skeleton, so those would be lost silently. ",
           "Re-export specA with the same commodity selection.")
    }
    for (mr in sr_w$measures) d[[tidy_name(mr)]] <- num(w[[mr]])[idx]
  }
  val_col <- grep("^customs_value_cons", names(d), value = TRUE)[1]
  if (is.na(val_col)) val_col <- grep("^value", names(d), value = TRUE)[1]
  if (is.na(val_col)) {
    cat(sprintf("  %-18s SKIP - no consumption value measure. Has: %s\n",
                cell, paste(grep("^(customs_value|value|cif|dutiable)",
                                 names(d), value = TRUE), collapse = ", ")))
    return(invisible(NULL))
  }

  ann <- d[d$is_annual & !is.na(d[[val_col]]), ]
  mon <- d[!d$is_annual & !is.na(d[[val_col]]), ]
  grp_cols <- c("hs_code", "hs_desc",
                vapply(setdiff(sr_a$dims, c("Commodity", "Time")),
                       tidy_name, character(1)))
  grp_cols <- grp_cols[grp_cols %in% names(d)]
  gkey <- function(x) do.call(paste, c(lapply(grp_cols, function(k) x[[k]]), list(sep = "|")))
  key_a <- gkey(ann)
  key_m <- gkey(mon)
  sum_m <- tapply(mon[[val_col]], key_m, sum)
  sum_a <- tapply(ann[[val_col]], key_a, sum)
  common <- intersect(names(sum_a), names(sum_m))
  bad <- common[abs(sum_a[common] - sum_m[common]) >
                  pmax(1, 1e-9 * abs(sum_a[common]))]
  if (length(bad)) {
    stop(sprintf("%s: annual != sum(monthly) for %d commodity-DF group(s), e.g. %s",
                 cell, length(bad), paste(utils::head(bad, 3), collapse = "; ")))
  }
  n_ann <- nrow(ann)

  d <- d[!d$is_annual, ]
  d$is_annual <- NULL
  if (DROP_BLANK) d <- d[!is.na(d[[val_col]]), ]

  ord_cols <- c("hs_code", setdiff(grp_cols, "hs_code"), "period")
  ord_cols <- ord_cols[ord_cols %in% names(d)]
  d <- d[do.call(order, lapply(ord_cols, function(k) d[[k]])), ]
  con <- if (GZIP_OUTPUT) gzfile(out_path, "w") else file(out_path, "w")
  write.csv(d, con, row.names = FALSE)
  close(con)

  cat(sprintf("  %-18s %8d rows  (%d annual cells checked and dropped)  %s\n",
              cell, nrow(d), n_ann, basename(out_path)))
  invisible(d)
}

for (cell in sort(unique(meta$cell))) reshape_cell(cell)

cat("\n--- tests ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails <<- fails + 1 }
}

outs <- list.files(UTO_OUT_DIR, pattern = "^uto_long_.*\\.csv(\\.gz)?$",
                   full.names = TRUE)
check("at least one long file was written", length(outs) > 0)

if (length(outs)) {
  s <- read.csv(outs[1], stringsAsFactors = FALSE,
                colClasses = c(hs_code = "character", period = "character"))

  check("no annual rows survived the drop",
        !any(nchar(s$period) != 6),
        "a period that is not YYYYMM means an annual cell leaked through")

  check("leading zeros intact on HS10 codes",
        any(grepl("^0", s$hs_code[nchar(s$hs_code) == 10])),
        "no HS10 code starts with 0 - the file was probably opened in Excel")

  check("commodity codes are 2, 4, 6 or 10 digits",
        all(nchar(s$hs_code[!is.na(s$hs_code)]) %in% c(2, 4, 6, 10)),
        paste("saw", paste(sort(unique(nchar(s$hs_code))), collapse = ",")))

  check("the 'All Commodities' roll-up has no code",
        any(is.na(s$hs_code)) && all(s$hs_desc[is.na(s$hs_code)] == "All Commodities"),
        "the roll-up row should carry NA as its code, not a parsed fragment")

  if ("domestic_foreign" %in% names(s)) {
    check("DomesticForeign holds only the two real values",
          all(s$domestic_foreign %in% c("Domestic Exports", "Foreign Exports")),
          paste(unique(s$domestic_foreign), collapse = ", "))
  } else {
    cat("  ....  no domestic_foreign column (import shape) - test skipped
")
  }

  dim_cols <- names(s)[!vapply(s, is.numeric, logical(1))]
  k <- do.call(paste, c(lapply(dim_cols, function(cc) s[[cc]]), list(sep = "
")))
  check(sprintf("no duplicate rows on (%s)", paste(dim_cols, collapse = ", ")),
        !anyDuplicated(k))

  if (grepl("can_exp_2002", outs[1])) {
    tot <- sum(s[[grep("^value", names(s), value = TRUE)[1]]][
      is.na(s$hs_code) & s$domestic_foreign == "Domestic Exports"], na.rm = TRUE)
    check("Canada 2002 domestic exports total = 142,528,743,935",
          abs(tot - 142528743935) < 1, sprintf("got %.0f", tot))
  }
}

cat(sprintf("\n%d test(s) failed.\n", fails))
if (fails > 0) stop("Reshape output failed its checks - do not use it.")
