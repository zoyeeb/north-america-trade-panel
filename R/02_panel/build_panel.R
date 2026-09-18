# build_panel.R
# Stacks the three extracted sources into one panel and verifies the result.
# Offline - reads only $DATA/panel/, writes only $DATA/panel/.
#
# In:   uto_core.csv.gz, census_core.csv.gz, cimt_core.csv.gz
# Out:  $DATA/panel/panel_core.csv.gz        the three, stacked
#       $DATA/panel/panel_coverage.csv       rows and value by source x year x flow
#       $DATA/panel/panel_build_report.txt   everything this run printed
# Run:  Rscript R/02_panel/build_panel.R
#
# STACKING IS NOT RECONCILIATION. The sources are not alternative measurements
# to be averaged: UTO and Census are the US side of the border in two eras, one
# continuous USD series that never overlaps, while CIMT is Canada measuring the
# same flows from the other side, in CAD, on its own classification. Both are
# kept; neither is corrected against the other.
#
# This script touches no raw data - its inputs are the three extractor outputs.

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

PANEL_DIR <- na_data("panel")
OUT_FILE  <- file.path(PANEL_DIR, "panel_core.csv.gz")
CHUNK     <- 500000L   # lines per streaming block

SOURCES <- list(
  uto    = list(file = "uto_core.csv.gz",    reporter = "US",
                era = "2002-2009", made_by = "R/02_panel/extract_uto.R",
                raw = na_data("uto")),
  census = list(file = "census_core.csv.gz", reporter = "US",
                era = "2010-2026", made_by = "R/02_panel/extract_census.R",
                raw = na_data("census_api")),
  cimt   = list(file = "cimt_core.csv.gz",   reporter = "CA",
                era = "2002-2026", made_by = "R/02_panel/extract_cimt.R",
                raw = CIMT_DIR)
)

LOG <- character(0)
say <- function(...) {
  s <- paste0(...)
  LOG <<- c(LOG, s)
  message(s)
}
fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)

missing <- Filter(function(k) !file.exists(file.path(PANEL_DIR, SOURCES[[k]]$file)),
                  names(SOURCES))
if (length(missing)) {
  for (k in missing)
    message("MISSING: ", file.path(PANEL_DIR, SOURCES[[k]]$file),
            "   run ", SOURCES[[k]]$made_by)
  stop("cannot build the panel - ", length(missing), " of ", length(SOURCES),
       " sources are not extracted yet")
}

say("PANEL BUILD  ", format(Sys.time(), "%Y-%m-%d %H:%M"))
say("")

say("SOURCE FILES")
for (k in names(SOURCES)) {
  s <- SOURCES[[k]]
  f <- file.path(PANEL_DIR, s$file)
  mt <- file.mtime(f)
  raw_mt <- if (dir.exists(s$raw)) {
    r <- list.files(s$raw, recursive = TRUE, full.names = TRUE)
    if (length(r)) max(file.mtime(r)) else as.POSIXct(NA)
  } else as.POSIXct(NA)
  flag <- if (!is.na(raw_mt) && raw_mt > mt) "  *** OLDER THAN ITS RAW INPUTS ***" else ""
  say(sprintf("  %-8s %-22s %7.1f MB  built %s%s", k, s$file,
              file.size(f) / 1e6, format(mt, "%Y-%m-%d %H:%M"), flag))
}
say("")

SUM_KEY <- c("reporter", "partner", "flow_reporter", "direction",
             "basis", "currency", "hs_level")

headers <- character(0)
summ <- list(); src_rows <- integer(0); src_val <- list(); src_periods <- list()

for (k in names(SOURCES)) {
  f <- file.path(PANEL_DIR, SOURCES[[k]]$file)
  say("READING ", k)

  hcon <- gzfile(f, "rt"); h <- readLines(hcon, 1L); close(hcon)
  headers <- c(headers, h)

  d <- vroom(f, col_types = cols(.default = "c"), progress = FALSE)
  d <- as.data.frame(d, stringsAsFactors = FALSE)

  for (nm in c("value", "quantity_1", "quantity_2"))
    d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  check_schema(d, paste0(k, " (re-read)"))

  key <- paste(d$period, d$reporter, d$partner, d$flow_reporter,
               d$basis, d$hs_code, sep = "\r")
  if (anyDuplicated(key)) {
    dup <- unique(key[duplicated(key)])
    stop(k, ": ", length(dup), " key(s) appear more than once at the panel ",
         "grain (period, reporter, partner, flow, basis, hs_code), e.g.\n  ",
         paste(gsub("\r", " | ", utils::head(dup, 5)), collapse = "\n  "))
  }
  rm(key); invisible(gc(verbose = FALSE))

  g <- do.call(paste, c(lapply(SUM_KEY, function(x) d[[x]]),
                        list(d$period), list(sep = "\r")))
  gn <- tapply(d$value, g, length)
  gv <- tapply(d$value, g, sum)
  agg <- data.frame(
    src   = k,
    g     = names(gn),
    n     = as.integer(gn),
    value = as.numeric(gv[names(gn)]),
    stringsAsFactors = FALSE
  )
  parts <- do.call(rbind, strsplit(agg$g, "\r", fixed = TRUE))
  colnames(parts) <- c(SUM_KEY, "period")
  parts <- as.data.frame(parts, stringsAsFactors = FALSE)
  for (nm in names(parts)) parts[[nm]][parts[[nm]] == "NA"] <- NA_character_
  agg <- cbind(agg["src"], parts, agg[c("n", "value")])
  summ[[k]] <- agg

  src_rows[k]    <- nrow(d)
  src_val[[k]]   <- tapply(d$value, d$currency, sum)
  src_periods[[k]] <- sort(unique(d$period))

  say(sprintf("  %s rows, %s periods (%s to %s)", fmt(nrow(d)),
              length(src_periods[[k]]), min(src_periods[[k]]), max(src_periods[[k]])))
  for (cur in names(src_val[[k]]))
    say(sprintf("    %s %s", cur, fmt(round(src_val[[k]][[cur]]))))

  rm(d, g, agg, parts); invisible(gc(verbose = FALSE))
}

if (length(unique(headers)) != 1L) {
  for (i in seq_along(headers))
    message("  ", names(SOURCES)[i], ": ", headers[i])
  stop("the three sources do not share a byte-identical header - they cannot ",
       "be concatenated as raw lines, and the schema has drifted")
}
say("")
say("header identical across all three sources - raw-line concatenation is safe")

us_a <- src_periods$uto; us_b <- src_periods$census
overlap <- intersect(us_a, us_b)
if (length(overlap))
  stop("UTO and Census both cover ", length(overlap), " period(s) - the US side ",
       "would be double counted: ", paste(utils::head(overlap, 6), collapse = ", "))

us <- sort(union(us_a, us_b))
yy <- as.integer(substr(us, 1, 4)); mm <- as.integer(substr(us, 5, 6))
idx <- yy * 12L + mm
gaps <- which(diff(idx) != 1L)
if (length(gaps)) {
  holes <- paste(us[gaps], "->", us[gaps + 1L])
  stop("the US side has ", length(gaps), " gap(s) in its monthly timeline: ",
       paste(holes, collapse = "; "))
}
say(sprintf("US side contiguous: %s months, %s to %s, no overlap and no gap",
            length(us), min(us), max(us)))
say(sprintf("  UTO    %s to %s (%d months)", min(us_a), max(us_a), length(us_a)))
say(sprintf("  Census %s to %s (%d months)", min(us_b), max(us_b), length(us_b)))
say("")

all_b <- do.call(rbind, summ)
usb   <- all_b[all_b$reporter == "US", ]
bkey  <- paste(usb$partner, usb$flow_reporter, usb$period)
has_t <- unique(bkey[!is.na(usb$basis) & usb$basis == "total"])
has_s <- unique(bkey[!is.na(usb$basis) & usb$basis %in% c("domestic", "foreign")])
both  <- intersect(has_t, has_s)
if (length(both))
  stop(length(both), " US flow-period(s) carry BOTH basis 'total' AND the ",
       "domestic/foreign split - summing them double counts: ",
       paste(utils::head(both, 5), collapse = "; "))
say("US basis unambiguous: no flow-period mixes 'total' with domestic/foreign")

all_b$year <- substr(all_b$period, 1, 4)
fk <- paste(all_b$reporter, all_b$partner, all_b$flow_reporter)
n_chg <- 0L
for (k in sort(unique(fk))) {
  x <- all_b[fk == k, ]
  by_yr <- tapply(x$basis, x$year,
                  function(b) paste(sort(unique(ifelse(is.na(b), "n/a", b))),
                                    collapse = "+"))
  yrs <- names(by_yr)
  for (i in which(by_yr[-1] != by_yr[-length(by_yr)]) + 1L) {
    n_chg <- n_chg + 1L
    say(sprintf("  BASIS CHANGES  %-14s %s %-17s -> %s %s",
                k, yrs[i - 1L], by_yr[[i - 1L]], yrs[i], by_yr[[i]]))
  }
}
if (!n_chg) say("basis stable: every flow reports the same basis set in every year")
say("")

say("WRITING ", OUT_FILE)
tmp <- paste0(OUT_FILE, ".part")   # same .part-then-rename convention as the
out <- gzfile(tmp, "wt")
written <- integer(0)

writeLines(headers[1], out)
for (k in names(SOURCES)) {
  con <- gzfile(file.path(PANEL_DIR, SOURCES[[k]]$file), "rt")
  readLines(con, 1L)                      # drop the header
  n <- 0L
  repeat {
    ln <- readLines(con, n = CHUNK)
    if (!length(ln)) break
    writeLines(ln, out)
    n <- n + length(ln)
  }
  close(con)
  written[k] <- n
  say(sprintf("  %-8s %s lines", k, fmt(n)))
}
close(out)

bad <- names(which(written != src_rows[names(written)]))
if (length(bad))
  stop("lines written do not match rows read for: ",
       paste(sprintf("%s (%s vs %s)", bad, fmt(written[bad]), fmt(src_rows[bad])),
             collapse = ", "),
       " - a field probably contains a newline")

if (!file.rename(tmp, OUT_FILE))
  stop("could not replace ", OUT_FILE, " with the newly written ", basename(tmp),
       " - the old panel is still in place and the new one is at ", tmp)
say(sprintf("  total %s rows, %.1f MB", fmt(sum(written)), file.size(OUT_FILE) / 1e6))
say("")

say("VERIFYING ", basename(OUT_FILE))
con <- gzfile(OUT_FILE, "rt")
hdr <- strsplit(gsub('"', "", readLines(con, 1L)), ",", fixed = TRUE)[[1]]
if (!identical(hdr, CORE_COLS))
  stop("the written panel's header is not CORE_COLS")
i_val <- match("value", CORE_COLS); i_src <- match("source", CORE_COLS)
i_cur <- match("currency", CORE_COLS)

back_n <- integer(0); back_v <- list()
repeat {
  ln <- readLines(con, n = CHUNK)
  if (!length(ln)) break
  p <- strsplit(ln, ",", fixed = TRUE)
  w <- lengths(p)
  if (any(w != length(CORE_COLS)))
    stop("a row in the written panel has ", w[which(w != length(CORE_COLS))[1]],
         " fields, not ", length(CORE_COLS), " - a field contains a comma")
  m <- matrix(unlist(p, use.names = FALSE), nrow = length(CORE_COLS))
  fn  <- gsub('"', "", m[i_src, ])
  key <- ifelse(startsWith(fn, "uto_"), "uto",
         ifelse(startsWith(fn, "census_"), "census", "cimt"))
  cur <- gsub('"', "", m[i_cur, ])
  val <- as.numeric(m[i_val, ])
  t1 <- tapply(val, key, length)
  for (nm in names(t1)) back_n[nm] <- sum(back_n[nm], t1[[nm]], na.rm = TRUE)
  t2 <- tapply(val, paste(key, cur), sum)
  for (nm in names(t2)) back_v[[nm]] <- sum(back_v[[nm]], t2[[nm]], na.rm = TRUE)
}
close(con)

for (k in names(SOURCES)) {
  if (!identical(as.integer(back_n[k]), as.integer(src_rows[k])))
    stop("row count changed for ", k, ": ", fmt(src_rows[k]), " in, ",
         fmt(back_n[k]), " out")
  for (cur in names(src_val[[k]])) {
    a <- src_val[[k]][[cur]]
    b <- back_v[[paste(k, cur)]]
    if (is.null(b))
      stop("currency ", cur, " is in ", k, " on the way in but absent on the ",
           "way out - rows were lost in the copy")
    if (abs(a - b) > 1e-12 * max(abs(a), 1))
      stop("value total changed for ", k, " ", cur, ": ",
           fmt(round(a)), " in, ", fmt(round(b)), " out")
  }
  say(sprintf("  %-8s %s rows and every currency total unchanged", k, fmt(back_n[k])))
}
say("")

cov <- do.call(rbind, summ)
cov$year <- substr(cov$period, 1, 4)
cov <- aggregate(cbind(n, value) ~ src + reporter + partner + flow_reporter +
                   direction + basis + currency + hs_level + year,
                 data = transform(cov, direction = ifelse(is.na(direction), ".", direction),
                                  basis = ifelse(is.na(basis), ".", basis)),
                 FUN = sum)
cov <- cov[order(cov$reporter, cov$partner, cov$flow_reporter, cov$basis, cov$year), ]
rownames(cov) <- NULL
write.csv(cov, file.path(PANEL_DIR, "panel_coverage.csv"), row.names = FALSE)
say("coverage: ", fmt(nrow(cov)), " source x flow x year cells -> panel_coverage.csv")
say("")

say("ROWS BY REPORTER AND DIRECTION  (direction '.' = Canada-Mexico, no US view)")
tb <- tapply(cov$n, list(paste(cov$reporter, cov$partner), cov$direction), sum)
tb[is.na(tb)] <- 0
LOG <- c(LOG, capture.output(print(tb)))
print(tb)
say("")

say("SEAM DIAGNOSTIC 2009/2010 - US-reported over CA-reported, US-Canada only")
say("  ratio is USD over CAD, unconverted, so it drifts with FX. A STEP at the")
say("  seam is the signal; a drift is not. Diagnostic only - nothing is gated.")
say("  Canada only: CIMT is Canada's data, so its Mexico rows are Canada-Mexico")
say("  trade, not a mirror of anything the US side reports.")

all_s <- do.call(rbind, summ)
us <- all_s$reporter == "US" & all_s$partner == "CA"
ca <- all_s$reporter == "CA" & all_s$partner == "US"

keep <- (us | (ca & (all_s$flow_reporter == "import" |
                     (all_s$flow_reporter == "export" & all_s$basis == "total"))))
seam <- all_s[keep & !is.na(all_s$direction) &
              all_s$period >= "200901" & all_s$period <= "201012", ]

if (nrow(seam) && length(unique(seam$reporter)) == 2L) {
  seam$side <- ifelse(seam$reporter == "US", "us", "ca")
  a <- aggregate(value ~ side + direction + period, data = seam, FUN = sum)
  for (dd in sort(unique(a$direction))) {
    x <- a[a$direction == dd, ]
    pu <- x$value[x$side == "us"]; names(pu) <- x$period[x$side == "us"]
    pc <- x$value[x$side == "ca"]; names(pc) <- x$period[x$side == "ca"]
    per <- sort(intersect(names(pu), names(pc)))
    if (!length(per)) next
    say(sprintf("  direction=%s  (%s)", dd,
                if (dd == "exp") "US -> Canada" else "Canada -> US"))
    for (p in per)
      say(sprintf("    %s  %.4f%s", p, pu[[p]] / pc[[p]],
                  if (p == "200912") "   <-- last UTO month" else
                  if (p == "201001") "   <-- first Census month" else ""))
  }
} else {
  say("  no comparable US/CA rows in 2009-2010 - skipped")
}
say("")

writeLines(LOG, file.path(PANEL_DIR, "panel_build_report.txt"))
message("wrote ", OUT_FILE)
message("wrote ", file.path(PANEL_DIR, "panel_coverage.csv"))
message("wrote ", file.path(PANEL_DIR, "panel_build_report.txt"))
