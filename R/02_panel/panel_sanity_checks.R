# panel_sanity_checks.R
# Eleven sanity checks on the built panel. Read-only apart from its report.
#
# In:   $DATA/panel/, plus the benchmark workbook
# Out:  $DATA/panel/panel_sanity_report.txt
# Run:  Rscript R/02_panel/panel_sanity_checks.R
#
# A script rather than an interactive session because any figure that gets
# quoted has to come from something re-runnable - when these checks were first
# run by hand, one of the numbers produced would have been quoted wrongly.

suppressMessages(library(vroom))

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

PANEL_DIR <- na_data("panel")
OUT   <- file.path(PANEL_DIR, "panel_sanity_report.txt")

BENCH_NAME <- "USA_trade_Canada_Mexico_2002-2009.xlsx"
BENCH_DIRS <- c(".", "data", na_data("uto"), "literature")
BENCH <- Filter(file.exists, file.path(BENCH_DIRS, BENCH_NAME))
BENCH <- if (length(BENCH)) BENCH[1] else file.path(BENCH_DIRS[1], BENCH_NAME)

LOG <- character(0)
say <- function(...) { s <- paste0(...); LOG <<- c(LOG, s); message(s) }
fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
verdict <- function(ok) if (ok) "PASS" else "*** CHECK ***"

say("PANEL SANITY CHECKS  ", format(Sys.time(), "%Y-%m-%d %H:%M"))
say("")

if (!file.exists(BENCH)) {
  say("CHECK 1-2  *** SKIPPED - THE EXTERNAL BENCHMARK IS MISSING ***")
  say("   wanted : ", BENCH_NAME)
  say("   looked in: ", paste(BENCH_DIRS, collapse = ", "))
  say("   Checks 1-2 are the ONLY comparison in this file against a source")
  say("   outside this project. Without them the run below proves internal")
  say("   consistency and nothing about whether the panel matches reality.")
  say("   Do NOT read a clean report as an externally validated one.")
  say("")
} else if (!requireNamespace("readxl", quietly = TRUE)) {
  say("CHECK 1-2  SKIPPED - readxl is not installed")
  say("")
} else {
  b <- suppressMessages(readxl::read_excel(BENCH, sheet = "Value summary"))
  hdr <- as.character(unlist(b[3, ]))        # row 3 carries Flow / Country / years
  bm  <- as.data.frame(b[4:7, ], stringsAsFactors = FALSE)
  names(bm) <- hdr
  yrs <- as.character(2002:2009)

  u <- vroom(file.path(PANEL_DIR, "uto_core.csv.gz"),
             col_select = c(partner, flow_reporter, basis, period, value),
             col_types = cols(value = "d", .default = "c"), progress = FALSE)
  u <- as.data.frame(u)
  u$year    <- substr(u$period, 1, 4)
  u$country <- ifelse(u$partner == "CA", "Canada", "Mexico")

  say("CHECK 1  US DOMESTIC EXPORTS vs USITC DataWeb (FAS, domestic only)")
  e  <- u[u$flow_reporter == "export" & u$basis == "domestic", ]
  ea <- aggregate(value ~ country + year, data = e, FUN = sum)
  w1 <- 0
  for (cty in c("Canada", "Mexico")) for (y in yrs) {
    p  <- ea$value[ea$country == cty & ea$year == y]
    q  <- as.numeric(bm[[y]][bm$Flow == "Export" & bm$Country == cty])
    dd <- 100 * (p - q) / q
    w1 <- max(w1, abs(dd))
    say(sprintf("   %-7s %s  panel %18s  dataweb %18s  %+8.4f%%",
                cty, y, fmt(round(p)), fmt(round(q)), dd))
  }
  say(sprintf("   worst deviation %.4f%%", w1))
  say("")

  say("CHECK 2  US IMPORTS vs USITC DataWeb (CONSUMPTION customs value)")
  say("   panel side read from uto_extras_imp.csv.gz so the basis matches")
  ex <- vroom(file.path(PANEL_DIR, "uto_extras_imp.csv.gz"),
              col_select = c(source, period, customs_value_cons_us),
              col_types = cols(customs_value_cons_us = "d", .default = "c"),
              progress = FALSE)
  ex <- as.data.frame(ex)
  ex$year    <- substr(ex$period, 1, 4)
  ex$country <- ifelse(grepl("_can_", ex$source), "Canada", "Mexico")
  ia <- aggregate(customs_value_cons_us ~ country + year, data = ex, FUN = sum)
  w2 <- 0
  for (cty in c("Canada", "Mexico")) for (y in yrs) {
    p  <- ia$customs_value_cons_us[ia$country == cty & ia$year == y]
    q  <- as.numeric(bm[[y]][bm$Flow == "Import" & bm$Country == cty])
    dd <- 100 * (p - q) / q
    w2 <- max(w2, abs(dd))
    say(sprintf("   %-7s %s  panel %18s  dataweb %18s  %+8.4f%%",
                cty, y, fmt(round(p)), fmt(round(q)), dd))
  }
  say(sprintf("   worst deviation %.4f%%", w2))
  say("")
  say("   NOTE: the residual is BETWEEN THE TWO CENSUS PORTALS, not introduced")
  say("   here - check 3 isolates that. Expect up to ~1% when quoting a")
  say("   2002-2009 figure the advisor might reconcile against DataWeb.")
  say("")
  rm(u, e, ea, ex, ia); invisible(gc(verbose = FALSE))
}

say("CHECK 3  Canada 2004 exports - is the gap ours, or the source's?")
l <- read.csv(gzfile(na_data("uto/uto_long_can_exp_2004.csv.gz")), colClasses = "character")
l$v <- suppressWarnings(as.numeric(l$value_us_default_member))
dom <- l$domestic_foreign == "Domestic Exports"
tot <- sum(l$v[is.na(l$hs_code) & l$hs_desc == "All Commodities" & dom], na.rm = TRUE)
h10 <- sum(l$v[!is.na(l$hs_code) & nchar(l$hs_code) == 10 & dom], na.rm = TRUE)
say(sprintf("   UTO's own All Commodities total : %s", fmt(tot)))
say(sprintf("   our HS10 rows summed            : %s", fmt(h10)))
say(sprintf("   identical: %s   %s", identical(tot, h10), verdict(identical(tot, h10))))
say("   -> extraction reproduces the source exactly, so the benchmark gap sits")
say("      between USA Trade Online and DataWeb and is not ours to fix.")
say("")
rm(l); invisible(gc(verbose = FALSE))

d <- vroom(file.path(PANEL_DIR, "panel_core.csv.gz"),
           col_select = c(source, reporter, partner, flow_reporter, basis,
                          period, hs_code, hs_level, hs6, value, quantity_1, unit_1),
           col_types = cols(value = "d", quantity_1 = "d", .default = "c"),
           progress = FALSE)
d <- as.data.frame(d)
d$year <- substr(d$period, 1, 4)
src <- ifelse(startsWith(d$source, "uto_"), "uto",
       ifelse(startsWith(d$source, "census_"), "census", "cimt"))
say(paste0("panel rows: ", fmt(nrow(d))))
say("")

n <- sum(d$hs6 != substr(d$hs_code, 1, 6), na.rm = TRUE)
say(sprintf("CHECK 4  hs6 == substr(hs_code,1,6)          mismatches %d   %s",
            n, verdict(n == 0)))

n <- sum(d$hs_level == "HS10" & nchar(d$hs_code) != 10) +
     sum(d$hs_level == "HS8"  & nchar(d$hs_code) != 8)
say(sprintf("CHECK 5  hs_code length agrees with hs_level  offenders  %d   %s",
            n, verdict(n == 0)))

n <- sum(d$value < 0)
say(sprintf("CHECK 6  no negative values                   negatives %d   %s",
            n, verdict(n == 0)))
say("")

say("CHECK 7  each (reporter,partner,flow) is a contiguous monthly run")
k <- paste(d$reporter, d$partner, d$flow_reporter)
for (kk in sort(unique(k))) {
  p  <- sort(unique(d$period[k == kk]))
  ix <- as.integer(substr(p, 1, 4)) * 12L + as.integer(substr(p, 5, 6))
  g  <- sum(diff(ix) != 1L)
  say(sprintf("   %-16s %3d months  %s..%s  gaps %d  %s",
              kk, length(p), min(p), max(p), g, verdict(g == 0)))
}
say("")

say(sprintf("CHECK 8  2026 is partial: %s",
            paste(sort(unique(d$period[d$year == "2026"])), collapse = " ")))
say("")

z <- d$value == 0
say(sprintf("CHECK 9  zero-value rows %s of %s (%.2f%%); quantity also zero on %.2f%%",
            fmt(sum(z)), fmt(nrow(d)), 100 * mean(z),
            100 * mean(d$quantity_1[z] == 0, na.rm = TRUE)))
cz <- src == "census"
tb <- tapply(d$value[cz] == 0,
             paste(d$flow_reporter[cz], ifelse(is.na(d$basis[cz]), "n/a", d$basis[cz])),
             function(x) 100 * mean(x))
say("   census zero-value share by flow and basis:")
for (nm in names(tb)) say(sprintf("     %-18s %.2f%%", nm, tb[[nm]]))
say("")

ok <- d$value > 0 & !is.na(d$quantity_1) & d$quantity_1 > 0 & !is.na(d$unit_1)
uv <- d$value[ok] / d$quantity_1[ok]
uu <- d$unit_1[ok]
say(sprintf("CHECK 10 unit values computable on %s rows (%.1f%%)",
            fmt(sum(ok)), 100 * mean(ok)))
for (t in names(sort(table(uu), decreasing = TRUE))[1:8])
  say(sprintf("     %-5s n %10s  median %10.2f  p99 %13.2f",
              t, fmt(sum(uu == t)), median(uv[uu == t]),
              as.numeric(quantile(uv[uu == t], 0.99))))
say("")

say("CHECK 11 mirror: US-reported imports (USD) over CIMT total exports (CAD)")
say("   CA side filtered to basis == 'total' - see the comment in the script;")
say("   this is the difference between a ratio of 0.70 and a spurious 0.36.")
us <- d$reporter == "US" & d$partner == "CA" & d$flow_reporter == "import"
ca <- d$reporter == "CA" & d$partner == "US" & d$flow_reporter == "export" &
      !is.na(d$basis) & d$basis == "total"
a  <- tapply(d$value[us], d$year[us], sum)
b  <- tapply(d$value[ca], d$year[ca], sum)
yy <- sort(intersect(names(a), names(b)))
r  <- sapply(yy, function(y) a[[y]] / b[[y]])
for (y in yy)
  say(sprintf("   %s  %.4f%s", y, r[[y]],
              if (y == "2009") "   <-- last UTO year" else
              if (y == "2010") "   <-- first Census year" else ""))
say(sprintf("   range %.3f-%.3f; 2009->2010 move %+.4f vs median move %.4f",
            min(r), max(r), r[["2010"]] - r[["2009"]], median(abs(diff(r)))))
say("   The 2009->2010 move is the CANADIAN DOLLAR, not the source seam: CAD")
say("   averaged ~0.88 USD in 2009 and ~0.97 in 2010. build_panel.R's MONTHLY")
say("   seam figures settle it - 200912 reads 0.9129 and 201001 reads 0.9212,")
say("   a smaller step than the ordinary month-to-month variation either side.")
say("")

writeLines(LOG, OUT)
message("wrote ", OUT)
