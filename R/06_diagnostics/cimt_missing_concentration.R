# cimt_missing_concentration.R
# The question cimt_quantity_sweep.R leaves open: the 20-27% of CIMT trade
# value carrying no usable quantity - is it spread thinly, or concentrated?
#
# In:   $CIMT_DIR/*.zip
# Out:  findings on stdout
# Run:  Rscript R/06_diagnostics/cimt_missing_concentration.R
#
# Value coverage of 73-80% is tolerable only if what is missing is diffuse. If
# it sits on crude petroleum, motor vehicles and electricity, the goods a
# pass-through exercise cares about most are the ones with no price.

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
  Imp     = list(pref = "ODPFN014", lvl = "HS10", ncol = 8L, desc = "ODPF_1_HS10Desc"),
  Tot_Exp = list(pref = "ODPFN017", lvl = "HS8",  ncol = 7L, desc = "ODPF_2_HS8Desc"),
  Dom_Exp = list(pref = "ODPFN016", lvl = "HS8",  ncol = 8L, desc = "ODPF_2_HS8Desc")
)
PARTNERS <- c("US", "MX")

if (!exists("YEARS_ONLY")) YEARS_ONLY <- NULL

zips <- sort(list.files(CIMT_DIR, pattern = "[.]zip$", full.names = TRUE))
parts <- list()

for (z in zips) {
  bn <- basename(z)
  yr <- as.integer(regmatches(bn, regexpr("[0-9]{4}", bn)))
  if (is.na(yr) || yr < 2002) next
  if (!is.null(YEARS_ONLY) && !(yr %in% YEARS_ONLY)) next

  dir_lab <- if (grepl("_Imp_", bn)) "Imp" else
             if (grepl("_Dom_Exp_", bn)) "Dom_Exp" else "Tot_Exp"
  spec <- NATIVE[[dir_lab]]

  contents <- tryCatch(utils::unzip(z, list = TRUE), error = function(e) NULL)
  if (is.null(contents)) next
  target <- contents$Name[grepl(spec$pref, basename(contents$Name))][1]
  if (is.na(target)) next

  d <- tryCatch(vroom(unz(z, target), delim = ",",
                      col_types = cols(.default = "c"),
                      progress = FALSE, altrep = FALSE),
                error = function(e) NULL)
  if (is.null(d) || ncol(d) != spec$ncol) next

  keep <- d[[3]] %in% PARTNERS
  if (!any(keep)) { rm(d); invisible(gc(verbose = FALSE)); next }

  code <- d[[2]][keep]
  val  <- suppressWarnings(as.numeric(d[[ncol(d) - 2]][keep]))
  qty  <- suppressWarnings(as.numeric(d[[ncol(d) - 1]][keep]))

  bad <- is.na(qty) | qty == 0
  val[is.na(val)] <- 0

  agg <- data.frame(
    direction  = dir_lab,
    year       = yr,
    code       = code,
    value      = val,
    value_miss = ifelse(bad, val, 0),
    stringsAsFactors = FALSE
  )
  a <- aggregate(cbind(value, value_miss) ~ direction + year + code,
                 data = agg, FUN = sum)
  parts[[length(parts) + 1]] <- a

  cat(sprintf("%-30s %-8s %6d codes\n", bn, dir_lab, nrow(a)))
  rm(d, code, val, qty, bad, agg, a); invisible(gc(verbose = FALSE))
}

all <- do.call(rbind, parts)

pooled <- aggregate(cbind(value, value_miss) ~ direction + code,
                    data = all, FUN = sum)
pooled$miss_share <- ifelse(pooled$value > 0,
                            100 * pooled$value_miss / pooled$value, NA_real_)

read_desc <- function(dir_lab) {
  spec <- NATIVE[[dir_lab]]
  z <- file.path(CIMT_DIR, sprintf("CIMT-CICM_%s_2025.zip", dir_lab))
  if (!file.exists(z)) return(NULL)
  ct <- utils::unzip(z, list = TRUE)
  tg <- ct$Name[grepl(spec$desc, basename(ct$Name), ignore.case = TRUE)][1]
  if (is.na(tg)) return(NULL)
  ln <- tryCatch(readLines(unz(z, tg), warn = FALSE), error = function(e) NULL)
  if (!is.null(ln)) ln <- iconv(ln, from = "latin1", to = "ASCII", sub = "")
  if (is.null(ln) || !length(ln)) return(NULL)

  m <- regmatches(ln, regexec("^([^ ]+) +([0-9]{6}) +([0-9]{6}) +([^ ]+) +(.*)$", ln))
  ok <- lengths(m) == 6L
  if (!any(ok)) return(NULL)
  m <- m[ok]

  d <- data.frame(
    code = vapply(m, `[`, "", 2),
    to   = vapply(m, `[`, "", 4),
    unit = vapply(m, `[`, "", 5),
    desc = trimws(sub("  +.*$", "", vapply(m, `[`, "", 6))),
    stringsAsFactors = FALSE
  )
  d <- d[order(d$code, d$to == "999912", d$to), ]
  d[!duplicated(d$code, fromLast = TRUE), ]
}
pooled$desc <- NA_character_
pooled$official_unit <- NA_character_
for (dl in unique(pooled$direction)) {
  dd <- read_desc(dl)
  if (is.null(dd)) next
  dd <- dd[!duplicated(dd$code), ]
  i <- pooled$direction == dl
  j <- match(pooled$code[i], dd$code)
  pooled$desc[i]          <- dd$desc[j]
  pooled$official_unit[i] <- dd$unit[j]
}

pooled$unit_is_na <- !is.na(pooled$official_unit) &
                     toupper(pooled$official_unit) == "N/A"

write.csv(pooled[order(pooled$direction, -pooled$value_miss), ],
          file.path(OUT_DIR, "cimt_missing_by_code.csv"), row.names = FALSE)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("CONCENTRATION OF MISSING VALUE (US+MX, pooled 2002-2026)\n")
cat(strrep("=", 78), "\n")

for (dl in c("Imp", "Tot_Exp", "Dom_Exp")) {
  p <- pooled[pooled$direction == dl, ]
  if (!nrow(p)) next
  p <- p[order(-p$value_miss), ]
  tot_miss <- sum(p$value_miss)
  tot_val  <- sum(p$value)

  cat(sprintf("\n%s  (%d codes, %.1f%% of value unusable)\n",
              dl, nrow(p), 100 * tot_miss / tot_val))

  for (k in c(1, 5, 10, 25, 50, 100)) {
    if (k > nrow(p)) break
    cat(sprintf("   top %3d codes: %5.1f%% of all missing value\n",
                k, 100 * sum(p$value_miss[1:k]) / tot_miss))
  }

  na_miss <- sum(p$value_miss[p$unit_is_na])
  cat(sprintf("   %5.1f%% of missing value is on codes whose OFFICIAL UNIT is N/A\n",
              100 * na_miss / tot_miss))
  cat(sprintf("   %5.1f%% is on codes that DO have a unit (a real gap, if large)\n",
              100 * (tot_miss - na_miss) / tot_miss))

  cat("   largest contributors:\n")
  for (i in seq_len(min(8, nrow(p)))) {
    cat(sprintf("     %-11s unit=%-4s %5.1f%% own  %4.1f%% of miss  %s\n",
                p$code[i],
                ifelse(is.na(p$official_unit[i]), "?", p$official_unit[i]),
                p$miss_share[i],
                100 * p$value_miss[i] / tot_miss,
                substr(ifelse(is.na(p$desc[i]), "(no description)", p$desc[i]), 1, 44)))
  }
}
cat("\nwrote ", file.path(OUT_DIR, "cimt_missing_by_code.csv"), "\n", sep = "")
