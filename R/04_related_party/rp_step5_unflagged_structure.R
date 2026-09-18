# rp_step5_unflagged_structure.R
# Companion to rp_step5_export_gate.R. That measures the LEVEL of unflagged
# trade; this measures its STRUCTURE, which turns out to be more informative.
#
# In:   $DATA/rp/
# Out:  $DATA/rp_panel/
# Run:  Rscript R/04_related_party/rp_step5_unflagged_structure.R

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

IN_DIR  <- na_data("rp")
OUT_DIR <- na_data("rp_panel")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

TEST_COUNTRY   <- "CANADA"
PEER_COUNTRIES <- c("MEXICO", "CHINA", "JAPAN", "GERMANY", "UNITED KINGDOM")
RESIDUAL_CODE  <- "990000"   # SPECIAL CLASSIFICATION PROVISIONS, NESOI

ERAS <- list("2005-2010" = 2005:2010, "2021-2025" = 2021:2025)

COLS <- c("naics_raw", "country", "year",
          "exp_total", "exp_related", "exp_nonrelated", "exp_notreported",
          "imp_total", "imp_related", "imp_nonrelated", "imp_notreported")

files <- list.files(IN_DIR, pattern = "^rp_naics6_.*\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No rp_naics6_*.csv files found in ", IN_DIR)

keep <- c(TEST_COUNTRY, PEER_COUNTRIES)
raw_list <- list()
for (f in files) {
  d <- read.csv(f, skip = 4, header = TRUE, stringsAsFactors = FALSE,
                colClasses = "character")
  if (ncol(d) != length(COLS)) {
    stop("Unexpected column count in ", basename(f), ": got ", ncol(d))
  }
  names(d) <- COLS
  raw_list[[basename(f)]] <- d[d$country %in% keep, ]
}
rp <- do.call(rbind, raw_list)
rownames(rp) <- NULL
rp <- rp[grepl("^[0-9]{4}$", rp$year), ]
rp$year <- as.integer(rp$year)

num <- function(x) suppressWarnings(as.numeric(x))
for (v in COLS[4:11]) rp[[v]] <- num(rp[[v]])

rp$code <- sub("^([A-Za-z0-9]+)\\s.*$", "\\1", rp$naics_raw)

rows <- list()
for (ctry in c(TEST_COUNTRY, PEER_COUNTRIES)) {
  for (dir_ in c("exp", "imp")) {
    ncol_ <- if (dir_ == "exp") "exp_notreported" else "imp_notreported"
    tcol_ <- if (dir_ == "exp") "exp_total" else "imp_total"
    for (era in names(ERAS)) {
      s <- rp[rp$country == ctry & rp$year %in% ERAS[[era]], ]
      unflagged <- sum(s[[ncol_]], na.rm = TRUE)
      if (unflagged <= 0) next
      real <- s[s$code != RESIDUAL_CODE, ]
      outside <- sum(real[[ncol_]], na.rm = TRUE)
      by_code <- tapply(real[[ncol_]], real$code, sum, na.rm = TRUE)
      rows[[length(rows) + 1]] <- data.frame(
        country = ctry, direction = dir_, era = era,
        trade_total     = sum(s[[tcol_]], na.rm = TRUE),
        unflagged_total = unflagged,
        unflagged_share = unflagged / sum(s[[tcol_]], na.rm = TRUE),
        outside_residual_share = outside / unflagged,
        n_real_sectors_affected = sum(by_code > 0, na.rm = TRUE),
        stringsAsFactors = FALSE)
    }
  }
}
res <- do.call(rbind, rows)

out <- file.path(OUT_DIR, "rp_step5_unflagged_structure.csv")
write.csv(res, out, row.names = FALSE)

cat("=========================================================\n")
cat("Step 5 companion: WHERE the unflagged trade sits\n")
cat("=========================================================\n")
cat("unflagged%   = share of all trade carrying no related-party flag\n")
cat("outside 990k = share of THAT which sits in real industries rather than\n")
cat("               the administrative residual bucket\n")
cat("sectors      = how many real industries carry any unflagged value\n\n")

for (dir_ in c("exp", "imp")) {
  cat(sprintf("--- %s (%s) ---\n", dir_,
              if (dir_ == "exp") "US -> partner" else "partner -> US"))
  cat(sprintf("%-16s %-10s %10s %14s %9s\n",
              "country", "era", "unflagged%", "outside 990k", "sectors"))
  for (ctry in c(TEST_COUNTRY, PEER_COUNTRIES)) {
    for (era in names(ERAS)) {
      r <- res[res$country == ctry & res$direction == dir_ & res$era == era, ]
      if (!nrow(r)) next
      cat(sprintf("%-16s %-10s %9.2f%% %13.1f%% %9d\n",
                  ctry, era, 100 * r$unflagged_share,
                  100 * r$outside_residual_share, r$n_real_sectors_affected))
    }
  }
  cat("\n")
}

cat("---------------------------------------------------------\n")
cat("READING THIS\n")
cat("---------------------------------------------------------\n")
cat("The LEVEL comparison the protocol asks for shows Canada elevated.\n")
cat("The STRUCTURE comparison shows something categorical: by 2021-2025 every\n")
cat("peer has driven unflagged EXPORT value entirely into the residual code,\n")
cat("so the flag is populated for 100% of real-industry export trade. Canada\n")
cat("has not - essentially all of its unflagged export value is in real\n")
cat("industries, spread across hundreds of them.\n\n")
cat("That is a difference in KIND, not degree, and it is much stronger\n")
cat("evidence than the 3x level gap. Two honest qualifications:\n")
cat("  1. It does NOT establish the proposed mechanism. If US->Canada exports\n")
cat("     bypassed EEI entirely the unflagged share would be ~100%, not ~14%.\n")
cat("     Something populates the flag for the large majority of it.\n")
cat("  2. Canada's IMPORT side is structurally anomalous too, though at a much\n")
cat("     lower level. So 'imports are the clean control' needs qualifying.\n")
cat(sprintf("\nsaved %s\n", out))

cat("\n--- tests ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails <<- fails + 1 }
}

check("all six countries present in both directions",
      all(table(res$country, res$direction) > 0))

check("shares within [0,1]",
      all(res$unflagged_share >= 0 & res$unflagged_share <= 1) &&
      all(res$outside_residual_share >= 0 & res$outside_residual_share <= 1))

can <- res[res$country == "CANADA" & res$direction == "exp" &
             res$era == "2021-2025", ]
check("Canada 2021-25 exports: >90% of unflagged value is outside 990000",
      can$outside_residual_share > 0.90,
      sprintf("got %.1f%%", 100 * can$outside_residual_share))

peers <- res[res$country %in% PEER_COUNTRIES & res$direction == "exp" &
               res$era == "2021-2025", ]
check("every peer 2021-25 exports: <1% of unflagged value outside 990000",
      all(peers$outside_residual_share < 0.01),
      paste(sprintf("%s=%.1f%%", peers$country,
                    100 * peers$outside_residual_share), collapse = " "))

check("Canada 2021-25 exports affect >100 real sectors; no peer exceeds 5",
      can$n_real_sectors_affected > 100 &&
        all(peers$n_real_sectors_affected <= 5),
      sprintf("CAN=%d, peers max=%d", can$n_real_sectors_affected,
              max(peers$n_real_sectors_affected)))

peers_old <- res[res$country %in% PEER_COUNTRIES & res$direction == "exp" &
                   res$era == "2005-2010", ]
check("peers had dispersed unflagged export value in 2005-2010 and lost it",
      any(peers_old$outside_residual_share > 0.02),
      sprintf("max %.1f%%", 100 * max(peers_old$outside_residual_share)))

cat(sprintf("\n%d test(s) failed.\n", fails))
if (fails > 0) stop("Tests failed - do not quote these figures.")
