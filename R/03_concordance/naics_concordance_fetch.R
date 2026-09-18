# naics_concordance_fetch.R
# Downloads the three official Census NAICS revision concordances spanning the
# 2005-2025 benchmark window and flattens them into one tidy edge list.
#
# In:   nothing (network)
# Out:  $DATA/naics_concordance/naics_concordance_edges.csv
# Run:  Rscript R/03_concordance/naics_concordance_fetch.R
#
# rp_apply_threshold.R reads that CSV as the authority for which NAICS6 codes
# are comparable across vintages. A Python equivalent with no package
# dependencies is in naics_concordance_fetch.py.

if (!requireNamespace("readxl", quietly = TRUE)) {


  stop("readxl is not installed. Install it with:\n",
       "  install.packages('readxl')\n",
       "or run the Python equivalent instead: python R/03_concordance/naics_concordance_fetch.py")
}
library(readxl)

local({
  for (up in c(".", "..", "../..", "../../..")) {
    f <- file.path(up, "R/config.R")
    if (file.exists(f)) return(source(f))
  }
  stop("Cannot find R/config.R from ", getwd(),
       ". Run from the repo root, or open north-america-panel.Rproj.")
})

BASE    <- "https://www.census.gov/naics/concordances/"
OUT_DIR <- na_data("naics_concordance")

FILES <- list(
  list(vintage = "2007->2012", fname = "2007_to_2012_NAICS.xls"),
  list(vintage = "2012->2017", fname = "2012_to_2017_NAICS.xlsx"),
  list(vintage = "2017->2022", fname = "2017_to_2022_NAICS.xlsx")
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

  fetch_if_missing <- function(fname) {
  path <- file.path(OUT_DIR, fname)
  if (file.exists(path)) {
    cat("using cached", path, "\n")
    return(path)
  }
  url <- paste0(BASE, fname)
  cat("downloading", url, "\n")
  part <- paste0(path, ".part")
  ok <- tryCatch({
    utils::download.file(url, part, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) { cat("  download failed:", conditionMessage(e), "\n"); FALSE })
  if (!ok || !file.exists(part) || file.size(part) == 0) {
    unlink(part)
    stop("Could not download ", url)
  }
  file.rename(part, path)
  path
}

read_concordance <- function(path) {
  d <- readxl::read_excel(path, sheet = 1, col_names = FALSE,
                          col_types = "text", .name_repair = "minimal")
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  if (ncol(d) < 3) {
    stop("Only ", ncol(d), " columns in ", basename(path),
         " - expected at least 3 (old code, old title, new code).")
  }

  first_col <- trimws(ifelse(is.na(d[[1]]), "", d[[1]]))
  hdr <- which(grepl("NAICS Code$", first_col[seq_len(min(10, nrow(d)))]))
  if (length(hdr) == 0) {
    stop("Could not find the header row in ", basename(path),
         " - layout changed. Expected a first cell ending in 'NAICS Code' ",
         "within the first 10 rows.")
  }
  list(data = d[seq.int(hdr[1] + 1, nrow(d)), , drop = FALSE],
       header_row = hdr[1])
}

clean <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  trimws(gsub("[\r\n]+", " ", x))
}

edges <- data.frame(vintage = character(0), old_code = character(0),
                    old_title = character(0), new_code = character(0),
                    new_title = character(0), code_changed = character(0),
                    stringsAsFactors = FALSE)

for (f in FILES) {
  path <- fetch_if_missing(f$fname)
  got  <- read_concordance(path)
  d    <- got$data

  old_code  <- clean(d[[1]])
  old_title <- clean(d[[2]])
  new_code  <- clean(d[[3]])
  new_title <- if (ncol(d) >= 4) clean(d[[4]]) else rep("", nrow(d))

  keep <- grepl("^[0-9]{6}$", old_code) & grepl("^[0-9]{6}$", new_code)

  add <- data.frame(
    vintage      = rep(f$vintage, sum(keep)),
    old_code     = old_code[keep],
    old_title    = old_title[keep],
    new_code     = new_code[keep],
    new_title    = new_title[keep],
    code_changed = ifelse(old_code[keep] != new_code[keep], "1", "0"),
    stringsAsFactors = FALSE)

  edges <- rbind(edges, add)
  cat(sprintf("  %s: %d six-digit pairs, %d where the code changed\n",
              f$vintage, nrow(add), sum(add$code_changed == "1")))
}

if (nrow(edges) == 0) {
  stop("No edges parsed - refusing to write an empty concordance.")
}

csv_field <- function(x) {
  needs <- grepl('[",\r\n]', x)
  ifelse(needs, paste0('"', gsub('"', '""', x, fixed = TRUE), '"'), x)
}

out_path <- file.path(OUT_DIR, "naics_concordance_edges.csv")
cols <- c("vintage", "old_code", "old_title", "new_code", "new_title",
          "code_changed")

lines <- c(
  paste(cols, collapse = ","),
  do.call(paste, c(lapply(cols, function(k) csv_field(edges[[k]])), sep = ","))
)

con <- file(out_path, open = "wb")
writeLines(lines, con, sep = "\r\n")
close(con)

chg <- edges[edges$code_changed == "1", c("old_code", "new_code")]
chg <- unique(chg)

nodes <- unique(c(chg$old_code, chg$new_code))
parent <- setNames(nodes, nodes)

find <- function(x) {
  while (parent[[x]] != x) x <- parent[[x]]
  x
}
for (i in seq_len(nrow(chg))) {
  a <- find(chg$old_code[i]); b <- find(chg$new_code[i])
  if (a != b) parent[[a]] <- b
}
comps <- length(unique(vapply(nodes, find, character(1))))

cat(sprintf("\nwrote %s\n", out_path))
cat(sprintf("  %d rows; %d distinct changed-code edges; %d official lineages spanning %d codes\n",
            nrow(edges), nrow(chg), comps, length(nodes)))

cat("\n--- tests ---\n")
fails <- 0
check <- function(label, ok, detail = "") {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", label))
  else { cat(sprintf("  FAIL  %s %s\n", label, detail)); fails <<- fails + 1 }
}

check("all three vintages present",
      setequal(unique(edges$vintage),
               vapply(FILES, function(f) f$vintage, character(1))),
      paste("got", paste(unique(edges$vintage), collapse = ", ")))

check("every code on both sides is exactly six digits",
      all(grepl("^[0-9]{6}$", edges$old_code)) &&
        all(grepl("^[0-9]{6}$", edges$new_code)))

check("code_changed agrees with the codes it describes",
      all((edges$code_changed == "1") == (edges$old_code != edges$new_code)))

check("no duplicate (vintage, old_code, new_code) rows",
      !any(duplicated(edges[, c("vintage", "old_code", "new_code")])))

check("no title contains a raw line break",
      !any(grepl("[\r\n]", c(edges$old_title, edges$new_title))))

autos <- unique(c(
  edges$new_code[edges$old_code %in% c("336111", "336112") & edges$code_changed == "1"],
  edges$old_code[edges$new_code %in% c("336110") & edges$code_changed == "1"]))
check("autos: 336111/336112 -> 336110 is in the official tables",
      "336110" %in% autos,
      paste("got", paste(autos, collapse = ", ")))

petro <- unique(edges$new_code[edges$old_code == "211111" & edges$code_changed == "1"])
check("petroleum: 211111 -> 211120 + 211130 is in the official tables",
      all(c("211120", "211130") %in% petro),
      paste("got", paste(petro, collapse = ", ")))

check("tires 326211 never changes code in any vintage",
      !any(edges$code_changed == "1" &
             (edges$old_code == "326211" | edges$new_code == "326211")))
