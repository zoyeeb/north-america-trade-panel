# install_packages.R
# Installs the CRAN packages the pipeline needs. Run once, before anything else.
#
# Run: Rscript setup/install_packages.R

pkgs <- c(
  "httr2",        # Census API requests (01_acquire, 04_related_party)
  "vroom",        # typed CSV reading throughout - see the schema note in README
  "arrow",        # optional Parquet path in 02_panel/panel_open.R
  "readxl",       # USA Trade Online exports (01_acquire/uto_*)
  "readr",
  "dplyr",
  "countrycode"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) == 0) {
  cat("All packages already installed.\n")
} else {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
}

cat("\nR version:", R.version.string, "\n")
cat("Developed against R 4.3+.\n")
