# data-raw/make_shulgin_data.R
# Run once to create data/Shulgin_csv.rda

# install.packages("usethis")  # if needed

csv_path <- file.path("inst", "extdata", "Shulgin.csv")
if (!file.exists(csv_path)) stop("CSV not found at ", csv_path)

Shulgin_csv <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)

# Clean names to ISO3
clean_iso3 <- function(x) { gsub("[[:space:]]+", "", toupper(trimws(x))) }
rownames(Shulgin_csv) <- clean_iso3(rownames(Shulgin_csv))
colnames(Shulgin_csv) <- clean_iso3(colnames(Shulgin_csv))

# Coerce to numeric
Shulgin_csv[] <- lapply(Shulgin_csv, function(x) suppressWarnings(as.numeric(x)))

# Validate
if (any(!grepl("^[A-Z]{3}$", rownames(Shulgin_csv)))) stop("Row names must be ISO3 codes.")
if (any(!grepl("^[A-Z]{3}$", colnames(Shulgin_csv)))) stop("Column names must be ISO3 codes.")
if (!setequal(rownames(Shulgin_csv), colnames(Shulgin_csv))) {
  warning("Row/column names differ; matrix may be asymmetric or not square over same set.")
}

usethis::use_data(Shulgin_csv, overwrite = TRUE)
