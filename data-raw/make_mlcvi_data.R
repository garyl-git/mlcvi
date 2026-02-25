# data-raw/make_mlcvi_distance_matrix.R
# Run once to create data/MLCVI_distance_matrix.rda

# install.packages("usethis")  # if needed

rds_path <- file.path("inst", "extdata", "MLCVI_distance_matrix.rds")
if (!file.exists(rds_path)) stop("RDS not found at ", rds_path)

# Read
MLCVI_distance_matrix <- readRDS(rds_path)

# Ensure it's a matrix (not a data.frame or dist)
if (inherits(MLCVI_distance_matrix, "dist")) {
  MLCVI_distance_matrix <- as.matrix(MLCVI_distance_matrix)
} else if (is.data.frame(MLCVI_distance_matrix)) {
  MLCVI_distance_matrix <- as.matrix(MLCVI_distance_matrix)
}

# Basic validation for a distance matrix
stopifnot(is.matrix(MLCVI_distance_matrix))

# Coerce storage to numeric
storage.mode(MLCVI_distance_matrix) <- "double"

# Validate dimnames presence
if (is.null(rownames(MLCVI_distance_matrix)) || is.null(colnames(MLCVI_distance_matrix))) {
  warning("Distance matrix has no row/col names; consider adding ISO3 or other IDs.")
}

# If you expect ISO3 codes like your other dataset, you can reuse your cleaner:
clean_iso3 <- function(x) { gsub("[[:space:]]+", "", toupper(trimws(x))) }
if (!is.null(rownames(MLCVI_distance_matrix))) rownames(MLCVI_distance_matrix) <- clean_iso3(rownames(MLCVI_distance_matrix))
if (!is.null(colnames(MLCVI_distance_matrix))) colnames(MLCVI_distance_matrix) <- clean_iso3(colnames(MLCVI_distance_matrix))

# Structural checks
if (nrow(MLCVI_distance_matrix) != ncol(MLCVI_distance_matrix)) {
  warning("Matrix is not square.")
}
if (!is.null(rownames(MLCVI_distance_matrix)) && !is.null(colnames(MLCVI_distance_matrix))) {
  if (!setequal(rownames(MLCVI_distance_matrix), colnames(MLCVI_distance_matrix))) {
    warning("Row/column names differ; matrix may not be aligned.")
  }
}
# Distance properties (soft checks; adjust as needed)
if (any(is.na(MLCVI_distance_matrix))) warning("Matrix contains NA values.")
if (any(MLCVI_distance_matrix < 0, na.rm = TRUE)) warning("Matrix contains negative values.")
if (!isTRUE(all.equal(MLCVI_distance_matrix, t(MLCVI_distance_matrix), tolerance = 1e-8))) {
  warning("Matrix is not perfectly symmetric.")
}
if (!isTRUE(all(diag(MLCVI_distance_matrix) == 0))) {
  warning("Diagonal is not all zeros.")
}

# Save as package dataset
usethis::use_data(MLCVI_distance_matrix, overwrite = TRUE, compress = "xz")
