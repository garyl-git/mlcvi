.clean_iso3_maybe <- function(x) {
  # fallback if package-level helper isn't loaded
  if (exists(".clean_iso3", mode = "function")) return(get(".clean_iso3")(x))
  x <- trimws(x); x <- toupper(x); gsub("[[:space:]]+", "", x)
}

# load ML-CVI distance matrix
.mlcvi_load_mlcvi_matrix <- function() {
  # 1) Prefer internal dataset if shipped via data/MLCVI_distance_matrix.rda
  if (exists("MLCVI_distance_matrix", where = asNamespace("mlcvi"), inherits = FALSE)) {
    return(get("MLCVI_distance_matrix", envir = asNamespace("mlcvi")))
  }

  # 2) Fallback: inst/extdata/MLCVI_distance_matrix.rds
  rds <- system.file("extdata", "MLCVI_distance_matrix.rds", package = "mlcvi")
  if (nzchar(rds) && file.exists(rds)) {
    x <- readRDS(rds)
    # accept matrix or dist; coerce if needed
    if (inherits(x, "dist")) x <- as.matrix(x)
    if (is.data.frame(x)) x <- as.matrix(x)
    return(x)
  }

  stop("Packaged MLCVI matrix not found. Include either ",
       "data/MLCVI_distance_matrix.rda or inst/extdata/MLCVI_distance_matrix.rds.")
}

# pairwise lookup
.mlcvi_pair <- function(from, to, data = NULL) {
  if (is.null(data)) data <- .mlcvi_load_mlcvi_matrix()

  # Coerce to matrix if needed
  if (inherits(data, "dist")) data <- as.matrix(data)
  if (is.data.frame(data)) data <- as.matrix(data)
  if (!is.matrix(data)) {
    return(list(value = NA_real_, error = "ERROR (MLCVI): data is not a matrix/dist/data.frame"))
  }

  # Must have dimnames
  rn <- rownames(data); cn <- colnames(data)
  if (is.null(rn) || is.null(cn)) {
    return(list(value = NA_real_, error = "ERROR (MLCVI): matrix must have row and column names"))
  }

  from <- .clean_iso3_maybe(from); to <- .clean_iso3_maybe(to)

  if (!(from %in% rn)) {
    return(list(value = NA_real_, error = paste0("ERROR (MLCVI): country code not found (row): ", from)))
  }
  if (!(to %in% cn)) {
    return(list(value = NA_real_, error = paste0("ERROR (MLCVI): country code not found (col): ", to)))
  }

  # Direct lookup; if your matrix is symmetric, this already covers it
  val <- suppressWarnings(as.numeric(data[from, to]))
  if (is.na(val)) {
    return(list(value = NA_real_, error = paste0("ERROR (MLCVI): missing value for ", from, "→", to)))
  }

  list(value = val, error = NULL)
}

# print result
.mlcvi_report <- function(from, to, res, digits = 3) {
  from <- .clean_iso3_maybe(from); to <- .clean_iso3_maybe(to)
  fmt <- if (is.null(res$error)) sprintf(paste0("%.", digits, "f"), res$value)
  else paste0("NA\n  ", res$error)

  cat(
    "The cultural distance (MLCVI method) between ", from, " and ", to, " is:\n",
    fmt, "\n",
    sep = ""
  )
}
