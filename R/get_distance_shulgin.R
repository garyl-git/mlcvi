# R/get_distance_shulgin.R

# ---- internal helpers ----
.clean_iso3 <- function(x) {
  x <- trimws(x)
  x <- toupper(x)
  gsub("[[:space:]]+", "", x)
}

.clean_iso3_dimnames <- function(df) {
  if (!is.null(rownames(df))) rownames(df) <- .clean_iso3(rownames(df))
  if (!is.null(colnames(df))) colnames(df) <- .clean_iso3(colnames(df))
  df
}

# load Shulgin data
.mlcvi_load_shulgin <- function() {
  # 1) prefer packaged dataset if present (created by data-raw/make_shulgin_data.R)
  if (exists("Shulgin_csv", where = asNamespace("mlcvi"), inherits = FALSE)) {
    return(get("Shulgin_csv", envir = asNamespace("mlcvi")))
  }

  # 2) fallback to shipped CSV
  csv <- system.file("extdata", "Shulgin.csv", package = "mlcvi")
  if (nzchar(csv) && file.exists(csv)) {
    df <- utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE, row.names = 1)
    df <- .clean_iso3_dimnames(df)
    # coerce to numeric
    df[] <- lapply(df, function(x) suppressWarnings(as.numeric(x)))
    return(df)
  }

  stop("Packaged Shulgin data not found. Include either data/Shulgin_csv.rda ",
       "or inst/extdata/Shulgin.csv in the mlcvi package.")
}

# returns list(value, error)
.shulgin_pair <- function(from, to, data = NULL) {
  if (is.null(data)) data <- .mlcvi_load_shulgin()
  from <- .clean_iso3(from); to <- .clean_iso3(to)

  if (!(from %in% rownames(data))) {
    return(list(value = NA_real_, error = paste0("ERROR (Shulgin): country code not found: ", from)))
  }
  if (!(to %in% colnames(data))) {
    return(list(value = NA_real_, error = paste0("ERROR (Shulgin): country code not found: ", to)))
  }

  val <- suppressWarnings(as.numeric(data[from, to]))
  if (is.na(val)) {
    return(list(value = NA_real_, error = paste0("ERROR (Shulgin): missing value for ", from, "→", to)))
  }
  list(value = val, error = NULL)
}

.shulgin_report <- function(from, to, res, digits = 3) {
  from <- .clean_iso3(from); to <- .clean_iso3(to)
  fmt <- if (is.null(res$error)) sprintf(paste0("%.", digits, "f"), res$value)
  else paste0("NA\n  ", res$error)

  cat(
    "The cultural distance (Shulgin method) between ", from, " and ", to, " is:\n",
    fmt, "\n",
    sep = ""
  )
}
