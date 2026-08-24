#' Cultural distances for a panel of country pairs
#'
#' Vectorized lookup of cultural distances for many country pairs at once,
#' for merging into dyadic (panel) data.
#'
#' @details
#' Each row of \code{pairs} is looked up independently and the input is
#' returned with two columns appended, so the result merges back one-to-one
#' with the original rows. Lookups that cannot be resolved (unknown country
#' code, missing cell, missing Hofstede dimension) yield \code{NA} in
#' \code{distance} and a short explanation in \code{distance_note} rather
#' than an error.
#'
#' The distance tables are time-invariant by design: the ML-CVI (Sheetal,
#' Madan, Lee, and Savani 2025) characterizes stable cross-country value
#' differences, so the same distance applies to every year of a panel. Any
#' further columns in \code{pairs} (such as a year) are passed through
#' untouched, which makes the result merge directly into panel data.
#'
#' @param pairs A data frame with the two country-code columns named by
#'   \code{iso1} and \code{iso2} (ISO alpha-3), and optionally a year column.
#' @param method Character; one of \code{"MLCVI"}, \code{"KS"},
#'   \code{"Shulgin"}, or \code{"matrix"}. Case-insensitive. \code{"matrix"}
#'   looks up a user-supplied square table passed as \code{data} (for
#'   example the output of \code{\link{mlcvi_build_matrix}}) and carries no
#'   method label of its own.
#' @param iso1,iso2 Names of the country-code columns in \code{pairs}.
#' @param data Optional custom table overriding the packaged one, with the
#'   same structure as for \code{\link{mlcvi.get.distance}}. Required when
#'   \code{method = "matrix"}.
#' @param ks_dims For \code{method = "KS"}: \code{"6dims"} (default) or
#'   \code{"4dims"}.
#'
#' @return \code{pairs} with two extra columns: \code{distance} (numeric) and
#'   \code{distance_note} (character, \code{NA} when the lookup succeeded).
#'
#' @examples
#' pairs <- data.frame(
#'   iso1 = c("USA", "USA", "DEU", "USA"),
#'   iso2 = c("JPN", "CHN", "CHN", "XXX"),
#'   year = c(2005, 2010, 2010, 2015)
#' )
#' mlcvi_get_panel(pairs, method = "MLCVI")
#' mlcvi_get_panel(pairs, method = "KS")
#' mlcvi_get_panel(pairs, method = "KS", ks_dims = "4dims")
#'
#' # Custom column names and a custom matrix
#' trade <- data.frame(origin = "USA", dest = c("JPN", "DEU"), yr = 2000)
#' mlcvi_get_panel(trade, method = "MLCVI", iso1 = "origin", iso2 = "dest",
#'                 data = MLCVI_distance_matrix)
#'
#' # A matrix built from a custom item set, looked up as-is
#' small <- mlcvi_training_data(small = TRUE)
#' m <- mlcvi_build_matrix(mlcvi_items_default[1:10], data = small, min_n = 20)
#' mlcvi_get_panel(pairs, method = "matrix", data = m)
#' @seealso [mlcvi.get.distance()] for single pairs with a printed report,
#'   [mlcvi_build_matrix()] to construct matrices from custom item sets.
#' @export
mlcvi_get_panel <- function(pairs,
                            method = c("MLCVI", "KS", "Shulgin", "matrix"),
                            iso1 = "iso1",
                            iso2 = "iso2",
                            data = NULL,
                            ks_dims = c("6dims", "4dims")) {
  if (!is.data.frame(pairs)) {
    stop("'pairs' must be a data frame.")
  }
  method  <- tolower(match.arg(method))
  ks_dims <- match.arg(ks_dims)

  for (col in c(iso1, iso2)) {
    if (!col %in% names(pairs)) {
      stop("Column '", col, "' not found in 'pairs'.")
    }
  }

  from <- .clean_iso3(as.character(pairs[[iso1]]))
  to   <- .clean_iso3(as.character(pairs[[iso2]]))

  if (method == "matrix" && is.null(data)) {
    stop("method = 'matrix' requires a square table in 'data'.")
  }

  res <- switch(
    method,
    ks      = .ks_vectorized(from, to, ks_dims, data),
    shulgin = .matrix_lookup_vectorized(
      from, to, if (is.null(data)) .mlcvi_load_shulgin() else data, "Shulgin"
    ),
    mlcvi   = .matrix_lookup_vectorized(
      from, to, if (is.null(data)) .mlcvi_load_mlcvi_matrix() else data, "MLCVI"
    ),
    matrix  = .matrix_lookup_vectorized(from, to, data, "matrix")
  )

  pairs$distance      <- res$value
  pairs$distance_note <- res$note
  pairs
}

# Vectorized [from, to] lookup in a square table with dimnames
.matrix_lookup_vectorized <- function(from, to, data, label) {
  if (inherits(data, "dist")) data <- as.matrix(data)
  if (is.data.frame(data)) data <- as.matrix(data)
  if (!is.matrix(data)) {
    stop("'data' for method '", label, "' must be a matrix, dist, or ",
         "data.frame.")
  }
  if (is.null(rownames(data)) || is.null(colnames(data))) {
    stop("'data' for method '", label, "' must have row and column names.")
  }
  rn <- .clean_iso3(rownames(data))
  cn <- .clean_iso3(colnames(data))

  i <- match(from, rn)
  j <- match(to, cn)
  n <- length(from)
  value <- rep(NA_real_, n)
  note  <- rep(NA_character_, n)

  bad_from <- is.na(i)
  bad_to   <- !bad_from & is.na(j)
  note[bad_from] <- paste0("country code not found: ", from[bad_from])
  note[bad_to]   <- paste0("country code not found: ", to[bad_to])

  ok <- !bad_from & !bad_to
  if (any(ok)) {
    v <- suppressWarnings(as.numeric(data[cbind(i[ok], j[ok])]))
    value[ok] <- v
    miss <- ok
    miss[ok] <- is.na(v)
    note[miss] <- paste0("missing value for ", from[miss], " -> ", to[miss])
  }
  list(value = value, note = note)
}

# Vectorized Kogut-Singh index for many pairs from a Hofstede-style table
.ks_vectorized <- function(from, to, ks_dims, data) {
  if (is.null(data)) data <- .mlcvi_load_hofstede()
  dims <- if (ks_dims == "4dims") .kogut_singh_dims4 else .kogut_singh_dims6

  n <- length(from)
  value <- rep(NA_real_, n)
  note  <- rep(NA_character_, n)

  miss_cols <- setdiff(c("Country_Code", dims), names(data))
  if (length(miss_cols) > 0) {
    note[] <- paste0("dataset is missing columns: ",
                     paste(miss_cols, collapse = ", "))
    return(list(value = value, note = note))
  }

  cc <- .clean_iso3(as.character(data$Country_Code))
  vars <- vapply(dims, function(d) stats::var(data[[d]], na.rm = TRUE),
                 numeric(1))
  valid <- !is.na(vars) & vars > 0
  if (!any(valid)) {
    note[] <- "all variances are NA/0"
    return(list(value = value, note = note))
  }
  if (!all(valid)) {
    warning("KS (", ks_dims, "): dropping dimension(s) with zero/NA ",
            "variance: ", paste(dims[!valid], collapse = ", "),
            ". KS index computed on reduced dimension set.")
    dims <- dims[valid]
    vars <- vars[valid]
  }

  scores <- as.matrix(data[, dims, drop = FALSE])
  storage.mode(scores) <- "double"

  i <- match(from, cc)
  j <- match(to, cc)
  bad_from <- is.na(i)
  bad_to   <- !bad_from & is.na(j)
  note[bad_from] <- paste0("country code not found: ", from[bad_from])
  note[bad_to]   <- paste0("country code not found: ", to[bad_to])

  ok <- !bad_from & !bad_to
  if (any(ok)) {
    s1 <- scores[i[ok], , drop = FALSE]
    s2 <- scores[j[ok], , drop = FALSE]
    d2 <- sweep((s1 - s2)^2, 2, vars, "/")
    v  <- rowMeans(d2)
    value[ok] <- v
    incomplete <- ok
    incomplete[ok] <- is.na(v)
    if (any(incomplete)) {
      m1 <- is.na(scores[i[incomplete], , drop = FALSE])
      m2 <- is.na(scores[j[incomplete], , drop = FALSE])
      note[incomplete] <- vapply(seq_len(sum(incomplete)), function(k) {
        parts <- character(0)
        if (any(m1[k, ])) {
          parts <- c(parts, paste0(from[incomplete][k], ": ",
                                   paste(dims[m1[k, ]], collapse = ", ")))
        }
        if (any(m2[k, ])) {
          parts <- c(parts, paste0(to[incomplete][k], ": ",
                                   paste(dims[m2[k, ]], collapse = ", ")))
        }
        paste0("missing dimensions -> ", paste(parts, collapse = " | "))
      }, character(1))
    }
  }
  list(value = value, note = note)
}
