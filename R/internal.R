.mlcvi_cache <- new.env(parent = emptyenv())

# Download location for the full training matrix; set once the file is
# hosted. Users can override via options() or the environment variable.
.mlcvi_train_url_default <- ""

.mlcvi_train_url <- function() {
  url <- getOption("mlcvi.train_url", "")
  if (!nzchar(url)) url <- Sys.getenv("MLCVI_TRAIN_URL", "")
  if (!nzchar(url)) url <- .mlcvi_train_url_default
  url
}

#' Load the ML-CVI training input matrix
#'
#' Loads the individual-level WVS response matrix, caching the result for the
#' remainder of the session. The subsampled matrix ships with the package.
#' The full matrix is looked up in `inst/extdata/` (source checkouts), then
#' in the user cache directory, and is otherwise downloaded once from the
#' configured URL and cached via `tools::R_user_dir()`.
#'
#' @param small Logical; if TRUE, load the subsampled matrix intended for
#'   examples and tests.
#' @return A numeric matrix of WVS responses.
#' @keywords internal
#' @noRd
mlcvi_train_input <- function(small = FALSE) {
  if (!is.logical(small) || length(small) != 1L || is.na(small)) {
    stop("'small' must be a single non-missing logical value.", call. = FALSE)
  }
  key <- if (isTRUE(small)) "small" else "full"
  cached <- .mlcvi_cache[[key]]
  if (!is.null(cached)) return(cached)

  x <- if (isTRUE(small)) .mlcvi_read_small() else .mlcvi_read_full()
  .mlcvi_cache[[key]] <- x
  x
}

.mlcvi_read_small <- function() {
  path <- system.file("extdata", "train_input_matrix_small.rds",
                      package = "mlcvi")
  if (!nzchar(path) || !file.exists(path)) {
    stop("mlcvi subsampled training matrix not found at ",
         "inst/extdata/train_input_matrix_small.rds. Reinstall the 'mlcvi' ",
         "package.", call. = FALSE)
  }
  readRDS(path)
}

.mlcvi_read_full <- function() {
  # 1) Bundled copy, present in source checkouts and local installs
  path <- system.file("extdata", "train_input_matrix.rds", package = "mlcvi")
  if (nzchar(path) && file.exists(path)) return(readRDS(path))

  # 2) User cache from an earlier download
  cache_dir <- tools::R_user_dir("mlcvi", which = "cache")
  cache_path <- file.path(cache_dir, "train_input_matrix.rds")
  if (file.exists(cache_path)) return(readRDS(cache_path))

  # 3) One-time download into the cache
  url <- .mlcvi_train_url()
  if (!nzchar(url)) {
    stop("The full ML-CVI training matrix (~280 MB) is not bundled with the ",
         "package and no download URL is configured. Set ",
         "options(mlcvi.train_url = ...) or the MLCVI_TRAIN_URL environment ",
         "variable, or pass your own matrix via 'train_input_matrix'.",
         call. = FALSE)
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  message("Downloading the ML-CVI training matrix (~280 MB) to ", cache_dir)
  tmp <- paste0(cache_path, ".download")
  on.exit(unlink(tmp), add = TRUE)
  status <- utils::download.file(url, tmp, mode = "wb", quiet = FALSE)
  if (status != 0L) {
    stop("Download of the ML-CVI training matrix failed (status ", status,
         ").", call. = FALSE)
  }
  x <- tryCatch(
    readRDS(tmp),
    error = function(e) {
      stop("Downloaded training matrix is not a valid RDS file: ",
           conditionMessage(e), call. = FALSE)
    }
  )
  if (!is.matrix(x)) {
    stop("Downloaded training matrix does not contain a matrix object.",
         call. = FALSE)
  }
  file.rename(tmp, cache_path)
  x
}
