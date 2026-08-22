.mlcvi_cache <- new.env(parent = emptyenv())

# Download location for the full training matrix (OSF project 3csbz).
# Users can override via options() or the environment variable.
.mlcvi_train_url_default <- "https://osf.io/download/st3hd/"
.mlcvi_train_md5 <- "81ad00e3cf1d5a6c28603ac3cc9fbc6b"
.mlcvi_train_file <- "train_input_matrix.rds"

.mlcvi_train_url <- function() {
  url <- getOption("mlcvi.train_url", "")
  if (!nzchar(url)) url <- Sys.getenv("MLCVI_TRAIN_URL", "")
  if (!nzchar(url)) url <- .mlcvi_train_url_default
  url
}

# Internal alias kept for existing call sites.
mlcvi_train_input <- function(small = FALSE) {
  mlcvi_training_data(small = small)
}

#' ML-CVI training data
#'
#' Returns the individual-level World Values Survey response matrix used to
#' train the ML-CVI model: either the bundled subsample, or the full matrix,
#' which is fetched from OSF the first time it is needed.
#'
#' @details
#' The subsample (2,450 respondents, 25 per country, all 594 variables)
#' ships inside the package and is what the examples, tests and vignette
#' use. The full matrix (313,856 respondents, about 280 MB) is not shipped;
#' it is downloaded from \url{https://osf.io/3csbz/} and verified against a
#' stored checksum.
#'
#' Where the downloaded file is kept is controlled by \code{cache}:
#' \describe{
#'   \item{\code{"session"}}{under \code{tempdir()}; discarded when R exits.
#'     This is what non-interactive sessions use unless you opt in below.}
#'   \item{\code{"persistent"}}{under
#'     \code{tools::R_user_dir("mlcvi", "cache")}; kept across sessions so the
#'     download happens once. Opt in for all sessions with
#'     \code{options(mlcvi.cache = "persistent")} or the environment
#'     variable \code{MLCVI_CACHE=persistent}, or choose the directory
#'     yourself with \code{options(mlcvi.cache_dir = ...)} /
#'     \code{MLCVI_CACHE_DIR}.}
#'   \item{\code{"ask"}}{the default: in an interactive session you are asked
#'     once whether to keep the file across sessions; in a non-interactive
#'     session this behaves like \code{"session"}.}
#' }
#' A file already present in the persistent location is always reused.
#' Remove it with \code{\link{mlcvi_clear_cache}()}.
#'
#' If the download fails (no internet, OSF unavailable, checksum mismatch)
#' the function returns \code{NULL} with an informative message rather than
#' an error. Use \code{options(mlcvi.train_url = ...)} or
#' \code{MLCVI_TRAIN_URL} to fetch the file from another location; the
#' checksum is only enforced for the default location.
#'
#' @param small Logical; \code{TRUE} for the bundled subsample, \code{FALSE}
#'   (default) for the full matrix.
#' @param cache One of \code{"ask"}, \code{"session"}, \code{"persistent"};
#'   see Details. Defaults to \code{getOption("mlcvi.cache")}, then the
#'   \code{MLCVI_CACHE} environment variable, then \code{"ask"}.
#' @param quiet Logical; if \code{TRUE}, suppress progress messages.
#' @return A numeric matrix with one row per respondent and one column per
#'   WVS variable (the subsample carries a \code{row_indices} attribute
#'   giving its rows' positions in the full matrix), or \code{NULL},
#'   invisibly, if the full matrix could not be obtained.
#' @examples
#' small <- mlcvi_training_data(small = TRUE)
#' dim(small)
#' head(colnames(small))
#' \dontrun{
#' # Full matrix: about 280 MB, downloaded once and cached
#' full <- mlcvi_training_data()
#' }
#' @seealso [mlcvi_clear_cache()], [mlcvi_extend()], [mlcvi_build_matrix()]
#' @export
mlcvi_training_data <- function(small = FALSE,
                                cache = getOption("mlcvi.cache",
                                                  Sys.getenv("MLCVI_CACHE",
                                                             "ask")),
                                quiet = FALSE) {
  if (!is.logical(small) || length(small) != 1L || is.na(small)) {
    stop("'small' must be a single non-missing logical value.", call. = FALSE)
  }
  if (!is.character(cache) || length(cache) != 1L ||
      !cache %in% c("ask", "session", "persistent")) {
    stop("'cache' must be one of \"ask\", \"session\", \"persistent\".",
         call. = FALSE)
  }
  key <- if (isTRUE(small)) "small" else "full"
  cached <- .mlcvi_cache[[key]]
  if (!is.null(cached)) return(cached)

  x <- if (isTRUE(small)) {
    .mlcvi_read_small()
  } else {
    .mlcvi_read_full(cache = cache, quiet = quiet)
  }
  if (is.null(x)) return(invisible(NULL))
  .mlcvi_cache[[key]] <- x
  x
}

#' Remove cached copies of the ML-CVI training matrix
#'
#' Deletes the full training matrix from the persistent cache directory
#' (\code{tools::R_user_dir("mlcvi", "cache")} or the directory set via
#' \code{options(mlcvi.cache_dir)} / \code{MLCVI_CACHE_DIR}) and from the
#' session cache, and forgets the in-memory copy.
#'
#' @return Invisibly, a character vector of the files that were removed
#'   (empty if none existed).
#' @examples
#' \dontrun{
#' # Removes the cached 280 MB download from your cache directory
#' mlcvi_clear_cache()
#' }
#' @seealso [mlcvi_training_data()]
#' @export
mlcvi_clear_cache <- function() {
  dirs  <- .mlcvi_cache_dirs()
  files <- file.path(unlist(dirs), .mlcvi_train_file)
  files <- files[file.exists(files)]
  if (length(files) > 0) unlink(files)
  rm(list = intersect("full", ls(.mlcvi_cache)), envir = .mlcvi_cache)
  invisible(files)
}

.mlcvi_cache_dirs <- function() {
  override <- getOption("mlcvi.cache_dir", "")
  if (!nzchar(override)) override <- Sys.getenv("MLCVI_CACHE_DIR", "")
  persistent <- if (nzchar(override)) {
    override
  } else {
    tools::R_user_dir("mlcvi", which = "cache")
  }
  list(persistent = persistent, session = file.path(tempdir(), "mlcvi"))
}

# Decide where a fresh download goes: "persistent" or "session".
.mlcvi_resolve_cache <- function(cache, dirs, quiet) {
  if (cache != "ask") return(cache)
  override <- getOption("mlcvi.cache_dir", "")
  if (!nzchar(override)) override <- Sys.getenv("MLCVI_CACHE_DIR", "")
  if (nzchar(override)) return("persistent")
  if (!interactive()) return("session")

  answer <- utils::askYesNo(
    paste0("Keep the ML-CVI training matrix (about 280 MB) in\n  ",
           dirs$persistent, "\nfor future sessions? (No = keep it only ",
           "for this session)"),
    default = FALSE
  )
  choice <- if (isTRUE(answer)) "persistent" else "session"
  options(mlcvi.cache = choice)
  choice
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

.mlcvi_say <- function(quiet, ...) {
  if (!isTRUE(quiet)) message(...)
  invisible(NULL)
}

# Load the full matrix. Returns NULL (after a message) on any failure.
.mlcvi_read_full <- function(cache = "session",
                             quiet = FALSE,
                             url = .mlcvi_train_url(),
                             md5 = if (identical(url, .mlcvi_train_url_default))
                               .mlcvi_train_md5 else NULL,
                             bundled = system.file("extdata", .mlcvi_train_file,
                                                   package = "mlcvi"),
                             dirs = .mlcvi_cache_dirs()) {
  # 1) Bundled copy, present in source checkouts
  if (nzchar(bundled) && file.exists(bundled)) return(readRDS(bundled))

  # 2) Earlier download, persistent or session; stale copies are replaced
  for (loc in c("persistent", "session")) {
    path <- file.path(dirs[[loc]], .mlcvi_train_file)
    if (!file.exists(path)) next
    if (!is.null(md5) && !identical(unname(tools::md5sum(path)), md5)) {
      .mlcvi_say(quiet, "Cached ML-CVI training matrix at ", path,
                 " does not match the expected checksum; replacing it.")
      unlink(path)
      next
    }
    return(readRDS(path))
  }

  # 3) Download
  if (!nzchar(url)) {
    .mlcvi_say(quiet, "No download location is configured for the ML-CVI ",
               "training matrix. Set options(mlcvi.train_url = ...) or ",
               "MLCVI_TRAIN_URL, or pass a matrix explicitly.")
    return(NULL)
  }
  where <- .mlcvi_resolve_cache(cache, dirs, quiet)
  dir   <- dirs[[where]]
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dir, .mlcvi_train_file)
  tmp  <- paste0(dest, ".download")
  on.exit(unlink(tmp), add = TRUE)

  old_timeout <- getOption("timeout")
  options(timeout = max(3600, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  .mlcvi_say(quiet, "Downloading the ML-CVI training matrix (about 280 MB) ",
             "from ", url, "\n  to ", dir,
             if (where == "session") " (this session only)" else "")
  status <- tryCatch(
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  if (!identical(status, 0L)) {
    .mlcvi_say(quiet, "Could not download the ML-CVI training matrix",
               if (is.character(status)) paste0(": ", status) else "",
               ". Check your internet connection, or set ",
               "options(mlcvi.train_url = ...) to another location.")
    return(NULL)
  }
  if (!is.null(md5)) {
    got <- unname(tools::md5sum(tmp))
    if (!identical(got, md5)) {
      .mlcvi_say(quiet, "The downloaded ML-CVI training matrix does not ",
                 "match the expected checksum (got ", got, "). The remote ",
                 "file may have changed or the download was corrupted; ",
                 "please retry or update the package.")
      return(NULL)
    }
  }
  x <- tryCatch(readRDS(tmp), error = function(e) NULL)
  if (!is.matrix(x)) {
    .mlcvi_say(quiet, "The downloaded file is not a valid ML-CVI training ",
               "matrix; please retry or update the package.")
    return(NULL)
  }
  if (!file.rename(tmp, dest)) file.copy(tmp, dest, overwrite = TRUE)
  x
}
