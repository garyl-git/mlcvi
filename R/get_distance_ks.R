#' Kogut–Singh cultural distance (Hofstede)
#'
#' Computes cultural distance between two countries (ISO alpha-3 codes)
#' using Hofstede's dimensions and the Kogut–Singh method.
#'
#' @param code1 Character scalar, ISO alpha-3 code (e.g., "USA")
#' @param code2 Character scalar, ISO alpha-3 code (e.g., "JPN")
#' @param method Character; one of "KS", "Shulgin", or "MLCVI".
#' @param digits Integer, number of digits to print in the report.
#' @param data Optional custom data to override the packaged dataset.
#'   \itemize{
#'     \item \strong{KS}: a data.frame with columns
#'       \code{Country_Code, pdi, idv, mas, uai, ltowvs, ivr}.
#'     \item \strong{Shulgin}: a data.frame (distance matrix) with ISO alpha-3
#'       row and column names.
#'     \item \strong{MLCVI}: a numeric matrix with ISO alpha-3 row and column
#'       names.
#'   }
#'   If \code{NULL} (default), the packaged dataset for the chosen method is used.
#' @param verbose Logical; if TRUE prints a multi-line report.
#'
#' @return The return value depends on \code{method}:
#'   \describe{
#'     \item{KS}{A list with elements \code{$KS_4dims} and \code{$KS_6dims},
#'       each a list with \code{$value} (numeric or NA) and \code{$error}
#'       (NULL or character).}
#'     \item{Shulgin}{A list with element \code{$Shulgin}, itself a list with
#'       \code{$value} and \code{$error}.}
#'     \item{MLCVI}{A list with element \code{$MLCVI}, itself a list with
#'       \code{$value} and \code{$error}.}
#'   }
#'
#' @examples
#' \dontrun{
#' mlcvi.get.distance("USA", "JPN", method = "KS")
#' }
#' @export
mlcvi.get.distance <- function(code1, code2, method = "KS", digits = 3,
                               data = NULL, verbose = TRUE) {
  method <- tolower(trimws(method))

  if (method == "ks") {
    if (is.null(data)) data <- .mlcvi_load_hofstede()
    res <- .kogut_singh_pair(code1, code2, data)
    if (isTRUE(verbose)) .ks_report(code1, code2, res, digits = digits)
    return(invisible(res))
  }

  if (method == "shulgin") {
    shulgin_data <- if (is.null(data)) .mlcvi_load_shulgin() else data
    res1 <- .shulgin_pair(code1, code2, shulgin_data)
    if (isTRUE(verbose)) .shulgin_report(code1, code2, res1, digits = digits)
    return(invisible(list(Shulgin = res1)))
  }

  if (method == "mlcvi") {
    mlcvi_mat <- if (is.null(data)) .mlcvi_load_mlcvi_matrix() else data
    res2 <- .mlcvi_pair(code1, code2, mlcvi_mat)
    if (isTRUE(verbose)) .mlcvi_report(code1, code2, res2, digits = digits)
    return(invisible(list(MLCVI = res2)))
  }

  stop("Unknown method: '", method, "'. Use method = 'KS', 'Shulgin', or 'MLCVI'.")
}

# ------------ internal helpers ------------

# Try to load an internal dataset named Hofstede_dims; if not found,
# fall back to reading the CSV from inst/extdata.
.mlcvi_load_hofstede <- function() {
  # 1) Prefer internal dataset if shipped via data/Hofstede_dims.rda
  if (exists("Hofstede_dims", where = asNamespace("mlcvi"), inherits = FALSE)) {
    return(get("Hofstede_dims", envir = asNamespace("mlcvi")))
  }

  # 2) Fallback to CSV in inst/extdata/
  csv <- system.file("extdata", "Hofstede_scores_updated.csv", package = "mlcvi")
  if (nzchar(csv) && file.exists(csv)) {
    df <- utils::read.csv(csv, stringsAsFactors = FALSE)
    return(df)
  }

  stop("Packaged Hofstede data not found. Please include either data/Hofstede_dims.rda ",
       "or inst/extdata/Hofstede_scores_updated.csv in the mlcvi package.")
}

.kogut_singh_dims4 <- c("pdi", "idv", "mas", "uai")
.kogut_singh_dims6 <- c("pdi", "idv", "mas", "uai", "ltowvs", "ivr")

.kogut_singh_pair <- function(code1, code2, data) {
  code1 <- toupper(trimws(code1))
  code2 <- toupper(trimws(code2))

  run_case <- function(dims, label) {
    miss_cols <- setdiff(dims, names(data))
    if (length(miss_cols) > 0) {
      return(list(value = NA_real_,
                  error = paste0("ERROR (Hofstede-", label, "): dataset is missing columns: ",
                                 paste(miss_cols, collapse = ", "))))
    }

    if (!("Country_Code" %in% names(data))) {
      return(list(value = NA_real_,
                  error = paste0("ERROR (Hofstede-", label, "): missing 'Country_Code' column.")))
    }

    cc <- toupper(trimws(data$Country_Code))
    i1 <- which(cc == code1)[1]
    i2 <- which(cc == code2)[1]
    if (is.na(i1)) return(list(value = NA_real_,
                               error = paste0("ERROR (Hofstede-", label, "): country code not found: ", code1)))
    if (is.na(i2)) return(list(value = NA_real_,
                               error = paste0("ERROR (Hofstede-", label, "): country code not found: ", code2)))

    s1 <- suppressWarnings(as.numeric(as.matrix(data[i1, dims, drop = FALSE])))
    s2 <- suppressWarnings(as.numeric(as.matrix(data[i2, dims, drop = FALSE])))
    names(s1) <- dims; names(s2) <- dims

    miss1 <- names(s1)[is.na(s1)]
    miss2 <- names(s2)[is.na(s2)]
    if (length(miss1) > 0 || length(miss2) > 0) {
      parts <- c()
      if (length(miss1) > 0) parts <- c(parts, paste0(code1, ": ", paste(miss1, collapse = ", ")))
      if (length(miss2) > 0) parts <- c(parts, paste0(code2, ": ", paste(miss2, collapse = ", ")))
      return(list(value = NA_real_,
                  error = paste0("ERROR (Hofstede-", label, "): missing dimensions -> ",
                                 paste(parts, collapse = " | "))))
    }

    vars <- sapply(dims, function(d) stats::var(data[[d]], na.rm = TRUE))
    valid <- !is.na(vars) & vars > 0
    if (!all(valid)) {
      if (!any(valid)) {
        return(list(value = NA_real_,
                    error = paste0("ERROR (Hofstede-", label, "): all variances are NA/0 for dims: ",
                                   paste(dims, collapse = ", "))))
      }
      dropped <- dims[!valid]
      warning("KS (", label, "): dropping dimension(s) with zero/NA variance: ",
              paste(dropped, collapse = ", "),
              ". KS index computed on reduced dimension set.")
      s1 <- s1[valid]; s2 <- s2[valid]; vars <- vars[valid]
    }

    list(value = mean(((s1 - s2)^2) / vars), error = NULL)
  }

  list(
    KS_4dims = run_case(.kogut_singh_dims4, "4-dims"),
    KS_6dims = run_case(.kogut_singh_dims6, "6-dims")
  )
}

.ks_report <- function(code1, code2, res, digits = 3) {
  code1 <- toupper(trimws(code1))
  code2 <- toupper(trimws(code2))

  fmt_val <- function(x) {
    if (is.null(x$error)) sprintf(paste0("%.", digits, "f"), x$value)
    else paste0("NA\n  ", x$error)
  }

  cat(
    "The cultural distance (Kogut-Singh method) between ", code1, " and ", code2, " is:\n",
    "4-dims: ", fmt_val(res$KS_4dims), "\n",
    "6-dims: ", fmt_val(res$KS_6dims), "\n",
    sep = ""
  )
}
