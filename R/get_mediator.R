#' Sobel mediation screen across ML-CVI items
#'
#' Generic engine to screen many mediators between a binary culture IV (0/1)
#' and a continuous DV. You can pass a data.frame (`df`) or a path to an Excel file
#' (`path`, requires readxl). By default it looks for mediators named
#' `MLCVI1..MLCVI60` (case-insensitive). If none are found and
#' `allow_abc_fallback = TRUE`, it will also accept `abc1..abc60` **without renaming**.
#'
#' @param df A data.frame containing IV, DV, and mediator columns. Optional if `path` is given.
#' @param path Path to an Excel file (optional). If provided, data are read via readxl.
#' @param sheet Sheet name or index for Excel (optional).
#' @param iv Character scalar: column name of the binary independent variable (0/1).
#' @param dv Character scalar: column name of the dependent variable.
#' @param mediator_names Optional character vector of mediator column names to use directly.
#' @param mediator_regex Optional regex to auto-detect mediators from names(df).
#'   Defaults to `^mlcvi([1-9]|[1-5][0-9]|60)$` (case-insensitive).
#' @param expect_n Integer: expected number of mediators (default 60). Used for checks.
#' @param require_exact_count Logical; if TRUE (default), error if `expect_n` not matched.
#' @param allow_abc_fallback Logical; if TRUE (default), if no MLCVI mediators are found
#'   the function will also try `abc1..abc60` and **use those names as-is**.
#' @param alpha Numeric; significance level after p-adjustment (default 0.05).
#' @param adjust Character; p.adjust method (default "BH").
#' @param ignore_case Logical; case-insensitive matching for regex (default TRUE).
#' @param na_action One of `"complete.cases"` (default) or `"none"`. If `"complete.cases"`,
#'   per-mediator fits use rows complete on {IV, DV, mediator}.
#' @param coerce_iv01 Logical; if TRUE (default), coerce non-numeric two-level IV to 0/1.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{results}: data.frame sorted by \code{p_adj}, \code{sobel_p} with columns:
#'     mediator, n, a, a_se, b, b_se, indirect, sobel_z, sobel_p, p_adj,
#'     c_total, c_prime, prop_mediated
#'   \item \code{significant_items}: character vector of mediators with \code{p_adj <= alpha}
#'   \item \code{meta}: list with iv, dv, alpha, adjust, n_mediators, matched_names
#' }
#'
#' @examples
#' \dontrun{
#' # Using a data.frame already in memory:
#' out <- mlcvi.get.mediator(df = mydata, iv = "US0IN1", dv = "envavg")
#' out$significant_items
#' head(out$results, 10)
#'
#' # Using an Excel file (requires readxl installed):
#' out <- mlcvi.get.mediator(
#'   path = "Study 4a_Data.xlsx", iv = "US0IN1", dv = "envavg"
#' )
#' }
#' @export
mlcvi.get.mediator <- function(df = NULL,
                               path = NULL,
                               sheet = NULL,
                               iv,
                               dv,
                               mediator_names = NULL,
                               mediator_regex = "^mlcvi([1-9]|[1-5][0-9]|60)$",
                               expect_n = 60,
                               require_exact_count = TRUE,
                               allow_abc_fallback = TRUE,
                               alpha = 0.05,
                               adjust = "BH",
                               ignore_case = TRUE,
                               na_action = c("complete.cases", "none"),
                               coerce_iv01 = TRUE) {
  na_action <- match.arg(na_action)

  # read from file if path given
  if (!is.null(path)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required when using `path`. Install it or pass a data.frame to `df`.")
    }
    df <- readxl::read_excel(path, sheet = sheet)
  }
  if (is.null(df)) stop("Provide either `df` (data.frame) or `path` (Excel).")
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # check IV/DV exist
  if (!is.character(iv) || length(iv) != 1 || !(iv %in% names(df))) {
    stop("IV '", iv, "' is not present in data.")
  }
  if (!is.character(dv) || length(dv) != 1 || !(dv %in% names(df))) {
    stop("DV '", dv, "' is not present in data.")
  }

  # recode IV to 0/1 if needed
  if (coerce_iv01 && !is.numeric(df[[iv]])) {
    # sort() makes the mapping deterministic regardless of row order:
    # alphabetically/numerically first unique value → 0, second → 1.
    ux <- sort(unique(stats::na.omit(df[[iv]])))
    if (length(ux) == 2) {
      df[[iv]] <- as.integer(df[[iv]] == ux[2])
      message("IV '", iv, "' recoded to 0/1 (", ux[1], " \u2192 0, ", ux[2], " \u2192 1).")
    } else {
      stop("IV '", iv, "' is not numeric and doesn't have exactly two unique values; please recode to 0/1.")
    }
  }

  # find mediator columns
  if (!is.null(mediator_names)) {
    missing_m <- setdiff(mediator_names, names(df))
    if (length(missing_m)) stop("These mediator columns are missing: ", paste(missing_m, collapse = ", "))
    matched <- mediator_names
  } else {
    # Try MLCVI pattern first
    flags <- grepl(mediator_regex, names(df), ignore.case = isTRUE(ignore_case))
    matched <- names(df)[flags]

    # If none, try abc1..abc60 (no renaming)
    if (!length(matched) && isTRUE(allow_abc_fallback)) {
      abc_flags <- grepl("^abc([1-9]|[1-5][0-9]|60)$", names(df), ignore.case = TRUE)
      matched <- names(df)[abc_flags]
      if (length(matched) == expect_n) {
        message("Using 'abc1..", expect_n, "' columns as mediators.")
      }
    }
  }

  if (length(matched) == 0) {
    stop("No mediator columns detected. Provide `mediator_names` or columns matching regex: ",
         mediator_regex, " (or abc1..", expect_n, " if allow_abc_fallback = TRUE).")
  }
  if (isTRUE(require_exact_count) && length(matched) != expect_n) {
    stop("Detected ", length(matched), " mediator columns; expected ", expect_n, ".")
  }

  # coef and SE helper
  coef_se <- function(mod, term) {
    s <- summary(mod)$coef
    if (!term %in% rownames(s)) return(c(NA_real_, NA_real_))
    c(s[term, 1], s[term, 2])  # estimate, SE
  }

  # Sobel test — returns both z and p so they are never computed twice.
  # Returns NA when the standard error is zero or non-finite (e.g. perfect
  # collinearity, constant mediator column) to avoid NaN / Inf propagation.
  sobel_test <- function(a, b, se_a, se_b) {
    se_ab <- sqrt(b^2 * se_a^2 + a^2 * se_b^2)
    if (!is.finite(se_ab) || se_ab == 0) return(list(z = NA_real_, p = NA_real_))
    z <- (a * b) / se_ab
    list(z = z, p = 2 * stats::pnorm(-abs(z)))
  }

  # run per mediator
  out <- lapply(matched, function(med) {
    use_rows <- if (na_action == "complete.cases") {
      stats::complete.cases(df[, c(iv, dv, med)])
    } else rep(TRUE, nrow(df))

    d <- df[use_rows, , drop = FALSE]
    n_obs <- sum(use_rows)

    m_c  <- stats::lm(stats::as.formula(paste(dv, "~", iv)), data = d)
    m_a  <- stats::lm(stats::as.formula(paste(med, "~", iv)), data = d)
    m_bc <- stats::lm(stats::as.formula(paste(dv, "~", med, "+", iv)), data = d)

    a    <- coef_se(m_a, iv)[1]; sa <- coef_se(m_a, iv)[2]
    b    <- coef_se(m_bc, med)[1]; sb <- coef_se(m_bc, med)[2]
    ctot <- coef_se(m_c, iv)[1]
    cpr  <- coef_se(m_bc, iv)[1]

    ab  <- a * b
    sob <- sobel_test(a, b, sa, sb)   # single call; avoids duplicate computation

    data.frame(
      mediator = med,
      n = n_obs,
      a = a, a_se = sa,
      b = b, b_se = sb,
      indirect = ab,
      sobel_z = sob$z,
      sobel_p = sob$p,
      c_total = ctot,
      c_prime = cpr,
      prop_mediated = ifelse(is.finite(ctot) && ctot != 0, ab / ctot, NA_real_),
      stringsAsFactors = FALSE
    )
  })

  res <- do.call(rbind, out)
  res$p_adj <- stats::p.adjust(res$sobel_p, method = adjust)
  res <- res[order(res$p_adj, res$sobel_p), ]
  rownames(res) <- NULL

  sig <- res$mediator[res$p_adj <= alpha]

  list(
    results = res,
    significant_items = sig,
    meta = list(
      iv = iv, dv = dv, alpha = alpha, adjust = adjust,
      n_mediators = length(matched),
      matched_names = matched
    )
  )
}
