#' Build a cultural distance matrix from a custom item set
#'
#' Computes classical cultural distances between countries (or between
#' country-wave groups) from individual-level WVS responses on a
#' user-specified set of items.
#'
#' @details
#' Responses are first aggregated to group means (country, or country by
#' wave when \code{by_wave = TRUE}), dropping groups with fewer than
#' \code{min_n} respondents. Distances between every pair of groups are then
#' computed from the means:
#' \describe{
#'   \item{\code{"ks"}}{Kogut-Singh index with items as dimensions: the mean
#'     over items of the squared difference divided by the cross-group
#'     variance of that item.}
#'   \item{\code{"euclidean"}}{Square root of the sum of squared differences
#'     of item means.}
#'   \item{\code{"mahalanobis"}}{Squared Mahalanobis distance using the
#'     covariance matrix of the group means (with a small ridge added when
#'     the covariance is singular).}
#' }
#' When some items are unobserved for a group (for example, items not
#' administered in a wave), \code{"ks"} and \code{"euclidean"} use the items
#' available for both members of a pair and renormalize by the number used,
#' mirroring the reduced-dimension fallback of the Kogut-Singh method in
#' \code{\link{mlcvi.get.distance}}; pairs with fewer than \code{min_items}
#' shared items get \code{NA}. \code{"mahalanobis"} requires complete item
#' means and returns \code{NA} for any pair involving a missing item.
#'
#' These are classical distance constructions on custom items and are
#' labelled by method. The label "ML-CVI" refers to the published matrix
#' \code{\link{MLCVI_distance_matrix}} only.
#'
#' @param items Character vector of item (column) names in \code{data}.
#' @param data Numeric matrix of individual-level responses, one column per
#'   item. Defaults to the full ML-CVI training matrix (downloaded once and
#'   cached on first use). Pass \code{mlcvi:::mlcvi_train_input(small = TRUE)}
#'   for the bundled subsample.
#' @param country_vec Factor or character vector of length \code{nrow(data)}
#'   giving each row's country. If \code{NULL}, derived from the built-in
#'   output matrix and mapped to ISO alpha-3 codes.
#' @param wave_vec Integer vector of length \code{nrow(data)} giving each
#'   row's WVS wave. If \code{NULL} and \code{by_wave = TRUE}, derived from
#'   the \code{s002_1..s002_6} indicator columns of \code{data}.
#' @param method Character; \code{"ks"}, \code{"euclidean"}, or
#'   \code{"mahalanobis"}.
#' @param by_wave Logical; if TRUE, compute one matrix per wave.
#' @param min_n Integer; minimum respondents per group. Default 30.
#' @param min_items Integer; minimum shared non-missing items per pair.
#'   Default 1.
#'
#' @return If \code{by_wave = FALSE}, a symmetric numeric matrix with zero
#'   diagonal and country codes as dimnames, with attributes
#'   \code{items_used} (integer matrix of items entering each pair) and
#'   \code{n_per_group} (respondents per group). If \code{by_wave = TRUE}, a
#'   named list of such matrices, one per wave in which at least two
#'   countries reach \code{min_n}; waves that do not are skipped with a
#'   message.
#'
#' @examples
#' small <- mlcvi:::mlcvi_train_input(small = TRUE)
#' items <- mlcvi_items_default[1:10]
#'
#' # Kogut-Singh style distance on ten ML-CVI items, small subsample
#' m <- mlcvi_build_matrix(items, data = small, min_n = 20)
#' dim(m)
#' m["USA", "JPN"]
#'
#' # The result plugs into the lookup functions
#' mlcvi.get.distance("USA", "JPN", method = "MLCVI", data = m)
#' mlcvi_get_panel(data.frame(iso1 = "USA", iso2 = c("JPN", "DEU")),
#'                 method = "MLCVI", data = m)
#'
#' # One matrix per WVS wave (the subsample is thin per wave, so min_n is
#' # lowered here; use the default with the full data)
#' by_wave <- mlcvi_build_matrix(items, data = small, by_wave = TRUE,
#'                               min_n = 10)
#' names(by_wave)
#' sapply(by_wave, nrow)
#' @seealso [mlcvi_get_panel()], [mlcvi.get.distance()], [wvs_country_codes]
#' @export
mlcvi_build_matrix <- function(items,
                               data = NULL,
                               country_vec = NULL,
                               wave_vec = NULL,
                               method = c("ks", "euclidean", "mahalanobis"),
                               by_wave = FALSE,
                               min_n = 30L,
                               min_items = 1L) {
  method <- match.arg(method)
  if (!is.character(items) || length(items) < 1L) {
    stop("'items' must be a non-empty character vector.")
  }
  if (is.null(data)) data <- mlcvi_train_input()
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("'data' must be a numeric matrix.")
  }
  missing_items <- setdiff(items, colnames(data))
  if (length(missing_items) > 0) {
    stop("These items are not columns of 'data': ",
         paste(missing_items, collapse = ", "))
  }
  if (!is.numeric(min_n) || length(min_n) != 1L || min_n < 1) {
    stop("'min_n' must be a single positive number.")
  }
  if (!is.numeric(min_items) || length(min_items) != 1L || min_items < 1) {
    stop("'min_items' must be a single positive number.")
  }

  if (is.null(country_vec)) {
    country_vec <- .default_country_vec(data)
    country_vec <- .wvs_to_iso3(country_vec)
  }
  country_vec <- as.character(country_vec)
  if (length(country_vec) != nrow(data)) {
    stop("'country_vec' must have one entry per row of 'data'.")
  }

  if (!isTRUE(by_wave)) {
    return(.build_one_matrix(data[, items, drop = FALSE], country_vec,
                             method, min_n, min_items))
  }

  if (is.null(wave_vec)) wave_vec <- .default_wave_vec(data)
  if (length(wave_vec) != nrow(data)) {
    stop("'wave_vec' must have one entry per row of 'data'.")
  }
  waves <- sort(unique(wave_vec[!is.na(wave_vec)]))
  out <- lapply(waves, function(w) {
    rows <- !is.na(wave_vec) & wave_vec == w
    n_ok <- sum(table(country_vec[rows]) >= min_n)
    if (n_ok < 2L) {
      message("Skipping wave ", w, ": fewer than two countries with at ",
              "least ", min_n, " respondents.")
      return(NULL)
    }
    .build_one_matrix(data[rows, items, drop = FALSE], country_vec[rows],
                      method, min_n, min_items)
  })
  names(out) <- as.character(waves)
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) {
    stop("No wave has at least two countries with ", min_n, " respondents.")
  }
  out
}

# Group means, dropping small groups; returns list(means, n)
.group_means <- function(x, groups, min_n) {
  groups <- as.character(groups)
  n <- table(groups)
  keep <- names(n)[n >= min_n]
  dropped <- setdiff(names(n), keep)
  if (length(dropped) > 0) {
    message("Dropping ", length(dropped), " group(s) with fewer than ", min_n,
            " respondents: ", paste(dropped, collapse = ", "))
  }
  if (length(keep) < 2L) {
    stop("Fewer than two groups have at least ", min_n, " respondents.")
  }
  rows <- groups %in% keep
  x <- x[rows, , drop = FALSE]
  g <- factor(groups[rows], levels = keep)
  means <- vapply(colnames(x), function(col) {
    tapply(x[, col], g, function(v) {
      if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
    })
  }, numeric(length(keep)))
  means <- matrix(means, nrow = length(keep),
                  dimnames = list(keep, colnames(x)))
  list(means = means, n = as.integer(n[keep]))
}

.build_one_matrix <- function(x, groups, method, min_n, min_items) {
  gm <- .group_means(x, groups, min_n)
  means <- gm$means
  G <- nrow(means)
  codes <- rownames(means)
  D <- matrix(NA_real_, G, G, dimnames = list(codes, codes))
  used <- matrix(0L, G, G, dimnames = list(codes, codes))
  diag(D) <- 0
  diag(used) <- sum(!is.na(means[1, ]))

  if (method == "mahalanobis") {
    complete <- stats::complete.cases(means)
    if (sum(complete) >= 2L) {
      cm <- means[complete, , drop = FALSE]
      S  <- stats::cov(cm)
      if (ncol(cm) > 1L) {
        ev <- tryCatch(eigen(S, symmetric = TRUE, only.values = TRUE)$values,
                       error = function(e) NA_real_)
        if (anyNA(ev) || min(ev) <= 1e-10 * max(ev)) {
          S <- S + diag(1e-6 * max(diag(S)), ncol(S))
        }
      }
      Sinv <- solve(S)
      idx <- which(complete)
      for (a in seq_along(idx)) {
        for (b in seq_along(idx)) {
          if (a == b) next
          d <- cm[a, ] - cm[b, ]
          D[idx[a], idx[b]] <- as.numeric(d %*% Sinv %*% d)
          used[idx[a], idx[b]] <- ncol(cm)
        }
      }
    }
    diag(D) <- 0
    attr(D, "items_used")  <- used
    attr(D, "n_per_group") <- stats::setNames(gm$n, codes)
    return(D)
  }

  vars <- apply(means, 2, stats::var, na.rm = TRUE)
  if (method == "ks") {
    valid <- !is.na(vars) & vars > 0
    if (!all(valid)) {
      warning("Dropping item(s) with zero/NA variance across groups: ",
              paste(colnames(means)[!valid], collapse = ", "))
      means <- means[, valid, drop = FALSE]
      vars  <- vars[valid]
    }
    if (ncol(means) == 0L) stop("No items with positive variance remain.")
  }

  for (a in seq_len(G - 1L)) {
    for (b in (a + 1L):G) {
      ok <- !is.na(means[a, ]) & !is.na(means[b, ])
      k  <- sum(ok)
      used[a, b] <- used[b, a] <- k
      if (k < min_items) next
      d2 <- (means[a, ok] - means[b, ok])^2
      val <- if (method == "ks") mean(d2 / vars[ok]) else sqrt(sum(d2))
      D[a, b] <- D[b, a] <- val
    }
  }
  attr(D, "items_used")  <- used
  attr(D, "n_per_group") <- stats::setNames(gm$n, codes)
  D
}

# Map "s003_<code>" / numeric WVS codes to ISO alpha-3 using the bundled table
.wvs_to_iso3 <- function(x) {
  x <- as.character(x)
  num <- suppressWarnings(as.integer(sub("^s003_", "", x)))
  map <- mlcvi::wvs_country_codes
  iso <- map$iso3[match(num, map$wvs_code)]
  iso[is.na(iso)] <- x[is.na(iso)]
  iso
}

# Wave from the one-hot s002_1..s002_6 columns of the training matrix
.default_wave_vec <- function(data) {
  wave_cols <- grep("^s002_[0-9]+$", colnames(data), value = TRUE)
  if (length(wave_cols) == 0L) {
    stop("Cannot derive 'wave_vec': no s002_<wave> columns in 'data'. ",
         "Supply 'wave_vec' explicitly.")
  }
  w <- data[, wave_cols, drop = FALSE]
  hit <- max.col(w, ties.method = "first")
  out <- as.integer(sub("^s002_", "", wave_cols))[hit]
  out[rowSums(w, na.rm = TRUE) == 0] <- NA_integer_
  out
}
