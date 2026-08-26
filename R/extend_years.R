#' Complete a country-year panel from sparse observations
#'
#' Fills the missing years of a country-year variable that is observed only
#' occasionally (for example at survey waves), optionally using annually
#' measured anchor variables to carry signal between and beyond the observed
#' years.
#'
#' @details
#' The method has two parts. When \code{anchors} are supplied, the anchor
#' coefficients are estimated once by pooled within-country regression (the
#' observed values and anchors are demeaned by country, so each country's
#' level is absorbed), giving a yearly anchor path per country. The part of
#' the observed values that the anchors do not explain is then interpolated
#' linearly between observed years and held constant beyond the first and
#' last observation. Observed years always keep their observed value
#' exactly. Without \code{anchors} the method reduces to interpolation with
#' end-carry.
#'
#' Anchor values missing for a target year are taken from that country's
#' nearest available anchor year. Countries with fewer than \code{min_obs}
#' observed years are dropped with a message.
#'
#' Freely available annual anchors include the KOF Globalisation Index
#' (\url{https://kof.ethz.ch}) and the World Bank development indicators
#' (\url{https://data.worldbank.org}); they are not bundled with the
#' package.
#'
#' @param data Data frame of observed values with the columns named by
#'   \code{country}, \code{year}, \code{value}. Rows with \code{NA} values
#'   are ignored.
#' @param anchors Optional data frame with the \code{country} and
#'   \code{year} columns plus one or more numeric anchor columns.
#' @param country,year,value Names of the key columns. \code{anchors} must
#'   use the same \code{country} and \code{year} names.
#' @param years Integer vector of target years. Defaults to the full span of
#'   observed years across all countries; supply a wider range to
#'   extrapolate (for example \code{1990:2024}).
#' @param min_obs Minimum observed years a country needs. Default 2.
#'
#' @return A data frame with the \code{country}, \code{year}, \code{value}
#'   columns and a logical \code{imputed} column (\code{FALSE} where the
#'   value is the original observation), one row per country and target
#'   year, with attributes \code{anchor_coef} (named coefficients, or NULL)
#'   and \code{dropped} (countries with too few observations).
#'
#' @examples
#' obs <- expand.grid(iso3 = c("AAA", "BBB"), year = c(2000, 2005, 2010))
#' obs$value <- c(10, 20, 12, 22, 14, 24)
#' anch <- expand.grid(iso3 = c("AAA", "BBB"), year = 1998:2014)
#' anch$trade <- with(anch, ifelse(iso3 == "AAA", 50, 70) + (year - 2000) / 2)
#'
#' out <- mlcvi_extend_years(obs, anchors = anch, value = "value",
#'                           years = 1998:2014)
#' head(out[out$iso3 == "AAA", ], 8)
#' attr(out, "anchor_coef")
#' @seealso [mlcvi_extend()] for imputing across countries,
#'   [mlcvi_build_matrix()] for distance matrices from item sets.
#' @export
mlcvi_extend_years <- function(data, anchors = NULL,
                               country = "iso3", year = "year",
                               value = "value", years = NULL,
                               min_obs = 2L) {
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  for (col in c(country, year, value)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in 'data'.")
    }
  }
  if (!is.numeric(min_obs) || length(min_obs) != 1L || min_obs < 2) {
    stop("'min_obs' must be a single number of at least 2.")
  }
  d <- data.frame(cty = as.character(data[[country]]),
                  yr = as.integer(data[[year]]),
                  val = as.numeric(data[[value]]),
                  stringsAsFactors = FALSE)
  d <- d[!is.na(d$val) & !is.na(d$yr), ]
  if (anyDuplicated(d[, c("cty", "yr")])) {
    stop("'data' has more than one value for the same country and year.")
  }

  n_obs   <- table(d$cty)
  dropped <- names(n_obs)[n_obs < min_obs]
  if (length(dropped) > 0) {
    message("Dropping ", length(dropped), " countr",
            if (length(dropped) == 1) "y" else "ies",
            " with fewer than ", min_obs, " observed years: ",
            paste(dropped, collapse = ", "))
    d <- d[!d$cty %in% dropped, ]
  }
  if (nrow(d) == 0) stop("No country has ", min_obs, " or more observations.")
  if (is.null(years)) years <- seq(min(d$yr), max(d$yr))
  years <- sort(unique(as.integer(years)))

  a <- NULL; anchor_names <- character(0); beta <- NULL
  if (!is.null(anchors)) {
    if (!is.data.frame(anchors)) stop("'anchors' must be a data frame.")
    for (col in c(country, year)) {
      if (!col %in% names(anchors)) {
        stop("Column '", col, "' not found in 'anchors'.")
      }
    }
    anchor_names <- setdiff(names(anchors), c(country, year))
    anchor_names <- anchor_names[vapply(anchors[anchor_names], is.numeric,
                                        logical(1))]
    if (length(anchor_names) == 0) {
      stop("'anchors' has no numeric anchor columns.")
    }
    a <- data.frame(cty = as.character(anchors[[country]]),
                    yr = as.integer(anchors[[year]]),
                    anchors[, anchor_names, drop = FALSE],
                    stringsAsFactors = FALSE)

    # pooled within-country regression: demean values and anchors by country
    tr <- merge(d, a, by = c("cty", "yr"))
    tr <- tr[stats::complete.cases(tr[, anchor_names, drop = FALSE]), ]
    enough <- nrow(tr) >= length(anchor_names) + 2 &&
      length(unique(tr$cty)) >= 2
    if (!enough) {
      message("Too few country-years match between 'data' and 'anchors' to ",
              "estimate anchor coefficients; continuing without anchors.")
      a <- NULL
    } else {
      dm <- function(x, g) x - stats::ave(x, g)
      Y  <- dm(tr$val, tr$cty)
      X  <- vapply(anchor_names, function(v) dm(tr[[v]], tr$cty),
                   numeric(nrow(tr)))
      fit  <- stats::lm.fit(as.matrix(X), Y)
      beta <- fit$coefficients
      beta[is.na(beta)] <- 0
      names(beta) <- anchor_names
    }
  }

  per_country <- lapply(split(d, d$cty), function(dc) {
    dc <- dc[order(dc$yr), ]
    path <- rep(0, length(years))
    if (!is.null(a)) {
      ac <- a[a$cty == dc$cty[1], ]
      if (nrow(ac) > 0) {
        av <- vapply(anchor_names, function(v) {
          ok <- !is.na(ac[[v]])
          if (!any(ok)) return(rep(0, length(years)))
          stats::approx(ac$yr[ok], ac[[v]][ok], xout = years, rule = 2)$y
        }, numeric(length(years)))
        path <- as.numeric(as.matrix(av) %*% beta)
      }
    }
    path_obs <- path[match(dc$yr, years)]
    res      <- dc$val - path_obs
    res_all  <- stats::approx(dc$yr, res, xout = years, rule = 2)$y
    out <- data.frame(cty = dc$cty[1], yr = years,
                      val = path + res_all,
                      imputed = !(years %in% dc$yr),
                      stringsAsFactors = FALSE)
    out$val[!out$imputed] <- dc$val[match(out$yr[!out$imputed], dc$yr)]
    out
  })
  out <- do.call(rbind, per_country)
  rownames(out) <- NULL
  names(out) <- c(country, year, value, "imputed")
  attr(out, "anchor_coef") <- beta
  attr(out, "dropped")     <- dropped
  out
}
