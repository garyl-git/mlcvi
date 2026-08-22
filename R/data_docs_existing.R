
#' Hofstede Cultural Dimensions
#'
#' Country-level scores on Hofstede's six cultural dimensions (PDI, IDV, MAS, UAI, LTO, IVR).
#'
#' @format A data frame with one row per country.
#' @source Hofstede's published national dimension scores, as distributed
#'   with the package in \code{inst/extdata/Hofstede_scores_updated.csv}.
#'   Provenance and permission status are recorded in
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"Hofstede_dims"

#' ML-CVI Study 3A Data
#'
#' Individual-level data from Study 3A.
#'
#' @format A data frame.
#' @source Survey data collected by the package authors (Study 3a of the
#'   ML-CVI project); see \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"MLCVI_3A"

#' ML-CVI Study 4A Data
#'
#' Individual-level data from Study 4A.
#'
#' @format A data frame.
#' @source Survey data collected by the package authors (Study 4a of the
#'   ML-CVI project); see \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"MLCVI_4A"

#' ML-CVI Distance Matrix
#'
#' Pairwise cultural distances between countries computed via the ML-CVI method.
#'
#' @format A numeric matrix with ISO alpha-3 country codes as row and column names.
#' @source Derived by the package authors from the ML-CVI model; see
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"MLCVI_distance_matrix"

#' Shulgin Distance Data
#'
#' Pairwise cultural distance matrix from Shulgin et al.
#'
#' @format A data frame with ISO alpha-3 country codes as row and column names.
#' @source As distributed with the package in
#'   \code{inst/extdata/Shulgin.csv}; see
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"Shulgin_csv"

