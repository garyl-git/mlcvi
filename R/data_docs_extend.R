
#' Default ML-CVI Item Names (60 WVS variables)
#'
#' A character vector of the 60 World Values Survey variable names used by
#' the ML-CVI model.
#'
#' @format Character vector of length 60.
#' @source The 60 highest-ranked World Values Survey variables from the
#'   ML-CVI feature ranking; see
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"mlcvi_items_default"

#' Default ML-CVI Feature Importance Weights
#'
#' A numeric vector of dropout-loss importance values for the 60 ML-CVI items.
#'
#' @format Numeric vector of length 60.
#' @source Dropout-loss variable importances from the ML-CVI model; see
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"mlcvi_weights_default"

#' Pre-trained Preprocessing Model
#'
#' A caret preProcess object fitted on the original ML-CVI training data.
#'
#' @format A preProcess object from the caret package.
#' @source Fitted by the package authors on the ML-CVI training data; see
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"preProcessModel_default"

#' ML-CVI Training Output Matrix
#'
#' One-hot encoded country membership matrix for training individuals.
#'
#' @format A numeric matrix.
#' @source Derived from the World Values Survey country variable (s003) of
#'   the ML-CVI training sample; see
#'   \code{system.file("COPYRIGHTS", package = "mlcvi")}.
"train_output_matrix"

#' WVS country codes mapped to ISO alpha-3
#'
#' The 98 countries of the ML-CVI training sample, keyed by their WVS
#' numeric country code (ISO 3166-1 numeric, as used in
#' \code{train_output_matrix} column names \code{s003_<code>}), with the
#' corresponding ISO 3166-1 alpha-3 code and English name.
#'
#' @format A data frame with 98 rows and 3 columns: \code{wvs_code}
#'   (integer), \code{iso3} (character), \code{name} (character).
#' @source ISO 3166-1 via the ISOcodes package.
"wvs_country_codes"

