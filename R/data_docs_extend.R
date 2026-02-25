
#' Default ML-CVI Item Names (60 WVS variables)
#'
#' A character vector of the 60 World Values Survey variable names used by
#' the ML-CVI model.
#'
#' @format Character vector of length 60.
"mlcvi_items_default"

#' Default ML-CVI Feature Importance Weights
#'
#' A numeric vector of dropout-loss importance values for the 60 ML-CVI items.
#'
#' @format Numeric vector of length 60.
"mlcvi_weights_default"

#' Pre-trained Preprocessing Model
#'
#' A caret preProcess object fitted on the original ML-CVI training data.
#'
#' @format A preProcess object from the caret package.
"preProcessModel_default"

#' ML-CVI Training Input Matrix
#'
#' Individual-level WVS responses used for training.
#'
#' @format A numeric matrix.
"train_input_matrix"

#' ML-CVI Training Output Matrix
#'
#' One-hot encoded country membership matrix for training individuals.
#'
#' @format A numeric matrix.
"train_output_matrix"

