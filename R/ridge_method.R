
#' Build a caret-compatible Ridge model list
#'
#' @param penalty_factors Numeric vector of per-feature penalty weights.
#' @return A list suitable for the method argument of caret::train().
#' @noRd
make_ridge_method <- function(penalty_factors) {
  list(
    type       = "Regression",
    library    = "glmnet",
    loop       = NULL,
    parameters = data.frame(
      parameter = "lambda",
      class     = "numeric",
      label     = "Lambda"
    ),
    grid = function(x, y, len = NULL, search = "grid") {
      data.frame(lambda = 0.01)
    },
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      glmnet::glmnet(
        x              = as.matrix(x),
        y              = y,
        alpha          = 0,
        lambda         = param$lambda,
        standardize    = TRUE,
        penalty.factor = penalty_factors,
        ...
      )
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      # glmnet returns an [n x 1] matrix; coerce to plain vector for caret
      as.vector(stats::predict(modelFit, as.matrix(newdata),
                               s = modelFit$tuneValue$lambda))
    },
    prob = NULL
  )
}

