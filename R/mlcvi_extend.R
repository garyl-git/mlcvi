
#' Extend Country-Level Indicators via ML-CVI Ridge Regression
#'
#' Given a data frame of individual-level WVS responses and a (partially
#' observed) country-level indicator, this function aggregates individual
#' responses to country means, trains a Ridge regression whose penalty
#' weights reflect ML-CVI feature importances, and predicts (imputes)
#' the indicator for countries that are missing.
#'
#' @param country_scores A data frame with columns \code{s003} (character
#'   country code) and \code{values} (numeric, NA for countries to predict).
#' @param train_input_matrix A numeric matrix of individual-level WVS
#'   responses. Defaults to the built-in data.
#' @param country_vec A factor vector of length nrow(train_input_matrix)
#'   with levels like "s003_032".
#' @param feature_names Character vector of predictor column names.
#'   Defaults to built-in 60 ML-CVI items.
#' @param feature_weights Numeric importance weights (same length as
#'   feature_names). Defaults to built-in ML-CVI dropout-loss weights.
#' @param lambda Optional fixed Ridge lambda. If NULL (default), Bayesian
#'   Optimisation is used.
#' @param metric Character. The caret metric to optimise. Default "MedAE".
#' @param repeats Integer. Repeated-CV repeats. Default 50.
#' @param n_iter Integer. BO iterations (ignored when lambda is supplied).
#'   Default 50.
#' @param seed Integer. Random seed. Default 2025.
#' @param verbose Logical. Print BO progress? Default TRUE.
#'
#' @return A data frame with columns \code{s003} and \code{values}
#'   (observed where available, predicted where originally NA).
#'
#' @export
mlcvi_extend <- function(country_scores,
                         train_input_matrix = NULL,
                         country_vec        = NULL,
                         feature_names      = NULL,
                         feature_weights    = NULL,
                         lambda             = NULL,
                         metric             = "MedAE",
                         repeats            = 50L,
                         n_iter             = 50L,
                         seed               = 2025L,
                         verbose            = TRUE) {

  # defaults
  if (is.null(train_input_matrix)) {
    train_input_matrix <- mlcvi::train_input_matrix
  }
  if (is.null(country_vec)) {
    tom <- mlcvi::train_output_matrix
    cidx <- max.col(tom, ties.method = "first")
    country_vec <- factor(colnames(tom)[cidx], levels = colnames(tom))
  }
  if (is.null(feature_names)) {
    feature_names <- mlcvi::mlcvi_items_default
  }
  if (is.null(feature_weights)) {
    feature_weights <- mlcvi::mlcvi_weights_default
  }
  stopifnot(
    length(feature_names) == length(feature_weights),
    all(feature_names %in% colnames(train_input_matrix)),
    is.data.frame(country_scores),
    all(c("s003", "values") %in% names(country_scores))
  )

  # penalty weights from feature importance
  w_norm  <- feature_weights / sum(feature_weights)
  penalty <- 1 / (w_norm + .Machine$double.eps)

  # aggregate to country level
  tl <- country_scores[!is.na(country_scores$values), , drop = FALSE]

  df_agg <- .aggregate_to_country(train_input_matrix, country_vec, feature_names)

  df_train <- merge(df_agg, tl, by = "s003", all.x = TRUE)
  df_train <- df_train[!is.na(df_train$values), , drop = FALSE]

  X_mat <- as.matrix(df_train[, feature_names, drop = FALSE])
  y_vec <- df_train$values

  if (nrow(X_mat) < 5) {
    stop("Too few countries with non-missing values (found ", nrow(X_mat), ").")
  }

  # build Ridge method
  ridge_method <- make_ridge_method(penalty)

  # fit model
  if (is.null(lambda)) {
    cv_obj_local <- function(log10_lambda, FOLDS) {
      lam  <- 10^log10_lambda
      ctrl <- caret::trainControl(
        method = "repeatedcv",
        number = as.integer(round(FOLDS)),
        repeats = repeats,
        search = "grid",
        savePredictions = "final",
        allowParallel = TRUE,
        summaryFunction = fancySummary
      )
      set.seed(seed)
      fit <- caret::train(
        x = X_mat, y = y_vec,
        method = ridge_method,
        trControl = ctrl,
        tuneGrid = data.frame(lambda = lam),
        metric = metric,
        maximize = metric_maximize(metric)
      )
      val   <- fit$results[[metric]][1]
      score <- if (metric_maximize(metric)) val else -val
      list(Score = score, Pred = score)
    }

    set.seed(seed)
    BO <- rBayesianOptimization::BayesianOptimization(
      FUN         = cv_obj_local,
      bounds      = list(log10_lambda = c(-6, 3), FOLDS = c(3L, 10L)),
      init_points = 10,
      n_iter      = n_iter,
      acq         = "ucb",
      kappa       = 2.576,
      eps         = 0.0,
      verbose     = verbose
    )
    best_lambda <- 10^(BO$Best_Par["log10_lambda"])
    best_folds  <- as.integer(round(BO$Best_Par["FOLDS"]))
  } else {
    best_lambda <- lambda
    best_folds  <- 5L
  }

  # final fit
  ctrl_final <- caret::trainControl(
    method = "repeatedcv",
    number = best_folds,
    repeats = repeats,
    search = "grid",
    savePredictions = "final",
    allowParallel = TRUE,
    summaryFunction = fancySummary
  )
  set.seed(seed)
  final_model <- caret::train(
    x = X_mat, y = y_vec,
    method = ridge_method,
    trControl = ctrl_final,
    tuneGrid = data.frame(lambda = best_lambda),
    metric = metric,
    maximize = metric_maximize(metric)
  )

  # predict missing
  all_countries  <- unique(c(country_scores$s003, df_agg$s003))
  observed_codes <- tl$s003
  missing_codes  <- setdiff(all_countries, observed_codes)

  df_missing <- df_agg[df_agg$s003 %in% missing_codes, , drop = FALSE]

  if (nrow(df_missing) > 0) {
    X_new   <- as.matrix(df_missing[, feature_names, drop = FALSE])
    preds   <- as.vector(
      stats::predict(final_model, newdata = X_new, type = "raw")
    )
    imputed <- data.frame(s003 = df_missing$s003, values = preds,
                          stringsAsFactors = FALSE)
  } else {
    imputed <- data.frame(s003 = character(0), values = numeric(0),
                          stringsAsFactors = FALSE)
  }

  # combine and return
  result <- rbind(
    data.frame(s003 = tl$s003, values = tl$values, stringsAsFactors = FALSE),
    imputed
  )
  result
}


#' Train an ML-CVI Ridge model
#'
#' @inheritParams mlcvi_extend
#' @return A train object from the caret package.
#' @export
mlcvi_ridge_model <- function(country_scores,
                              train_input_matrix = NULL,
                              country_vec        = NULL,
                              feature_names      = NULL,
                              feature_weights    = NULL,
                              lambda             = NULL,
                              metric             = "MedAE",
                              repeats            = 50L,
                              n_iter             = 50L,
                              seed               = 2025L,
                              verbose            = TRUE) {

  if (is.null(train_input_matrix)) {
    train_input_matrix <- mlcvi::train_input_matrix
  }
  if (is.null(country_vec)) {
    tom <- mlcvi::train_output_matrix
    cidx <- max.col(tom, ties.method = "first")
    country_vec <- factor(colnames(tom)[cidx], levels = colnames(tom))
  }
  if (is.null(feature_names))  feature_names  <- mlcvi::mlcvi_items_default
  if (is.null(feature_weights)) feature_weights <- mlcvi::mlcvi_weights_default

  w_norm  <- feature_weights / sum(feature_weights)
  penalty <- 1 / (w_norm + .Machine$double.eps)

  tl <- country_scores[!is.na(country_scores$values), , drop = FALSE]
  df_agg   <- .aggregate_to_country(train_input_matrix, country_vec, feature_names)
  df_train <- merge(df_agg, tl, by = "s003", all.x = TRUE)
  df_train <- df_train[!is.na(df_train$values), , drop = FALSE]

  X_mat <- as.matrix(df_train[, feature_names, drop = FALSE])
  y_vec <- df_train$values
  ridge_method <- make_ridge_method(penalty)

  if (is.null(lambda)) {
    cv_obj_local <- function(log10_lambda, FOLDS) {
      lam  <- 10^log10_lambda
      ctrl <- caret::trainControl(
        method = "repeatedcv", number = as.integer(round(FOLDS)),
        repeats = repeats, search = "grid", savePredictions = "final",
        allowParallel = TRUE, summaryFunction = fancySummary
      )
      set.seed(seed)
      fit <- caret::train(x = X_mat, y = y_vec, method = ridge_method,
                          trControl = ctrl, tuneGrid = data.frame(lambda = lam),
                          metric = metric, maximize = metric_maximize(metric))
      val   <- fit$results[[metric]][1]
      score <- if (metric_maximize(metric)) val else -val
      list(Score = score, Pred = score)
    }
    set.seed(seed)
    BO <- rBayesianOptimization::BayesianOptimization(
      FUN = cv_obj_local,
      bounds = list(log10_lambda = c(-6, 3), FOLDS = c(3L, 10L)),
      init_points = 10, n_iter = n_iter, acq = "ucb",
      kappa = 2.576, eps = 0.0, verbose = verbose
    )
    best_lambda <- 10^(BO$Best_Par["log10_lambda"])
    best_folds  <- as.integer(round(BO$Best_Par["FOLDS"]))
  } else {
    best_lambda <- lambda
    best_folds  <- 5L
  }

  ctrl_final <- caret::trainControl(
    method = "repeatedcv", number = best_folds, repeats = repeats,
    search = "grid", savePredictions = "final", allowParallel = TRUE,
    summaryFunction = fancySummary
  )
  set.seed(seed)
  caret::train(x = X_mat, y = y_vec, method = ridge_method,
               trControl = ctrl_final, tuneGrid = data.frame(lambda = best_lambda),
               metric = metric, maximize = metric_maximize(metric))
}


#' Aggregate individual data to country means
#' @noRd
.aggregate_to_country <- function(input_matrix, country_vec, feature_names) {
  df <- data.frame(input_matrix[, feature_names, drop = FALSE],
                   s003 = country_vec, check.names = FALSE)
  agg <- stats::aggregate(
    df[, feature_names, drop = FALSE],
    by  = list(s003 = df$s003),
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  agg$s003 <- sub("^s003_", "", agg$s003)
  agg
}

