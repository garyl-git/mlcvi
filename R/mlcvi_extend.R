
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
#'   responses. Defaults to the full ML-CVI training matrix, which is
#'   downloaded once and cached on first use (about 280 MB); see Details.
#'   Pass \code{mlcvi:::mlcvi_train_input(small = TRUE)} for a bundled
#'   subsample suitable for quick experiments.
#' @param country_vec A factor vector of length nrow(train_input_matrix)
#'   with levels like "s003_032". If NULL, it is derived from the built-in
#'   output matrix (respecting a \code{row_indices} attribute on subsampled
#'   input).
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
#' @details
#' The full training matrix is not shipped inside the package. On first use
#' it is downloaded from OSF (\url{https://osf.io/3csbz/}), or from the
#' location given by \code{getOption("mlcvi.train_url")} or the
#' \code{MLCVI_TRAIN_URL} environment variable, and cached under
#' \code{tools::R_user_dir("mlcvi", "cache")}. Later calls read the cache.
#'
#' @examples
#' # A country-level indicator observed for most countries, missing for a few
#' codes  <- sub("^s003_", "", colnames(train_output_matrix))
#' set.seed(1)
#' scores <- data.frame(s003 = codes, values = rnorm(length(codes)))
#' scores$values[1:3] <- NA
#'
#' # Fast run on the bundled subsample with a fixed lambda
#' small <- mlcvi:::mlcvi_train_input(small = TRUE)
#' out <- mlcvi_extend(scores, train_input_matrix = small, lambda = 0.1,
#'                     repeats = 1L, verbose = FALSE)
#' head(out)
#' out[out$s003 %in% codes[1:3], ]
#'
#' \dontrun{
#' # Full training data and Bayesian optimisation of lambda (slow)
#' out <- mlcvi_extend(scores)
#' }
#' @seealso [mlcvi_ridge_model()], [mlcvi.get.distance()]
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

  prep <- .mlcvi_prepare_training(country_scores, train_input_matrix,
                                  country_vec, feature_names, feature_weights)
  final_model <- .mlcvi_fit_ridge(prep, lambda, metric, repeats, n_iter,
                                  seed, verbose)

  # predict missing
  all_countries  <- unique(c(country_scores$s003, prep$df_agg$s003))
  missing_codes  <- setdiff(all_countries, prep$observed$s003)
  df_missing <- prep$df_agg[prep$df_agg$s003 %in% missing_codes, , drop = FALSE]

  if (nrow(df_missing) > 0) {
    X_new <- as.matrix(df_missing[, prep$feature_names, drop = FALSE])
    preds <- as.vector(
      stats::predict(final_model, newdata = X_new, type = "raw")
    )
    imputed <- data.frame(s003 = df_missing$s003, values = preds,
                          stringsAsFactors = FALSE)
  } else {
    imputed <- data.frame(s003 = character(0), values = numeric(0),
                          stringsAsFactors = FALSE)
  }

  rbind(
    data.frame(s003 = prep$observed$s003, values = prep$observed$values,
               stringsAsFactors = FALSE),
    imputed
  )
}


#' Train an ML-CVI Ridge model
#'
#' Fits the same weighted Ridge regression used by [mlcvi_extend()] and
#' returns the fitted caret model instead of imputed values.
#'
#' @inheritParams mlcvi_extend
#' @return A train object from the caret package.
#' @examples
#' codes  <- sub("^s003_", "", colnames(train_output_matrix))
#' set.seed(1)
#' scores <- data.frame(s003 = codes, values = rnorm(length(codes)))
#' small  <- mlcvi:::mlcvi_train_input(small = TRUE)
#' fit <- mlcvi_ridge_model(scores, train_input_matrix = small, lambda = 0.1,
#'                          repeats = 1L, verbose = FALSE)
#' fit$results[, c("lambda", "RMSE", "MedAE")]
#' @seealso [mlcvi_extend()]
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

  prep <- .mlcvi_prepare_training(country_scores, train_input_matrix,
                                  country_vec, feature_names, feature_weights)
  .mlcvi_fit_ridge(prep, lambda, metric, repeats, n_iter, seed, verbose)
}


#' Resolve defaults, validate, and aggregate to country level
#'
#' Shared preparation step for [mlcvi_extend()] and [mlcvi_ridge_model()].
#' @return A list with the training design (\code{X_mat}, \code{y_vec}),
#'   the per-feature Ridge penalties, the country-level aggregate
#'   \code{df_agg}, the observed rows of \code{country_scores}, and the
#'   resolved \code{feature_names}.
#' @noRd
.mlcvi_prepare_training <- function(country_scores,
                                    train_input_matrix = NULL,
                                    country_vec        = NULL,
                                    feature_names      = NULL,
                                    feature_weights    = NULL) {
  if (is.null(train_input_matrix)) {
    train_input_matrix <- mlcvi_train_input()
  }
  if (is.null(country_vec)) {
    country_vec <- .default_country_vec(train_input_matrix)
  }
  if (is.null(feature_names)) {
    feature_names <- mlcvi::mlcvi_items_default
  }
  if (is.null(feature_weights)) {
    feature_weights <- mlcvi::mlcvi_weights_default
  }

  if (!is.data.frame(country_scores) ||
      !all(c("s003", "values") %in% names(country_scores))) {
    stop("'country_scores' must be a data frame with columns 's003' and ",
         "'values'.")
  }
  if (length(feature_names) != length(feature_weights)) {
    stop("'feature_names' and 'feature_weights' must have the same length.")
  }
  missing_feat <- setdiff(feature_names, colnames(train_input_matrix))
  if (length(missing_feat) > 0) {
    stop("These feature_names are not columns of train_input_matrix: ",
         paste(missing_feat, collapse = ", "))
  }
  if (length(country_vec) != nrow(train_input_matrix)) {
    stop("'country_vec' must have one entry per row of train_input_matrix.")
  }

  # penalty weights from feature importance
  w_norm  <- feature_weights / sum(feature_weights)
  penalty <- 1 / (w_norm + .Machine$double.eps)

  # aggregate to country level
  observed <- country_scores[!is.na(country_scores$values), , drop = FALSE]
  df_agg   <- .aggregate_to_country(train_input_matrix, country_vec,
                                    feature_names)
  df_train <- merge(df_agg, observed, by = "s003", all.x = TRUE)
  df_train <- df_train[!is.na(df_train$values), , drop = FALSE]

  X_mat <- as.matrix(df_train[, feature_names, drop = FALSE])
  y_vec <- df_train$values
  if (nrow(X_mat) < 5) {
    stop("Too few countries with non-missing values (found ", nrow(X_mat), ").")
  }

  list(X_mat = X_mat, y_vec = y_vec, penalty = penalty, df_agg = df_agg,
       observed = observed, feature_names = feature_names)
}

#' Fit the weighted Ridge model, tuning lambda by Bayesian optimisation
#' unless a fixed lambda is supplied
#' @noRd
.mlcvi_fit_ridge <- function(prep, lambda, metric, repeats, n_iter, seed,
                             verbose) {
  ridge_method <- make_ridge_method(prep$penalty)
  X_mat <- prep$X_mat
  y_vec <- prep$y_vec

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
  caret::train(
    x = X_mat, y = y_vec,
    method = ridge_method,
    trControl = ctrl_final,
    tuneGrid = data.frame(lambda = best_lambda),
    metric = metric,
    maximize = metric_maximize(metric)
  )
}

#' Default country membership for a training matrix
#'
#' Derives the country factor from the one-hot output matrix. When the input
#' matrix is a subsample carrying a \code{row_indices} attribute (as the
#' bundled small matrix does), the output matrix is subset accordingly.
#' @noRd
.default_country_vec <- function(train_input_matrix) {
  tom <- mlcvi::train_output_matrix
  ri  <- attr(train_input_matrix, "row_indices")
  if (!is.null(ri)) {
    tom <- tom[ri, , drop = FALSE]
  }
  if (nrow(tom) != nrow(train_input_matrix)) {
    stop("Cannot derive 'country_vec': train_input_matrix has ",
         nrow(train_input_matrix), " rows but the built-in output matrix has ",
         nrow(tom), ". Supply 'country_vec' explicitly.")
  }
  cidx <- max.col(tom, ties.method = "first")
  factor(colnames(tom)[cidx], levels = colnames(tom))
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

