
# internal metric helpers

rmse_ <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

mae_ <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)

medae_ <- function(y, yhat) stats::median(abs(y - yhat), na.rm = TRUE)

r2_ <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2, na.rm = TRUE)
  ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

huber_ <- function(y, yhat, delta = 1) {
  e  <- y - yhat
  ae <- abs(e)
  quad <- ae <= delta
  mean(ifelse(quad, 0.5 * e^2, delta * (ae - 0.5 * delta)), na.rm = TRUE)
}

qloss_ <- function(y, yhat, tau = 0.5) {
  e <- y - yhat
  mean((tau - (e < 0)) * e, na.rm = TRUE)
}

#' Multi-metric summary function for caret
#' @noRd
fancySummary <- function(data, lev = NULL, model = NULL) {
  y    <- data$obs
  yhat <- data$pred
  c(
    RMSE      = rmse_(y, yhat),
    MAE       = mae_(y, yhat),
    MedAE     = medae_(y, yhat),
    R2        = r2_(y, yhat),
    Huber     = huber_(y, yhat, delta = 1.0),
    QLoss_0.5 = qloss_(y, yhat, tau = 0.5)
  )
}

#' Is this metric higher-is-better?
#' @noRd
metric_maximize <- function(metric) startsWith(metric, "R2")

