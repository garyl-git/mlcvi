y    <- c(1, 2, 3, 4)
yhat <- c(1, 2, 5, 2)

test_that("metric helpers return known values", {
  expect_equal(rmse_(y, yhat), sqrt(mean(c(0, 0, 4, 4))))
  expect_equal(mae_(y, yhat), 1)
  expect_equal(medae_(y, yhat), 1)
  expect_equal(r2_(y, yhat), 1 - 8 / 5)
  # Huber with delta = 1: residuals 0,0,-2,2 -> 0,0,1.5,1.5
  expect_equal(huber_(y, yhat, delta = 1), 0.75)
  # pinball loss at tau = 0.5 is half the MAE
  expect_equal(qloss_(y, yhat, tau = 0.5), 0.5)
})

test_that("metric helpers ignore NA", {
  expect_equal(mae_(c(y, NA), c(yhat, 1)), 1)
  expect_equal(medae_(c(y, NA), c(yhat, 1)), 1)
})

test_that("fancySummary exposes all metrics with the expected names", {
  s <- fancySummary(data.frame(obs = y, pred = yhat))
  expect_named(s, c("RMSE", "MAE", "MedAE", "R2", "Huber", "QLoss_0.5"))
  expect_equal(unname(s["MAE"]), 1)
})

test_that("metric_maximize is TRUE only for R2", {
  expect_true(metric_maximize("R2"))
  expect_false(metric_maximize("MedAE"))
  expect_false(metric_maximize("RMSE"))
})
