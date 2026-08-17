test_that("mediator screen on MLCVI_4A returns the documented structure", {
  out <- mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg")
  expect_named(out, c("results", "significant_items", "meta"))
  res <- out$results
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 60)
  expect_true(all(c("mediator", "n", "a", "a_se", "b", "b_se", "indirect",
                    "sobel_z", "sobel_p", "p_adj", "c_total", "c_prime",
                    "prop_mediated") %in% names(res)))
  expect_false(is.unsorted(res$p_adj))
  n_expected <- vapply(res$mediator, function(m) {
    sum(stats::complete.cases(MLCVI_4A[, c("US0IN1", "envavg", m)]))
  }, integer(1))
  expect_equal(res$n, unname(n_expected))
  expect_equal(out$meta$n_mediators, 60)
  expect_setequal(out$significant_items, res$mediator[res$p_adj <= 0.05])
})

test_that("Sobel statistics match a manual computation", {
  out <- mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg")
  row <- out$results[out$results$mediator == "mlcvi1", ]

  m_a  <- stats::lm(mlcvi1 ~ US0IN1, data = MLCVI_4A)
  m_bc <- stats::lm(envavg ~ mlcvi1 + US0IN1, data = MLCVI_4A)
  m_c  <- stats::lm(envavg ~ US0IN1, data = MLCVI_4A)
  a  <- coef(summary(m_a))["US0IN1", 1];  sa <- coef(summary(m_a))["US0IN1", 2]
  b  <- coef(summary(m_bc))["mlcvi1", 1]; sb <- coef(summary(m_bc))["mlcvi1", 2]
  z  <- (a * b) / sqrt(b^2 * sa^2 + a^2 * sb^2)

  expect_equal(row$a, a)
  expect_equal(row$b, b)
  expect_equal(row$indirect, a * b)
  expect_equal(row$sobel_z, z)
  expect_equal(row$sobel_p, 2 * stats::pnorm(-abs(z)))
  expect_equal(row$c_total, coef(m_c)["US0IN1"], ignore_attr = TRUE)
  expect_equal(row$c_prime, coef(m_bc)["US0IN1"], ignore_attr = TRUE)
  expect_equal(row$prop_mediated, (a * b) / coef(m_c)["US0IN1"],
               ignore_attr = TRUE)
})

test_that("abc fallback is used on MLCVI_3A with a message", {
  expect_message(
    out <- mlcvi.get.mediator(df = MLCVI_3A, iv = "US0Mex1", dv = "covidavg"),
    "Using 'abc1..60' columns as mediators"
  )
  expect_equal(nrow(out$results), 60)
  expect_true(all(grepl("^abc", out$meta$matched_names)))
})

test_that("explicit mediator_names bypasses detection", {
  out <- mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg",
                            mediator_names = c("mlcvi1", "mlcvi2"),
                            require_exact_count = FALSE)
  expect_equal(out$results$mediator[order(out$results$mediator)],
               c("mlcvi1", "mlcvi2"))
})

test_that("input validation errors are informative", {
  expect_error(mlcvi.get.mediator(iv = "x", dv = "y"), "Provide either")
  expect_error(mlcvi.get.mediator(df = MLCVI_4A, iv = "nope", dv = "envavg"),
               "IV 'nope' is not present")
  expect_error(mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "nope"),
               "DV 'nope' is not present")
  expect_error(mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg",
                                  mediator_names = c("mlcvi1", "ghost")),
               "missing: ghost")
  expect_error(mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg",
                                  expect_n = 10),
               "Detected 60 mediator columns; expected 10")
  d <- MLCVI_4A[, c("US0IN1", "envavg")]
  expect_error(mlcvi.get.mediator(df = d, iv = "US0IN1", dv = "envavg"),
               "No mediator columns detected")
})

test_that("a two-level character IV is recoded to 0/1 deterministically", {
  d <- MLCVI_4A
  d$grp <- ifelse(d$US0IN1 == 1, "india", "usa")
  expect_message(
    out <- mlcvi.get.mediator(df = d, iv = "grp", dv = "envavg"),
    "recoded to 0/1 \\(india → 0, usa → 1\\)"
  )
  ref <- mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg")
  # 'usa' -> 1 flips the sign of a relative to US0IN1 (india = 1)
  expect_equal(out$results$a[match("mlcvi1", out$results$mediator)],
               -ref$results$a[match("mlcvi1", ref$results$mediator)])

  d$grp3 <- sample(c("a", "b", "c"), nrow(d), replace = TRUE)
  expect_error(mlcvi.get.mediator(df = d, iv = "grp3", dv = "envavg"),
               "exactly two unique values")
})

test_that("na_action = complete.cases drops incomplete rows per mediator", {
  d <- MLCVI_4A
  d$mlcvi1[1:5] <- NA
  out <- mlcvi.get.mediator(df = d, iv = "US0IN1", dv = "envavg")
  expect_equal(out$results$n[out$results$mediator == "mlcvi1"], nrow(d) - 5)
  expect_equal(out$results$n[out$results$mediator == "mlcvi2"], nrow(d))
})

test_that("Sobel returns NA rather than NaN for a constant mediator", {
  d <- MLCVI_4A
  d$mlcvi1 <- 3
  out <- suppressWarnings(
    mlcvi.get.mediator(df = d, iv = "US0IN1", dv = "envavg")
  )
  row <- out$results[out$results$mediator == "mlcvi1", ]
  expect_true(is.na(row$sobel_z))
  expect_true(is.na(row$sobel_p))
})
