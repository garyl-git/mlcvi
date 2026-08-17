small <- mlcvi_train_input(small = TRUE)

# Country-level target: observed for most countries, NA for a few
make_scores <- function(n_missing = 3, seed = 1) {
  set.seed(seed)
  codes  <- sub("^s003_", "", colnames(train_output_matrix))
  values <- stats::rnorm(length(codes))
  values[seq_len(n_missing)] <- NA
  data.frame(s003 = codes, values = values, stringsAsFactors = FALSE)
}

test_that("mlcvi_train_input loads and caches the small matrix", {
  expect_true(is.matrix(small))
  expect_equal(dim(small), c(2450L, 594L))
  expect_true(all(mlcvi_items_default %in% colnames(small)))
  expect_length(attr(small, "row_indices"), 2450L)
  expect_identical(mlcvi_train_input(small = TRUE), small)
  expect_error(mlcvi_train_input(small = NA), "single non-missing logical")
  expect_error(mlcvi_train_input(small = "yes"), "single non-missing logical")
})

test_that(".default_country_vec honours row_indices and validates length", {
  cv <- .default_country_vec(small)
  expect_s3_class(cv, "factor")
  expect_length(cv, nrow(small))
  expect_equal(nlevels(cv), ncol(train_output_matrix))
  expect_true(all(table(droplevels(cv)) == 25))

  bad <- small[1:10, ]
  attr(bad, "row_indices") <- NULL
  expect_error(.default_country_vec(bad), "Cannot derive 'country_vec'")
})

test_that(".aggregate_to_country returns one row per country with clean codes", {
  cv  <- .default_country_vec(small)
  agg <- .aggregate_to_country(small, cv, mlcvi_items_default[1:3])
  expect_equal(nrow(agg), nlevels(cv))
  expect_false(any(grepl("^s003_", agg$s003)))
  expect_named(agg, c("s003", mlcvi_items_default[1:3]))
  one <- agg$s003[1]
  expect_equal(
    agg[[mlcvi_items_default[1]]][1],
    mean(small[cv == paste0("s003_", one), mlcvi_items_default[1]], na.rm = TRUE)
  )
})

test_that("mlcvi_extend imputes missing countries and preserves observed ones", {
  scores <- make_scores(n_missing = 3)
  out <- mlcvi_extend(scores, train_input_matrix = small, lambda = 0.1,
                      repeats = 1L, verbose = FALSE)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("s003", "values"))
  expect_setequal(out$s003, scores$s003)
  expect_false(anyNA(out$values))

  obs <- scores[!is.na(scores$values), ]
  expect_equal(out$values[match(obs$s003, out$s003)], obs$values)

  imputed <- out[out$s003 %in% scores$s003[is.na(scores$values)], ]
  expect_equal(nrow(imputed), 3)
  expect_true(all(is.finite(imputed$values)))
})

test_that("mlcvi_extend is deterministic under a fixed seed", {
  scores <- make_scores(n_missing = 2)
  a <- mlcvi_extend(scores, train_input_matrix = small, lambda = 0.1,
                    repeats = 1L, verbose = FALSE)
  b <- mlcvi_extend(scores, train_input_matrix = small, lambda = 0.1,
                    repeats = 1L, verbose = FALSE)
  expect_equal(a, b)
})

test_that("mlcvi_extend errors with too few observed countries", {
  scores <- make_scores(n_missing = 0)
  scores$values[-(1:4)] <- NA
  expect_error(
    mlcvi_extend(scores, train_input_matrix = small, lambda = 0.1,
                 repeats = 1L, verbose = FALSE),
    "Too few countries"
  )
})

test_that("mlcvi_ridge_model returns a caret train object with the given lambda", {
  scores <- make_scores(n_missing = 3)
  fit <- mlcvi_ridge_model(scores, train_input_matrix = small, lambda = 0.1,
                           repeats = 1L, verbose = FALSE)
  expect_s3_class(fit, "train")
  expect_equal(fit$bestTune$lambda, 0.1)
  expect_equal(nrow(fit$trainingData), sum(!is.na(scores$values)))
  expect_true(all(c("MedAE", "RMSE") %in% names(fit$results)))
})

test_that("make_ridge_method builds a caret method list", {
  m <- make_ridge_method(rep(1, 3))
  expect_named(m, c("type", "library", "loop", "parameters", "grid", "fit",
                    "predict", "prob"))
  expect_equal(m$library, "glmnet")
  expect_equal(m$grid(NULL, NULL)$lambda, 0.01)
})
