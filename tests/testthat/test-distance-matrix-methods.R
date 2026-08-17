test_that("Shulgin lookup returns the [from, to] cell of the bundled table", {
  res <- mlcvi.get.distance("USA", "JPN", method = "Shulgin", verbose = FALSE)
  expect_named(res, "Shulgin")
  expect_null(res$Shulgin$error)
  expect_equal(res$Shulgin$value, Shulgin_csv["USA", "JPN"])
  # the bundled table is directional: [from, to] is looked up as given
  rev <- mlcvi.get.distance("JPN", "USA", method = "Shulgin", verbose = FALSE)
  expect_equal(rev$Shulgin$value, Shulgin_csv["JPN", "USA"])
  lc <- mlcvi.get.distance(" usa", "jpn ", method = "shulgin", verbose = FALSE)
  expect_equal(lc$Shulgin$value, res$Shulgin$value)
})

test_that("Shulgin reports unknown codes and missing cells", {
  res <- mlcvi.get.distance("USA", "XXX", method = "Shulgin", verbose = FALSE)
  expect_true(is.na(res$Shulgin$value))
  expect_match(res$Shulgin$error, "country code not found: XXX")

  d <- Shulgin_csv[1:3, 1:3]
  d[1, 2] <- NA
  res <- mlcvi.get.distance(rownames(d)[1], colnames(d)[2], method = "Shulgin",
                            data = d, verbose = FALSE)
  expect_match(res$Shulgin$error, "missing value")
})

test_that("MLCVI lookup returns the bundled matrix entry and is symmetric", {
  res <- mlcvi.get.distance("USA", "JPN", method = "MLCVI", verbose = FALSE)
  expect_named(res, "MLCVI")
  expect_null(res$MLCVI$error)
  expect_equal(res$MLCVI$value, MLCVI_distance_matrix["USA", "JPN"])
  rev <- mlcvi.get.distance("jpn", "usa", method = "mlcvi", verbose = FALSE)
  expect_equal(rev$MLCVI$value, res$MLCVI$value)
  self <- mlcvi.get.distance("USA", "USA", method = "MLCVI", verbose = FALSE)
  expect_equal(self$MLCVI$value, 0)
})

test_that("MLCVI accepts dist and data.frame inputs and validates dimnames", {
  m <- MLCVI_distance_matrix[1:4, 1:4]
  cn <- colnames(m)
  as_dist <- stats::as.dist(m)
  r1 <- mlcvi.get.distance(cn[1], cn[3], method = "MLCVI", data = as_dist,
                           verbose = FALSE)
  expect_equal(r1$MLCVI$value, m[1, 3])

  r2 <- mlcvi.get.distance(cn[1], cn[3], method = "MLCVI",
                           data = as.data.frame(m), verbose = FALSE)
  expect_equal(r2$MLCVI$value, m[1, 3])

  bare <- unname(m)
  r3 <- mlcvi.get.distance(cn[1], cn[3], method = "MLCVI", data = bare,
                           verbose = FALSE)
  expect_match(r3$MLCVI$error, "must have row and column names")

  r4 <- mlcvi.get.distance(cn[1], "ZZZ", method = "MLCVI", verbose = FALSE)
  expect_match(r4$MLCVI$error, "country code not found \\(col\\): ZZZ")
})

test_that("bundled distance matrices are well formed", {
  expect_true(isSymmetric(MLCVI_distance_matrix))
  expect_true(all(diag(MLCVI_distance_matrix) == 0))
  expect_false(anyNA(MLCVI_distance_matrix))
  expect_identical(rownames(Shulgin_csv), colnames(Shulgin_csv))
})

test_that("Shulgin and MLCVI verbose reports print", {
  expect_output(mlcvi.get.distance("USA", "JPN", method = "Shulgin"),
                "Shulgin method")
  expect_output(mlcvi.get.distance("USA", "JPN", method = "MLCVI"),
                "MLCVI method")
})
