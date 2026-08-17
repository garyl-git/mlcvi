ks_manual <- function(data, c1, c2, dims) {
  i1 <- which(data$Country_Code == c1)[1]
  i2 <- which(data$Country_Code == c2)[1]
  s1 <- unlist(data[i1, dims])
  s2 <- unlist(data[i2, dims])
  v  <- sapply(dims, function(d) stats::var(data[[d]], na.rm = TRUE))
  mean((s1 - s2)^2 / v)
}

test_that("KS distance matches the Kogut-Singh formula on bundled data", {
  res <- mlcvi.get.distance("USA", "JPN", method = "KS", verbose = FALSE)
  expect_named(res, c("KS_4dims", "KS_6dims"))
  expect_null(res$KS_4dims$error)
  expect_null(res$KS_6dims$error)
  expect_equal(res$KS_4dims$value,
               ks_manual(Hofstede_dims, "USA", "JPN", .kogut_singh_dims4))
  expect_equal(res$KS_6dims$value,
               ks_manual(Hofstede_dims, "USA", "JPN", .kogut_singh_dims6))
})

test_that("KS distance is symmetric, zero for identical countries, case-insensitive", {
  ab <- mlcvi.get.distance("DEU", "CHN", method = "KS", verbose = FALSE)
  ba <- mlcvi.get.distance("CHN", "DEU", method = "KS", verbose = FALSE)
  expect_equal(ab$KS_6dims$value, ba$KS_6dims$value)

  aa <- mlcvi.get.distance("DEU", "DEU", method = "KS", verbose = FALSE)
  expect_equal(aa$KS_4dims$value, 0)

  lc <- mlcvi.get.distance(" deu", "chn ", method = "ks", verbose = FALSE)
  expect_equal(lc$KS_6dims$value, ab$KS_6dims$value)
})

test_that("KS reports unknown country codes without erroring", {
  res <- mlcvi.get.distance("USA", "XXX", method = "KS", verbose = FALSE)
  expect_true(is.na(res$KS_4dims$value))
  expect_match(res$KS_4dims$error, "country code not found: XXX")
})

test_that("KS reports missing dimensions for a country", {
  # Albania has ltowvs/ivr but no pdi/idv/mas/uai in the bundled table
  res <- mlcvi.get.distance("USA", "ALB", method = "KS", verbose = FALSE)
  expect_true(is.na(res$KS_4dims$value))
  expect_match(res$KS_4dims$error, "missing dimensions")
  expect_match(res$KS_4dims$error, "ALB: pdi")
})

test_that("KS drops zero-variance dimensions with a warning", {
  d <- data.frame(
    Country_Code = c("AAA", "BBB", "CCC"),
    pdi = c(10, 20, 30), idv = c(5, 5, 5), mas = c(1, 2, 4), uai = c(3, 9, 6),
    ltowvs = c(1, 2, 3), ivr = c(3, 2, 1)
  )
  warns <- capture_warnings(
    res <- mlcvi.get.distance("AAA", "BBB", method = "KS", data = d,
                              verbose = FALSE)
  )
  # one warning per dimension set (4-dims and 6-dims both contain idv)
  expect_length(warns, 2)
  expect_match(warns, "dropping dimension\\(s\\) with zero/NA variance: idv",
               all = TRUE)
  expect_equal(res$KS_4dims$value,
               ks_manual(d, "AAA", "BBB", c("pdi", "mas", "uai")))
  expect_equal(res$KS_6dims$value,
               ks_manual(d, "AAA", "BBB", c("pdi", "mas", "uai", "ltowvs", "ivr")))
})

test_that("KS reports missing columns in custom data", {
  d <- data.frame(Country_Code = c("AAA", "BBB"), pdi = 1:2, idv = 2:3)
  res <- mlcvi.get.distance("AAA", "BBB", method = "KS", data = d,
                            verbose = FALSE)
  expect_match(res$KS_4dims$error, "missing columns: mas, uai")
})

test_that("verbose = TRUE prints a report and returns invisibly", {
  expect_output(
    out <- withVisible(mlcvi.get.distance("USA", "JPN", method = "KS")),
    "Kogut-Singh method"
  )
  expect_false(out$visible)
})

test_that("unknown method errors", {
  expect_error(mlcvi.get.distance("USA", "JPN", method = "bogus"),
               "Unknown method")
})
