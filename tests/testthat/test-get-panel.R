pairs <- data.frame(
  iso1 = c("USA", "USA", "DEU", "USA", " usa", "ALB"),
  iso2 = c("JPN", "CHN", "CHN", "XXX", "jpn ", "USA"),
  year = 2001:2006,
  stringsAsFactors = FALSE
)

scalar <- function(k, method, field) {
  mlcvi.get.distance(pairs$iso1[k], pairs$iso2[k], method = method,
                     verbose = FALSE)[[field]]$value
}

test_that("panel preserves rows, order, and input columns", {
  out <- mlcvi_get_panel(pairs, method = "MLCVI")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(pairs))
  expect_named(out, c(names(pairs), "distance", "distance_note"))
  expect_identical(out[, names(pairs)], pairs)
  expect_type(out$distance, "double")
  expect_type(out$distance_note, "character")
})

test_that("panel MLCVI values equal the scalar lookups", {
  out <- mlcvi_get_panel(pairs, method = "MLCVI")
  for (k in c(1, 2, 3, 6)) {
    expect_equal(out$distance[k], scalar(k, "MLCVI", "MLCVI"))
  }
  expect_true(is.na(out$distance[4]))
  expect_match(out$distance_note[4], "country code not found: XXX")
  # case and whitespace insensitive
  expect_equal(out$distance[5], out$distance[1])
  expect_true(all(is.na(out$distance_note[-4])))
})

test_that("panel KS values equal the scalar lookups for 6 and 4 dims", {
  out6 <- mlcvi_get_panel(pairs, method = "KS")
  out4 <- mlcvi_get_panel(pairs, method = "KS", ks_dims = "4dims")
  for (k in 1:3) {
    expect_equal(out6$distance[k], scalar(k, "KS", "KS_6dims"))
    expect_equal(out4$distance[k], scalar(k, "KS", "KS_4dims"))
  }
  expect_match(out6$distance_note[4], "country code not found: XXX")
  # Albania lacks the four classic dimensions
  expect_true(is.na(out6$distance[6]))
  expect_match(out6$distance_note[6], "missing dimensions -> ALB: pdi, idv, mas, uai")
})

test_that("panel Shulgin values equal the scalar lookups", {
  out <- mlcvi_get_panel(pairs, method = "Shulgin")
  for (k in 1:3) {
    expect_equal(out$distance[k], scalar(k, "Shulgin", "Shulgin"))
  }
  expect_match(out$distance_note[6], "country code not found: ALB")
})

test_that("panel is vectorized over many rows and matches scalar calls", {
  set.seed(7)
  cn <- rownames(MLCVI_distance_matrix)
  big <- data.frame(iso1 = sample(cn, 500, TRUE), iso2 = sample(cn, 500, TRUE))
  out <- mlcvi_get_panel(big, method = "MLCVI")
  expect_false(anyNA(out$distance))
  expect_equal(out$distance, MLCVI_distance_matrix[cbind(big$iso1, big$iso2)])
})

test_that("custom column names, custom data, and no year column work", {
  trade <- data.frame(origin = "USA", dest = c("JPN", "DEU"))
  out <- mlcvi_get_panel(trade, method = "MLCVI", iso1 = "origin",
                         iso2 = "dest", data = MLCVI_distance_matrix)
  expect_equal(out$distance, MLCVI_distance_matrix["USA", c("JPN", "DEU")],
               ignore_attr = TRUE)

  m <- MLCVI_distance_matrix[1:3, 1:3]
  m[1, 2] <- NA
  out <- mlcvi_get_panel(
    data.frame(iso1 = rownames(m)[1], iso2 = colnames(m)[2]),
    method = "MLCVI", data = m
  )
  expect_true(is.na(out$distance))
  expect_match(out$distance_note, "missing value for")

  out <- mlcvi_get_panel(pairs[1:2, ], method = "MLCVI",
                         data = stats::as.dist(MLCVI_distance_matrix))
  expect_equal(out$distance[1], MLCVI_distance_matrix["USA", "JPN"])
})

test_that("panel input validation", {
  expect_error(mlcvi_get_panel(list(a = 1)), "must be a data frame")
  expect_error(mlcvi_get_panel(pairs, iso1 = "nope"),
               "Column 'nope' not found")
  expect_error(mlcvi_get_panel(pairs, method = "bogus"))
  expect_error(mlcvi_get_panel(pairs, method = "MLCVI", rule = "wave"),
               "not yet available")
  expect_error(mlcvi_get_panel(pairs[, c("iso1", "iso2")], rule = "locf"),
               "Column 'year' is required")
  expect_error(mlcvi_get_panel(pairs, method = "MLCVI",
                               data = unname(MLCVI_distance_matrix)),
               "must have row and column names")
  expect_error(mlcvi_get_panel(pairs, method = "MLCVI", data = 1:3),
               "must be a matrix, dist, or data.frame")
})

test_that("panel KS reports missing columns and zero-variance dimensions", {
  d <- data.frame(Country_Code = c("AAA", "BBB"), pdi = 1:2, idv = 2:3)
  out <- mlcvi_get_panel(data.frame(iso1 = "AAA", iso2 = "BBB"),
                         method = "KS", data = d)
  expect_match(out$distance_note, "missing columns: mas, uai")

  d <- data.frame(
    Country_Code = c("AAA", "BBB", "CCC"),
    pdi = c(10, 20, 30), idv = c(5, 5, 5), mas = c(1, 2, 4), uai = c(3, 9, 6),
    ltowvs = c(1, 2, 3), ivr = c(3, 2, 1)
  )
  expect_warning(
    out <- mlcvi_get_panel(data.frame(iso1 = "AAA", iso2 = "BBB"),
                           method = "KS", data = d),
    "dropping dimension\\(s\\) with zero/NA variance: idv"
  )
  ref <- suppressWarnings(
    mlcvi.get.distance("AAA", "BBB", method = "KS", data = d, verbose = FALSE)
  )
  expect_equal(out$distance, ref$KS_6dims$value)
})

test_that("panel never prints", {
  expect_silent(mlcvi_get_panel(pairs[1:3, ], method = "MLCVI"))
  expect_silent(mlcvi_get_panel(pairs[1:3, ], method = "KS"))
})

test_that("method = 'matrix' looks up a user-supplied table and requires data", {
  m <- MLCVI_distance_matrix[1:5, 1:5]
  cn <- colnames(m)
  p  <- data.frame(iso1 = cn[1], iso2 = c(cn[2], cn[3], "ZZZ"))
  out <- mlcvi_get_panel(p, method = "matrix", data = m)
  expect_equal(out$distance[1:2], m[cn[1], cn[2:3]], ignore_attr = TRUE)
  expect_true(is.na(out$distance[3]))
  expect_match(out$distance_note[3], "country code not found: ZZZ")
  # identical to the MLCVI path on the same table
  ref <- mlcvi_get_panel(p, method = "MLCVI", data = m)
  expect_equal(out$distance, ref$distance)
  expect_error(mlcvi_get_panel(p, method = "matrix"),
               "requires a square table in 'data'")
  # a build_matrix result plugs in directly
  small <- mlcvi_train_input(small = TRUE)
  b <- mlcvi_build_matrix(mlcvi_items_default[1:5], data = small, min_n = 20)
  q <- data.frame(iso1 = "USA", iso2 = c("JPN", "DEU"))
  out2 <- mlcvi_get_panel(q, method = "matrix", data = b)
  expect_equal(out2$distance, b["USA", c("JPN", "DEU")], ignore_attr = TRUE)
})
