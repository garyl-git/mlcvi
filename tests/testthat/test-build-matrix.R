small <- mlcvi_train_input(small = TRUE)
items <- mlcvi_items_default[1:6]

# small toy data with known structure: 3 groups x 40 rows, 3 items
make_toy <- function(seed = 1) {
  set.seed(seed)
  g <- rep(c("AAA", "BBB", "CCC"), each = 40)
  x <- cbind(
    i1 = stats::rnorm(120, mean = rep(c(0, 1, 2), each = 40)),
    i2 = stats::rnorm(120, mean = rep(c(5, 5, 6), each = 40)),
    i3 = stats::rnorm(120, mean = rep(c(-1, 0, 1), each = 40))
  )
  list(x = x, g = g)
}

test_that("build_matrix returns a well-formed matrix on bundled data", {
  m <- mlcvi_build_matrix(items, data = small, min_n = 20)
  expect_true(is.matrix(m))
  expect_true(isSymmetric(unclass(m)))
  expect_true(all(diag(m) == 0))
  expect_false(anyNA(m))
  expect_true(all(rownames(m) %in% wvs_country_codes$iso3))
  expect_true(all(c("USA", "JPN", "DEU") %in% rownames(m)))
  expect_equal(dim(attr(m, "items_used")), dim(m))
  expect_true(all(attr(m, "items_used")[upper.tri(m)] == length(items)))
  expect_named(attr(m, "n_per_group"), rownames(m))
  expect_true(all(attr(m, "n_per_group") >= 20))
})

test_that("ks method matches the Kogut-Singh formula on group means", {
  toy <- make_toy()
  m <- mlcvi_build_matrix(c("i1", "i2", "i3"), data = toy$x,
                          country_vec = toy$g, min_n = 10)
  means <- t(sapply(c("AAA", "BBB", "CCC"),
                    function(k) colMeans(toy$x[toy$g == k, ])))
  vars  <- apply(means, 2, stats::var)
  manual <- mean((means["AAA", ] - means["BBB", ])^2 / vars)
  expect_equal(m["AAA", "BBB"], manual)
  expect_equal(m["BBB", "AAA"], manual)
})

test_that("euclidean method matches dist() on group means", {
  toy <- make_toy()
  m <- mlcvi_build_matrix(c("i1", "i2", "i3"), data = toy$x,
                          country_vec = toy$g, method = "euclidean",
                          min_n = 10)
  means <- t(sapply(c("AAA", "BBB", "CCC"),
                    function(k) colMeans(toy$x[toy$g == k, ])))
  expect_equal(unclass(m)[1:3, 1:3], as.matrix(stats::dist(means)),
               ignore_attr = TRUE)
})

test_that("mahalanobis method matches stats::mahalanobis on group means", {
  toy <- make_toy()
  # need more groups than items for a non-singular covariance
  set.seed(2)
  g <- rep(sprintf("G%02d", 1:8), each = 30)
  x <- cbind(i1 = stats::rnorm(240) + rep(stats::rnorm(8), each = 30),
             i2 = stats::rnorm(240) + rep(stats::rnorm(8), each = 30),
             i3 = stats::rnorm(240) + rep(stats::rnorm(8), each = 30))
  m <- mlcvi_build_matrix(c("i1", "i2", "i3"), data = x, country_vec = g,
                          method = "mahalanobis", min_n = 10)
  means <- t(sapply(unique(g), function(k) colMeans(x[g == k, ])))
  S <- stats::cov(means)
  manual <- stats::mahalanobis(means["G01", ], means["G02", ], S)
  expect_equal(m["G01", "G02"], unname(manual))
  expect_equal(m["G02", "G01"], unname(manual))
  expect_true(all(diag(m) == 0))
})

test_that("missing items are renormalized for ks/euclidean and NA for mahalanobis", {
  toy <- make_toy()
  x <- toy$x
  x[toy$g == "AAA", "i3"] <- NA   # item 3 never observed for AAA

  m <- mlcvi_build_matrix(c("i1", "i2", "i3"), data = x,
                          country_vec = toy$g, min_n = 10)
  used <- attr(m, "items_used")
  expect_equal(used["AAA", "BBB"], 2L)
  expect_equal(used["BBB", "CCC"], 3L)
  expect_false(anyNA(m))
  # AAA-BBB computed on i1, i2 only, normalized by 2
  means <- t(sapply(c("AAA", "BBB", "CCC"),
                    function(k) colMeans(x[toy$g == k, ], na.rm = TRUE)))
  vars  <- apply(means, 2, stats::var, na.rm = TRUE)
  manual <- mean((means["AAA", 1:2] - means["BBB", 1:2])^2 / vars[1:2])
  expect_equal(m["AAA", "BBB"], manual)

  # min_items enforced
  m2 <- mlcvi_build_matrix(c("i1", "i2", "i3"), data = x,
                           country_vec = toy$g, min_n = 10, min_items = 3)
  expect_true(is.na(m2["AAA", "BBB"]))
  expect_false(is.na(m2["BBB", "CCC"]))

  mh <- mlcvi_build_matrix(c("i1", "i2", "i3"), data = x,
                           country_vec = toy$g, method = "mahalanobis",
                           min_n = 10)
  expect_true(is.na(mh["AAA", "BBB"]))
  expect_equal(mh["AAA", "AAA"], 0)
})

test_that("small groups are dropped with a message and min_n is enforced", {
  toy <- make_toy()
  g <- toy$g
  g[1:35] <- "TINY"                       # AAA shrinks to 5 rows
  expect_message(
    m <- mlcvi_build_matrix(c("i1", "i2"), data = toy$x, country_vec = g,
                            min_n = 10),
    "Dropping 1 group\\(s\\) with fewer than 10 respondents: AAA"
  )
  expect_false("AAA" %in% rownames(m))
  expect_true("TINY" %in% rownames(m))

  expect_error(
    mlcvi_build_matrix(c("i1", "i2"), data = toy$x, country_vec = g,
                       min_n = 100),
    "Fewer than two groups"
  )
})

test_that("zero-variance items are dropped for ks with a warning", {
  toy <- make_toy()
  x <- cbind(toy$x, flat = 1)
  expect_warning(
    m <- mlcvi_build_matrix(c("i1", "flat"), data = x, country_vec = toy$g,
                            min_n = 10),
    "zero/NA variance across groups: flat"
  )
  ref <- mlcvi_build_matrix("i1", data = x, country_vec = toy$g, min_n = 10)
  expect_equal(unclass(m), unclass(ref), ignore_attr = TRUE)
})

test_that("by_wave returns one matrix per wave and skips thin waves", {
  expect_message(
    bw <- mlcvi_build_matrix(items, data = small, by_wave = TRUE, min_n = 10),
    "Skipping wave 1"
  )
  expect_type(bw, "list")
  expect_true(all(names(bw) %in% as.character(1:6)))
  expect_false("1" %in% names(bw))
  for (m in bw) {
    expect_true(isSymmetric(unclass(m)))
    expect_true(all(diag(m) == 0))
  }

  # explicit wave_vec on toy data
  toy <- make_toy()
  wv <- rep(c(3L, 5L), length.out = 120)
  bw2 <- mlcvi_build_matrix(c("i1", "i2"), data = toy$x, country_vec = toy$g,
                            wave_vec = wv, by_wave = TRUE, min_n = 10)
  expect_named(bw2, c("3", "5"))
  ref3 <- mlcvi_build_matrix(c("i1", "i2"), data = toy$x[wv == 3L, ],
                             country_vec = toy$g[wv == 3L], min_n = 10)
  expect_equal(bw2[["3"]], ref3)
})

test_that(".default_wave_vec reads the s002 indicators", {
  wv <- .default_wave_vec(small)
  expect_length(wv, nrow(small))
  expect_true(all(wv %in% 1:6))
  expect_equal(as.vector(table(wv)),
               unname(colSums(small[, paste0("s002_", 1:6)])))
  expect_error(.default_wave_vec(small[, 10:20]), "no s002_<wave> columns")
})

test_that(".wvs_to_iso3 maps codes and passes unknown values through", {
  expect_equal(.wvs_to_iso3(c("s003_840", "s003_392", "s003_276")),
               c("USA", "JPN", "DEU"))
  expect_equal(.wvs_to_iso3(c(840, 392)), c("USA", "JPN"))
  expect_equal(.wvs_to_iso3(c("s003_840", "ZZZ")), c("USA", "ZZZ"))
})

test_that("build_matrix output plugs into the lookup functions", {
  m <- mlcvi_build_matrix(items, data = small, min_n = 20)
  r <- mlcvi.get.distance("USA", "JPN", method = "MLCVI", data = m,
                          verbose = FALSE)
  expect_equal(r$MLCVI$value, m["USA", "JPN"])
  p <- mlcvi_get_panel(data.frame(iso1 = "USA", iso2 = c("JPN", "DEU")),
                       method = "MLCVI", data = m)
  expect_equal(p$distance, m["USA", c("JPN", "DEU")], ignore_attr = TRUE)
})

test_that("build_matrix input validation", {
  expect_error(mlcvi_build_matrix(character(0), data = small),
               "non-empty character vector")
  expect_error(mlcvi_build_matrix(c("a001", "ghost"), data = small),
               "not columns of 'data': ghost")
  expect_error(mlcvi_build_matrix("a001", data = as.data.frame(small[1:5, 1:5])),
               "numeric matrix")
  expect_error(mlcvi_build_matrix("a001", data = small, min_n = 0),
               "'min_n' must be")
  expect_error(mlcvi_build_matrix("a001", data = small, min_items = 0),
               "'min_items' must be")
  expect_error(mlcvi_build_matrix("a001", data = small,
                                  country_vec = c("A", "B")),
               "one entry per row")
  expect_error(mlcvi_build_matrix("a001", data = small, by_wave = TRUE,
                                  wave_vec = 1:3),
               "'wave_vec' must have one entry per row")
})
