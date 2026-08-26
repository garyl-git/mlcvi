toy_obs <- function() {
  d <- expand.grid(iso3 = c("AAA", "BBB", "CCC"), year = c(2000, 2005, 2010),
                   stringsAsFactors = FALSE)
  d$value <- c(10, 20, 30, 12, 22, 33, 14, 24, 36)
  d
}
toy_anchor <- function(years = 1998:2014) {
  a <- expand.grid(iso3 = c("AAA", "BBB", "CCC"), year = years,
                   stringsAsFactors = FALSE)
  a$x <- with(a, match(iso3, c("AAA", "BBB", "CCC")) * 10 + (year - 2000) * 0.5)
  a
}

test_that("noise-free anchor relation is recovered exactly", {
  a <- toy_anchor()
  obs <- merge(toy_obs()[, c("iso3", "year")], a)
  obs$value <- 3 + 2 * obs$x + c(AAA = 0, BBB = 5, CCC = -4)[obs$iso3]
  out <- mlcvi_extend_years(obs, anchors = a, years = 1998:2014)
  expect_equal(unname(attr(out, "anchor_coef")["x"]), 2, tolerance = 1e-8)
  truth <- 3 + 2 * a$x + c(AAA = 0, BBB = 5, CCC = -4)[a$iso3]
  got <- out$value[match(paste(a$iso3, a$year), paste(out$iso3, out$year))]
  expect_equal(got, unname(truth), tolerance = 1e-8)
  # includes true extrapolation years outside 2000-2010
  expect_true(all(out$imputed[out$year %in% c(1998, 2014)]))
})

test_that("observed years keep their observed values exactly", {
  obs <- toy_obs()
  out <- mlcvi_extend_years(obs, anchors = toy_anchor())
  m <- merge(obs, out, by = c("iso3", "year"))
  expect_equal(m$value.x, m$value.y)
  expect_false(any(out$imputed[paste(out$iso3, out$year) %in%
                                 paste(obs$iso3, obs$year)]))
})

test_that("without anchors the method is interpolation with end-carry", {
  obs <- toy_obs()
  out <- mlcvi_extend_years(obs, years = 1998:2012)
  aaa <- out[out$iso3 == "AAA", ]   # AAA observes 10 (2000), 12 (2005), 14 (2010)
  expect_equal(aaa$value[aaa$year == 2002], 10 + (12 - 10) * 2 / 5)
  expect_equal(aaa$value[aaa$year == 1998], 10)   # carry before first obs
  expect_equal(aaa$value[aaa$year == 2012], 14)   # carry after last obs
  expect_null(attr(out, "anchor_coef"))
})

test_that("shape, flags, and attributes are as documented", {
  out <- mlcvi_extend_years(toy_obs(), anchors = toy_anchor(), years = 1999:2011)
  expect_named(out, c("iso3", "year", "value", "imputed"))
  expect_equal(nrow(out), 3 * length(1999:2011))
  expect_equal(sum(!out$imputed), 9)
  expect_identical(attr(out, "dropped"), character(0))
})

test_that("countries below min_obs are dropped with a message", {
  obs <- rbind(toy_obs(), data.frame(iso3 = "DDD", year = 2005, value = 1))
  expect_message(out <- mlcvi_extend_years(obs, anchors = toy_anchor()),
                 "Dropping 1 country with fewer than 2 observed years: DDD")
  expect_false("DDD" %in% out$iso3)
  expect_identical(attr(out, "dropped"), "DDD")
})

test_that("anchor years missing for a country are carried from the nearest year", {
  a <- toy_anchor(2000:2010)             # anchors end in 2010
  out <- mlcvi_extend_years(toy_obs(), anchors = a, years = 2000:2014)
  ccc <- out[out$iso3 == "CCC", ]
  # beyond both data and anchors, the series is flat (carry + flat anchor)
  expect_equal(ccc$value[ccc$year == 2014], ccc$value[ccc$year == 2010])
})

test_that("input validation errors are informative", {
  expect_error(mlcvi_extend_years(list(1)), "must be a data frame")
  expect_error(mlcvi_extend_years(toy_obs(), country = "nope"),
               "Column 'nope' not found in 'data'")
  expect_error(mlcvi_extend_years(toy_obs(), min_obs = 1), "at least 2")
  dup <- rbind(toy_obs(), toy_obs()[1, ])
  expect_error(mlcvi_extend_years(dup), "more than one value")
  bad_a <- toy_anchor(); bad_a$x <- as.character(bad_a$x)
  expect_error(mlcvi_extend_years(toy_obs(), anchors = bad_a),
               "no numeric anchor columns")
  expect_error(mlcvi_extend_years(toy_obs(), anchors = toy_anchor()[, -1]),
               "Column 'iso3' not found in 'anchors'")
})

test_that("too little anchor overlap falls back to no anchors, with a message", {
  a <- toy_anchor(2020:2024)             # no overlap with observed years
  expect_message(out <- mlcvi_extend_years(toy_obs(), anchors = a),
                 "Too few country-years match")
  expect_null(attr(out, "anchor_coef"))
})
