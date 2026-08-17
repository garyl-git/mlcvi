test_that(".clean_iso3 upper-cases and strips whitespace", {
  expect_identical(.clean_iso3(" usa "), "USA")
  expect_identical(.clean_iso3("j p n"), "JPN")
  expect_identical(.clean_iso3(c("deu", "\tCHN\n")), c("DEU", "CHN"))
})

test_that(".clean_iso3_dimnames cleans both dimensions", {
  m <- matrix(1:4, 2, dimnames = list(c(" usa", "jpn "), c("usa ", " jpn")))
  out <- .clean_iso3_dimnames(m)
  expect_identical(rownames(out), c("USA", "JPN"))
  expect_identical(colnames(out), c("USA", "JPN"))
})

test_that(".clean_iso3_maybe falls back to the same behaviour", {
  expect_identical(.clean_iso3_maybe(" usa"), .clean_iso3(" usa"))
})
