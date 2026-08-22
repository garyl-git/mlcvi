# Tests are skipped entirely when testthat is not installed, so that checks
# run without suggested packages (for example CRAN's noSuggests checks) do
# not fail here.
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(mlcvi)
  test_check("mlcvi")
}
