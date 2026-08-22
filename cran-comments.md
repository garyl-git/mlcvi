## Test environments

* local macOS 15 (arm64), R 4.5.1: `R CMD check --as-cran` with PDF/HTML
  manual and CRAN incoming checks
* R-hub v2: linux (Ubuntu, R-devel), macos-arm64 (R-devel),
  windows (R-devel), plus the `donttest` and `nosuggests` configurations
* win-builder R-release (R 4.6.1, 2026-08-22): 0 errors, 0 warnings,
  1 NOTE (New submission; possibly misspelled words in DESCRIPTION, see
  below)
* win-builder R-devel: TODO -- paste the result from the second email

## R CMD check results

0 errors | 0 warnings | 0 notes locally and on all three R-hub platforms.
The R-hub `nosuggests` configuration fails only at re-building the
vignette because rmarkdown is not installed there; examples and tests pass
without any suggested package.

On CRAN incoming checks we expect one NOTE: "New submission", together
with "Possibly misspelled words in DESCRIPTION" (win-builder lists CVI,
Guillen, Hofstede's, Kogut, Leng, Sheetal, Shulgin, Sobel, Zhou,
importances, lookups). These are author names, the names of the cited
methods, and standard terms, spelled as in the cited literature.

## Internet resources and user file space

The package's imputation and matrix-construction functions can use a large
training matrix (about 280 MB) that is not included in the package. It is
distributed from the authors' OSF project (https://osf.io/3csbz/) and fetched
by `mlcvi_training_data()` only when a user calls one of those functions
without supplying their own data. No example, test, or vignette triggers the
download: all of them use a 2.4 MB subsample that ships inside the package.

The download fails gracefully, as the policy requires: if the resource is
unreachable or its checksum no longer matches, the function returns NULL
with an informative message and no warning or error.

Nothing is written outside the session's temporary directory without
consent. A downloaded file is kept under `tempdir()` by default. In an
interactive session the user is asked once whether to keep it under
`tools::R_user_dir("mlcvi", "cache")` across sessions; non-interactive
sessions never write there unless the user has opted in explicitly via
`options(mlcvi.cache = "persistent")` or the `MLCVI_CACHE` environment
variable. `mlcvi_clear_cache()` removes the file. The package therefore
depends on R >= 4.0.0 for `tools::R_user_dir()`.

## Function names

Two exported functions use dotted names (`mlcvi.get.distance`,
`mlcvi.get.mediator`). They are retained unchanged because they appear in
the package's published user guide (Leng & Sheetal 2025,
<doi:10.31235/osf.io/73g8z_v1>) and in users' existing code; all functions
added since use snake_case.
