## Test environments

TODO (filled in by the pre-flight run; see dev/cran-preflight.md for the
raw results):

* local: macOS (Darwin 24.4), R 4.5.x -- `R CMD check --as-cran`: 0 errors,
  0 warnings, 0 notes
* rhub v2: linux (R-devel), macos (R-release), windows (R-release / R-devel)
* win-builder: R-devel and R-release

## R CMD check results

0 errors | 0 warnings | 0 notes locally.

On CRAN incoming checks we expect one NOTE, "New submission", and possibly
"Possibly misspelled words in DESCRIPTION" for the proper nouns Hofstede,
Kogut, Shulgin, Guillen and the abbreviation ML-CVI, which are spelled as in
the cited literature.

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
