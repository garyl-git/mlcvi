# mlcvi 1.0.0

First CRAN release. Version 1.0.0 marks the API as stable: the two original
function names (`mlcvi.get.distance()`, `mlcvi.get.mediator()`) are kept for
compatibility with the published user guide; all newer functions use
snake_case.

## New functions

- `mlcvi_training_data()` is the documented way to obtain the ML-CVI
  training matrix: `small = TRUE` returns the bundled subsample, the default
  fetches the full matrix (about 280 MB) from OSF on first use. The download
  is verified against a checksum and, if it fails, the function returns
  `NULL` with a message instead of an error. Where the file is kept is
  opt-in: the current session's temporary directory by default, or the user
  cache directory after interactive consent or `options(mlcvi.cache =
  "persistent")`. `mlcvi_clear_cache()` removes it again.

- `mlcvi_get_panel()` looks up cultural distances for a whole data frame of
  country pairs at once (for merging into dyadic panel data). Unresolvable
  rows get `NA` plus a `distance_note` instead of an error, and the input
  rows and order are preserved. The distance tables are time-invariant by
  design, so the same distance applies to every year of a panel.
- `mlcvi_build_matrix()` constructs classical distance matrices
  (Kogut-Singh style, Euclidean, Mahalanobis) from a user-specified WVS item
  set, at country level or one matrix per WVS wave, with renormalization
  over the items available to each pair. Output plugs directly into
  `mlcvi.get.distance()` and `mlcvi_get_panel()` as `data`.
- New dataset `wvs_country_codes` maps the WVS numeric country codes of the
  training sample to ISO alpha-3 codes.
- `mlcvi.get.distance()` and `mlcvi_get_panel()` accept `method = "matrix"`
  to look up a user-supplied square table (such as the output of
  `mlcvi_build_matrix()`) without attaching a method label; the ML-CVI
  label stays with the packaged matrix.

## Data distribution

- The full training input matrix no longer ships inside the package. It is
  hosted on OSF (https://osf.io/3csbz/) and obtained through
  `mlcvi_training_data()` (see above). Configure the source with
  `options(mlcvi.train_url = ...)` or the `MLCVI_TRAIN_URL` environment
  variable.
- A subsampled training matrix (25 respondents per country) ships with the
  package for examples and tests. It carries a `row_indices` attribute, and
  `mlcvi_extend()` / `mlcvi_ridge_model()` now derive the default
  `country_vec` from it, so the subsample can be passed as
  `train_input_matrix` directly.

## Code

- `mlcvi_extend()` and `mlcvi_ridge_model()` share one preparation and one
  fitting routine; results are unchanged. Input validation now uses named
  error messages instead of `stopifnot()`.
- `mlcvi_extend()` and `mlcvi_ridge_model()` restore the caller's
  random-number state on exit; results are unchanged.
- The reports printed by `mlcvi.get.distance()` go through `message()`
  (suppressible with `suppressMessages()` or `verbose = FALSE`) instead of
  `cat()`.
- Error messages use ASCII only.
- `readxl` moved from Imports to Suggests; it is only needed for the
  `path` argument of `mlcvi.get.mediator()`.

## Documentation and infrastructure

- Runnable examples for every exported function on bundled data.
- testthat suite covering the distance methods, the mediator screen, the
  metric helpers, and the imputation routines.
- `inst/CITATION` cites the published user guide.
- README documents installation, bundled data, and the function naming
  policy.
- Removed committed session artifacts and a stray root-level data file from
  the repository.

# mlcvi 0.3.1

- Fix seven bugs identified in a code audit.

# mlcvi 0.3.0

- Add `mlcvi_extend()` and `mlcvi_ridge_model()`.
