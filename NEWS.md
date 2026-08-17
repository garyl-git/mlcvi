# mlcvi (development version)

## Data distribution

- The full training input matrix no longer ships inside the package. It is
  looked up in `inst/extdata/` for source checkouts, then in the user cache,
  and is otherwise downloaded once and cached via `tools::R_user_dir()`.
  Configure the source with `options(mlcvi.train_url = ...)` or the
  `MLCVI_TRAIN_URL` environment variable.
- A subsampled training matrix (25 respondents per country) ships with the
  package for examples and tests. It carries a `row_indices` attribute, and
  `mlcvi_extend()` / `mlcvi_ridge_model()` now derive the default
  `country_vec` from it, so the subsample can be passed as
  `train_input_matrix` directly.

## Code

- `mlcvi_extend()` and `mlcvi_ridge_model()` share one preparation and one
  fitting routine; results are unchanged. Input validation now uses named
  error messages instead of `stopifnot()`.
- Error messages use ASCII only.

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
