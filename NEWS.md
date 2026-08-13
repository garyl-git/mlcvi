# mlcvi (development version)

- The full training input matrix no longer ships inside the package. It is
  looked up in `inst/extdata/` for source checkouts, then in the user cache,
  and is otherwise downloaded once and cached via `tools::R_user_dir()`.
  Configure the source with `options(mlcvi.train_url = ...)` or the
  `MLCVI_TRAIN_URL` environment variable.
- A subsampled training matrix now ships with the package for examples and
  tests.
- Removed committed session artifacts and a stray root-level data file from
  the repository.

# mlcvi 0.3.1
