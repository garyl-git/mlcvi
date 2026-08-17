# Design: mlcvi_get_panel() and mlcvi_build_matrix()

Written 2026-08-13 before implementation. Kept out of the built package
(docs/ is in .Rbuildignore).

## 1. mlcvi_get_panel()

Purpose: vectorized dyad-year distance lookup for merging into panel data
(e.g. a 1990-2024 dyadic trade panel).

Signature:

    mlcvi_get_panel(pairs,
                    method = c("MLCVI", "KS", "Shulgin"),
                    iso1 = "iso1", iso2 = "iso2", year = "year",
                    rule = c("static", "wave", "locf"),
                    data = NULL,
                    ks_dims = c("6dims", "4dims"))

- `pairs`: data frame with at least the two country-code columns; a year
  column is required for rule != "static" and optional otherwise.
- Returns `pairs` with the distance appended as a numeric column named
  `distance` plus a character `distance_note` column that is NA when the
  lookup succeeded and otherwise holds the reason (unknown code, missing
  cell, missing dimension). Rows are never dropped or reordered, so the
  result merges back 1:1.
- Vectorized: matrix indexing `mat[cbind(i, j)]` for the matrix methods; the
  KS method computes the index once for all rows from the Hofstede table.
- Never prints; no `cat()`.
- `rule = "static"`: single time-invariant table (all the package can do
  today; the year column is passed through untouched).
- `rule = "wave"` / `"locf"`: accepted, validated, and currently `stop()`
  with a clear message that per-wave tables are not yet available. When
  they are, `data` will accept a named list of matrices keyed by wave and
  the year->wave map will be a documented internal table. Keeping the
  argument now fixes the API shape so users' code does not change later.
- `data`: same override semantics as `mlcvi.get.distance()`.
- KS returns the six-dimension index by default (`ks_dims`), because
  Hofstede's LTO/IVR are the dimensions most often available for the
  countries in this table; the four-dimension variant is a switch.

Reuse: `.mlcvi_load_*` loaders, `.clean_iso3`, `.kogut_singh_dims4/6`.
The KS per-row logic is factored so `.kogut_singh_pair()` and the panel share
the variance computation.

## 2. mlcvi_build_matrix()

Purpose: classical cultural-distance matrices from a user-specified WVS item
set, at country level or country-by-wave level, from individual-level data.

Signature:

    mlcvi_build_matrix(items,
                       data = NULL,          # individual-level matrix
                       country_vec = NULL,   # factor per row
                       wave_vec = NULL,      # integer per row (optional)
                       method = c("ks", "euclidean", "mahalanobis"),
                       by_wave = FALSE,
                       min_n = 30L,
                       min_items = 1L)

- `data` defaults to the full training matrix (download-on-first-use);
  the bundled subsample can be passed for quick runs.
- `country_vec` defaults via `.default_country_vec()`. `wave_vec`, when
  NULL and `by_wave = TRUE`, is derived from the one-hot `s002_1..s002_6`
  columns in the training matrix (WVS wave indicator).
- Step 1: country(-wave) means of `items` with `na.rm = TRUE`; groups with
  fewer than `min_n` respondents are dropped with a message.
- Step 2: distance between all pairs of groups:
  - `ks`: mean over items of squared difference / cross-group variance of
    the item (Kogut-Singh with items as dimensions);
  - `euclidean`: root sum of squared differences of item means;
  - `mahalanobis`: squared Mahalanobis distance using the covariance of the
    group means (regularized with a small ridge if singular).
- Missing-item handling: for each pair, use the items non-missing for both
  groups; require at least `min_items`; for `ks` and `euclidean`
  renormalize by the number of items used (mirrors the existing KS
  reduced-dimension fallback and its warning). For `mahalanobis`, pairs
  with any missing item get NA (renormalization is not well defined).
- Returns: if `by_wave = FALSE`, a symmetric numeric matrix with ISO3
  dimnames and zero diagonal, directly usable as `data` for
  `mlcvi.get.distance(method = "MLCVI")` and `mlcvi_get_panel()`. If
  `by_wave = TRUE`, a named list of such matrices, one per wave, ready for
  the future `rule = "wave"` in `mlcvi_get_panel()`.
- Attribute `items_used` (integer matrix, per pair) so users can audit
  coverage; attribute `n_per_group`.
- Naming: these are classical methods on custom items and are labelled by
  their method name; the "MLCVI" label stays reserved for the published
  matrix (pending the methodology decision on custom-item labelling).

Country codes: `train_output_matrix` columns are `s003_<numeric WVS code>`,
not ISO3. `mlcvi_build_matrix()` therefore returns WVS numeric country codes
as dimnames unless a `code_map` (data frame with `wvs`, `iso3`) is supplied;
the package ships one derived from the WVS codebook if available, otherwise
users map codes themselves. This is checked at implementation time.

## Tests

- panel: shape preserved, order preserved, values equal scalar lookups,
  errors surface as notes not stops, unknown method/rule errors, wave/locf
  stop cleanly, KS 4 vs 6 dims, custom `data`.
- build_matrix: symmetric/zero-diagonal, KS on custom items equals manual
  formula on two countries, missing-item renormalization, min_n dropping,
  by_wave returns list keyed by wave, mahalanobis on well-conditioned data
  matches stats::mahalanobis.
