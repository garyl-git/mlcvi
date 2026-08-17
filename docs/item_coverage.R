# docs/item_coverage.R
# Coverage table for a set of WVS item codes in the ML-CVI training matrix:
# is each item a column, and in which waves / how many countries is it
# non-missing? Run from the package root with devtools::load_all().
#
# Usage:
#   source("docs/item_coverage.R")
#   cov <- item_coverage(c("a001", "e018", ...))   # item codes to check
#   print(cov$items)      # one row per item
#   print(cov$by_wave)    # items x waves: countries with >= min_n respondents
#
# Set full = FALSE to use the bundled subsample (fast, indicative only).
#
# IMPORTANT LIMITATION (verified 2026-08-13): the ML-CVI training matrix is
# complete -- every one of its 582 base items is 100% non-missing in all six
# waves and all 98 countries -- so it was imputed/preprocessed before it
# reached this package. This tool therefore answers "is the item present in
# the ML-CVI training data, and for which country-waves does the matrix
# have respondents?" It CANNOT tell whether an item was originally
# administered in a given WVS wave. For that, check the WVS codebook or the
# raw WVS longitudinal file (see docs/decisions-for-sheetal.md, question 5).

item_coverage <- function(items, full = TRUE, min_n = 30L) {
  items <- tolower(trimws(items))
  data  <- mlcvi_train_input(small = !full)
  cv    <- .wvs_to_iso3(.default_country_vec(data))
  wv    <- .default_wave_vec(data)
  present <- items %in% colnames(data)

  per_item <- data.frame(
    item          = items,
    in_matrix     = present,
    n_nonmissing  = NA_integer_,
    pct_nonmissing = NA_real_,
    waves_present = NA_character_,
    n_countries   = NA_integer_,
    stringsAsFactors = FALSE
  )
  by_wave <- matrix(NA_integer_, nrow = length(items), ncol = 6,
                    dimnames = list(items, paste0("wave", 1:6)))

  for (k in which(present)) {
    x  <- data[, items[k]]
    ok <- !is.na(x)
    per_item$n_nonmissing[k]   <- sum(ok)
    per_item$pct_nonmissing[k] <- round(100 * mean(ok), 1)
    per_item$n_countries[k]    <- length(unique(cv[ok]))
    waves <- sort(unique(wv[ok]))
    per_item$waves_present[k]  <- paste(waves, collapse = ",")
    for (w in 1:6) {
      rows <- ok & !is.na(wv) & wv == w
      if (!any(rows)) { by_wave[k, w] <- 0L; next }
      by_wave[k, w] <- sum(table(cv[rows]) >= min_n)
    }
  }
  list(items = per_item, by_wave = by_wave,
       note = paste0("by_wave = number of countries with >= ", min_n,
                     " non-missing respondents on the item in that wave; ",
                     "training data covers WVS waves 1-6 only."))
}
