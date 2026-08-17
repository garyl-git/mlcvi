# data-raw/make_country_codes.R
# Run once to create data/wvs_country_codes.rda
#
# Maps the WVS numeric country codes (ISO 3166-1 numeric, used as
# s003_<code> in train_output_matrix) to ISO 3166-1 alpha-3 codes and names.
# Source: ISOcodes::ISO_3166_1 (ISOcodes package, CRAN).

if (!requireNamespace("ISOcodes", quietly = TRUE)) {
  stop("Install the ISOcodes package to regenerate this dataset.")
}

iso <- ISOcodes::ISO_3166_1[, c("Numeric", "Alpha_3", "Name")]
iso$Numeric <- as.integer(iso$Numeric)

load(file.path("data", "train_output_matrix.rda"))
wvs_codes <- as.integer(sub("^s003_", "", colnames(train_output_matrix)))

idx <- match(wvs_codes, iso$Numeric)
if (anyNA(idx)) {
  stop("Unmapped WVS codes: ", paste(wvs_codes[is.na(idx)], collapse = ", "))
}

wvs_country_codes <- data.frame(
  wvs_code = wvs_codes,
  iso3     = iso$Alpha_3[idx],
  name     = iso$Name[idx],
  stringsAsFactors = FALSE
)
wvs_country_codes <- wvs_country_codes[order(wvs_country_codes$wvs_code), ]
rownames(wvs_country_codes) <- NULL

# Cross-check against the countries in the published ML-CVI matrix
load(file.path("data", "MLCVI_distance_matrix.rda"))
stopifnot(setequal(wvs_country_codes$iso3, rownames(MLCVI_distance_matrix)))

usethis::use_data(wvs_country_codes, overwrite = TRUE, compress = "xz")
