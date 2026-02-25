# data-raw/make_hofstede_data.R
# Run this once to create data/Hofstede_dims.rda

# If 'usethis' isn't installed, uncomment the next line and run once:
# install.packages("usethis")

csv_path <- file.path("inst", "extdata", "Hofstede_scores_updated.csv")
if (!file.exists(csv_path)) {
  stop("CSV not found at ", csv_path, ". Put it there first.")
}

Hofstede_dims <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

# Canonicalize column names you rely on in code
# (keep Country_Code with exact casing; dims in lower-case)
names(Hofstede_dims) <- sub("^\\s+|\\s+$", "", names(Hofstede_dims))  # trim whitespace

if (!"Country_Code" %in% names(Hofstede_dims)) {
  # try common alternatives (case-insensitive)
  alt <- tolower(names(Hofstede_dims))
  cc_idx <- which(alt %in% c("country_code", "iso alpha-3 code", "iso alpha3 code"))
  if (length(cc_idx) == 1) names(Hofstede_dims)[cc_idx] <- "Country_Code"
}

# Normalize Hofstede dims to lower-case canonical set if needed
dim_map <- c("pdi","idv","mas","uai","ltowvs","ivr")
for (nm in names(Hofstede_dims)) {
  lower <- tolower(nm)
  if (lower %in% dim_map && nm != lower) {
    names(Hofstede_dims)[names(Hofstede_dims) == nm] <- lower
  }
}

# Minimal validation
required <- c("Country_Code","pdi","idv","mas","uai")
missing_cols <- setdiff(required, names(Hofstede_dims))
if (length(missing_cols)) {
  stop("Hofstede CSV is missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Save as a package dataset (lazy-loaded)
usethis::use_data(Hofstede_dims, overwrite = TRUE)

