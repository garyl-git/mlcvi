# data-raw/make_study_datasets.R

# Requires:
# install.packages(c("readxl","usethis"))

if (!requireNamespace("readxl", quietly = TRUE)) stop("Install 'readxl'")
if (!requireNamespace("usethis", quietly = TRUE)) stop("Install 'usethis'")

read_xlsx_df <- function(path, sheet = NULL) {
  df <- readxl::read_excel(path, sheet = sheet)
  as.data.frame(df, stringsAsFactors = FALSE)
}

# ---- Your files live directly under data-raw/ ----
path_3a <- file.path("data-raw", "Study 3a_Data.xlsx")
path_4a <- file.path("data-raw", "Study 4a_Data.xlsx")

if (!file.exists(path_3a)) stop("Not found: ", path_3a, "\ngetwd(): ", getwd())
if (!file.exists(path_4a)) stop("Not found: ", path_4a, "\ngetwd(): ", getwd())

MLCVI_3A <- read_xlsx_df(path_3a)
MLCVI_4A <- read_xlsx_df(path_4a)

# (Optional) light numeric coercion
numify <- function(x) suppressWarnings(if (is.character(x)) as.numeric(x) else x)
make_numeric_if_possible <- function(df) {
  for (j in seq_along(df)) {
    if (is.factor(df[[j]])) df[[j]] <- as.character(df[[j]])
    tmp <- numify(df[[j]])
    if (sum(is.na(tmp)) <= sum(is.na(df[[j]]))) df[[j]] <- tmp
  }
  df
}
MLCVI_3A <- make_numeric_if_possible(MLCVI_3A)
MLCVI_4A <- make_numeric_if_possible(MLCVI_4A)

# Save as .rda in data/
usethis::use_data(MLCVI_3A, MLCVI_4A, overwrite = TRUE, compress = "xz")

message("Wrote: data/MLCVI_3A.rda and data/MLCVI_4A.rda")
