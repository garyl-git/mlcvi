# mlcvi

**Machine Learning-Based Cultural Values Inventory (ML-CVI)**

An R package for cross-cultural research. Provides tools for computing cultural distance between countries via the Kogut-Singh, Shulgin, and ML-CVI methods, screening cultural value mediators using Sobel tests, and imputing missing country-level indicators via Ridge regression weighted by ML-CVI feature importances.

## Installation

The package is not yet on CRAN. Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("garyl-git/mlcvi")
```

## Quick example

```r
library(mlcvi)

# Cultural distance between two countries (ISO alpha-3 codes)
mlcvi.get.distance("USA", "JPN", method = "KS")      # Kogut-Singh on Hofstede
mlcvi.get.distance("USA", "JPN", method = "Shulgin")
mlcvi.get.distance("USA", "JPN", method = "MLCVI")

# Screen the 60 ML-CVI items as mediators between a binary culture IV
# and a continuous DV (bundled Study 4a data: US vs India, environmental
# behaviour)
out <- mlcvi.get.mediator(df = MLCVI_4A, iv = "US0IN1", dv = "envavg")
out$significant_items
head(out$results)

# Impute a country-level indicator for countries where it is missing,
# using Ridge regression weighted by ML-CVI feature importances
codes  <- sub("^s003_", "", colnames(train_output_matrix))
scores <- data.frame(s003 = codes, values = rnorm(length(codes)))
scores$values[1:3] <- NA
mlcvi_extend(scores, lambda = 0.1, repeats = 5L, verbose = FALSE)
```

## Bundled data

| Object | Description |
|---|---|
| `Hofstede_dims` | Country scores on Hofstede's six dimensions |
| `Shulgin_csv` | Shulgin cultural distance table |
| `MLCVI_distance_matrix` | ML-CVI cultural distance matrix (98 countries) |
| `MLCVI_3A`, `MLCVI_4A` | Individual-level data from Studies 3a and 4a |
| `mlcvi_items_default`, `mlcvi_weights_default` | The 60 ML-CVI WVS items and their importance weights |
| `train_output_matrix` | One-hot country membership for the training sample |

The full individual-level training matrix (about 280 MB) is not shipped inside the package. `mlcvi_extend()` and `mlcvi_ridge_model()` download it once on first use and cache it under `tools::R_user_dir("mlcvi", "cache")`. Set `options(mlcvi.train_url = ...)` or the `MLCVI_TRAIN_URL` environment variable to point at an alternative source. A subsampled matrix for quick experiments ships with the package: `mlcvi:::mlcvi_train_input(small = TRUE)`.

## Function naming

The functions `mlcvi.get.distance()` and `mlcvi.get.mediator()` keep their original dotted names for compatibility with the published user guide. All newer functions use snake_case (`mlcvi_extend()`, `mlcvi_ridge_model()`), and future additions will follow that convention.

## Contact

For any queries or clarifications regarding the MLCVI R-package, please direct them to gary-h.leng@polyu.edu.hk. We will get back to you as soon as possible.

## Citation

Leng, G., & Sheetal, A. (2025). *Machine learning-based cultural values inventory (MLCVI) R package user guide*. SocArXiv. https://doi.org/10.31235/osf.io/73g8z_v1

```r
citation("mlcvi")
```
