# CleanFHU <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Standardised, reproducible data-cleaning pipelines for pregnancy and
> maternal/neonatal mortality studies.

<!-- badges: start -->
[![R-CMD-check](https://github.com/OnduruFred/CleanFHU/workflows/R-CMD-check/badge.svg)](https://github.com/OnduruFred/CleanFHU/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

---

## Overview

**CleanFHU** wraps the most common data-preparation steps for PRISMA-aligned
pregnancy studies — demographic recoding, clinical variable classification,
biomarker thresholds, WASH coding, and anemia grading — into composable,
well-documented functions. The goal is a single shared pipeline that multiple
analysts and sites can use without copy-pasting code.

### Design principles

| Principle | How CleanFHU achieves it |
|---|---|
| **Reproducibility** | Every classification rule lives in one function. Change the threshold once, it propagates everywhere. |
| **Composability** | Each classifier is exported individually — use `prisma_pipeline()` for the full workflow, or call `classify_anemia()` alone. |
| **Transparency** | Detailed roxygen docs and unit tests for every function. |
| **Extensibility** | Pass your own `facility_map` vector; override visit patterns; plug classifiers into your own pipeline. |

---

## Installation

```r
# Install from GitHub (once published):
# install.packages("pak")
pak::pak("OnduruFred/CleanFHU")
```

---

## Quick start

```r
library(CleanFHU)

# Run the full PRISMA pipeline
clean_data <- prisma_pipeline(raw_prisma_df)

# Or use individual classifiers
classify_anemia(hb = c(9.5, 11.2, 6.8),
                trimester = c("2nd Trimester", "1st Trimester", "3rd Trimester"))
#> [1] Mild None Severe
#> Levels: None Mild Moderate Severe

classify_water_source(c("Piped water into dwelling", "Unprotected well"))
#> [1] "Improved"   "Unimproved"

map_facility_level(c("JOOTRH", "Tingwangi Health Centre"))
#> [1] "Level 6" "Level 3"
```

---

## Function reference

### Pipeline

| Function | Description |
|---|---|
| `prisma_pipeline()` | Full enrollment + ANC + IPC + PNC cleaning pipeline |

### Demographic classifiers

| Function | Description |
|---|---|
| `classify_gravidity()` | Gravidity category from raw previous-pregnancy count |
| `classify_anc_visits()` | ANC visit frequency category |
| `classify_maternal_age()` | Teenage / Adult / Advanced Maternal Age |
| `classify_job_type()` | Collapse job codes to 4 employment groups |
| `classify_residence()` | Rural-Karemo vs Peri-urban Manyata |
| `classify_wall_type()` | Dirt/mud vs Cement/brick wall material |
| `map_facility_level()` | Facility name → MoH level |
| `default_facility_map()` | Built-in PRISMA Kenya facility lookup |
| `derive_preg_loss()` | Binary indicator for prior pregnancy loss |

### WASH classifiers

| Function | Description |
|---|---|
| `classify_water_source()` | WHO improved / unimproved water |
| `classify_toilet()` | WHO improved / unimproved sanitation |
| `classify_cooking_fuel()` | WHO clean / polluting cooking fuel |

### Clinical / biomarker classifiers

| Function | Description |
|---|---|
| `classify_trimester()` | Gestational trimester from GA weeks |
| `classify_anemia()` | Trimester-adjusted anemia severity (4 levels) |
| `derive_any_anemia()` | Binary trimester-adjusted anemia indicator |
| `classify_vitb12()` | Vitamin B12 status (deficient / insufficient / sufficient) |
| `classify_vita()` | Vitamin A / RBP4 status (4 levels) |
| `classify_stfr()` | sTfR iron status with trimester-specific cut-offs |
| `classify_iron_ferritin()` | Ferritin + inflammation-adjusted iron deficiency |
| `classify_g6pd()` | G6PD enzyme activity (normal / abnormal) |
| `derive_thalassemia()` | Any thalassemia from wide `rbc_thala_*` columns |

---

---

## Contributing

Please file issues and pull requests on GitHub. Run `devtools::test()` before
submitting changes. All new classifiers should include:

1. A documented, exported function in the appropriate `R/classifiers_*.R` file.
2. At least three unit tests in `tests/testthat/test-classifiers.R`.

---

## License

MIT © CleanFHU authors
