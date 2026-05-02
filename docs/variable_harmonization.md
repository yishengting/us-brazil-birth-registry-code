# Variable Harmonization

Variable mappings are maintained in `config/harmonization_dictionary.yml`. This document summarizes the core principles.

- Unknown or not stated source values are preserved as `unknown`.
- Primary adjusted models use complete-case records for the outcome, exposure, and harmonized covariates.
- Gestational age uses obstetric estimate for the United States and `SEMAGESTAC` for Brazil when available.
- Brazil gestational-age categories are used only as a fallback and should be interpreted cautiously.
- Education and prenatal visit-count definitions are harmonized into broad categories because source coding differs by country.
- Low education is defined as US `meduc` codes 1-2 and Brazil `ESCMAE2010` codes 0-2. US high school/GED completion (`meduc` 3) and Brazil secondary education (`ESCMAE2010` 3) are classified as middle education rather than low education.
- The submission education mapping is source-code-level: US `meduc` 1-2 are low, 3-6 are middle, 7-8 are high, and 9/missing is unknown; Brazil `ESCMAE2010` 0-2 are low, 3-4 are middle, 5 is high, and 9/ignored/missing values are unknown.
