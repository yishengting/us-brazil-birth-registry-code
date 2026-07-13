# Statistical Analysis Plan

## Study Design

Repeated cross-sectional individual-level registry analysis of public-use live birth records in the United States and Brazil, 2017-2024. The analysis is framed as a registry marker surveillance and harmonisation study, not as causal inference or individual clinical prediction. The US 2024 natality public-use file had NCHS public-use documentation, whereas the Brazil SINASC 2024 OpenDataSUS resource was labelled preliminary at extraction; excluding-2024 models evaluate whether conclusions depend on the most recent registry year.

## Study Population

The primary analysis is restricted to singleton live births. Multiple births are excluded from the primary risk-score and risk-profile analyses because multiple gestation is a dominant obstetric pathway for preterm birth and low birth weight.

Outcome-specific complete records are used for adjusted models. Implausible gestational age (<22 or >44 weeks) and birth weight (<300 or >7000 g) values are set to missing. Multiple imputation is not used because unknown coding and source-variable meaning are not fully symmetric across countries, which could introduce less interpretable cross-national measurement error.

## Exposures

Three risk domains are used to build the singleton registry risk-marker score and mutually exclusive registry risk-marker profiles:

- Maternal age risk: age <20 or >=35 years.
- Low recorded prenatal-visit count marker: <4 recorded prenatal visits.
- Low education: less than completed high school or country-specific equivalent.

Low education is harmonized as US `meduc` codes 1-2 and Brazil `ESCMAE2010` codes 0-2. US high school/GED completion and Brazil secondary education are classified as middle education, not low education.

Prenatal-visit count is interpreted as a gestational-length-sensitive registry marker, not as a validated measure of care quality or a temporally isolated causal exposure. The singleton risk score is categorized as 0, 1, 2, or 3 domains. Mutually exclusive risk profiles are low risk, age risk only, low recorded prenatal-visit count only, low education only, age risk plus low recorded prenatal-visit count, age risk plus low education, low education plus low recorded prenatal-visit count, and all three domains. Records with missing score- or profile-defining domains are excluded from risk-score and risk-profile analyses rather than recoded as low risk.

## Outcomes

Primary outcomes are preterm birth (<37 completed weeks) and low birth weight (<2500 g).

Secondary outcomes reported in the manuscript or supplement include very preterm birth, very low birth weight, term low birth weight, and caesarean delivery. Caesarean delivery is treated as a contextual service-delivery indicator.

## Models

Modified Poisson regression with robust standard errors estimates adjusted risk ratios. Main models are adjusted for birth year, parity or birth order, and newborn sex. Models are fit separately by country and as pooled models with risk score by country or risk profile by country interaction terms, as appropriate. Country-interaction results are interpreted as differences in registry risk-marker association strength within a parallel surveillance framework, not as proof that source variables are clinically exchangeable across countries.

Risk-score models do not adjust for maternal age, prenatal visit count, or education because these variables define the exposure. Records with missing profile-defining domains are excluded from profile-classified analyses and are not recoded as low risk. Absolute risks and risk differences per 1000 singleton births are standardized to the observed covariate distribution within each country and estimated with delta-method 95% confidence intervals. Risk-difference variance uses the gradient difference and the full covariance between the two standardized predictions.

Grouped-count models use event counts and log offsets for covariate patterns and target the same categorical mean model as the corresponding individual-record Poisson model. Individual-record HC0 covariance is reconstructed exactly from grouped Bernoulli counts: within each covariate pattern, the squared score contribution is the sum of event and non-event contributions rather than one residual for the grouped row. A country-year stratified validation subsample compares point estimates, HC0 standard errors, standardized risks, and risk-difference standard errors against individual-record Poisson models using identical covariate specifications.

The validation subset uses seed `20260501`. Arrow CPU scanning is restricted to one thread before constructing the fixed country-year pools so file-fragment completion order cannot change the sampled records. Two consecutive recovery reruns produced byte-identical validation CSV files.

## Sensitivity Analyses

Sensitivity analyses include:

- Replacing the prenatal visit-count domain with no prenatal care only.
- Omitting prenatal visit count entirely and using an age-plus-education score.
- Repeating age-plus-education models among births to mothers aged >=25 years to evaluate age-education coupling.
- Repeating main 3-domain models by birth period: 2017-2019, 2020-2021, and 2022-2024.
- Repeating main 3-domain models after excluding 2024 to evaluate whether conclusions depend on the most recent registry year.
- Age-subtype models separating teenage motherhood, advanced maternal age 35-39 years, and very advanced maternal age >=40 years.
- Term low birth weight analyses.

The risk-stratified excess burden fraction is descriptive and is not interpreted as a causal population-attributable fraction.

A geographic fixed-effect sensitivity analysis was considered but not implemented because the US public-use natality files used for this analysis did not provide comparable state or county identifiers in the harmonized analytic file. Brazil UF identifiers alone were not used for an asymmetric cross-national fixed-effect sensitivity.

## Transparency Addenda

The primary manuscript display is limited to one main table and three main figures; annual outcome rates, risk-score models, risk-profile models, adjusted absolute risks, and interaction P values are provided in supplementary tables. Supplementary Table 12 reports complete-case derivation for the primary outcome models. Supplementary Table 13 reports registry variable algorithms, unknown handling, plausibility rules, and grouped-count modeling implementation. Supplementary Table 19 reports grouped-count coefficient and HC0 validation against individual-record Poisson models. A separate validation artifact records standardized-risk and joint-delta risk-difference checks.
