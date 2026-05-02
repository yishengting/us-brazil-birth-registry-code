suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(arrow)
})

source("R/utils.R")

harmonize_births <- function(df) {
  df |>
    mutate(
      maternal_education_raw = as.character(maternal_education_raw),
      prenatal_visits_raw = as.character(prenatal_visits_raw),
      plurality_raw = as.character(plurality_raw),
      delivery_mode_raw = as.character(delivery_mode_raw),
      newborn_sex_raw = as.character(newborn_sex_raw),
      maternal_age = if_else(maternal_age >= 10 & maternal_age <= 55, maternal_age, NA_real_),
      maternal_age_cat = cut(
        maternal_age,
        breaks = c(-Inf, 19, 24, 29, 34, 39, Inf),
        labels = c("<20", "20-24", "25-29", "30-34", "35-39", ">=40"),
        right = TRUE
      ),
      age_risk = case_when(
        maternal_age < 20 | maternal_age >= 35 ~ TRUE,
        maternal_age >= 20 & maternal_age <= 34 ~ FALSE,
        TRUE ~ NA
      ),
      maternal_education_cat = harmonize_education(country, maternal_education_raw),
      low_education = case_when(
        maternal_education_cat == "low" ~ TRUE,
        maternal_education_cat %in% c("middle", "high") ~ FALSE,
        TRUE ~ NA
      ),
      prenatal_care_cat = case_when(
        is.na(prenatal_visits) ~ "unknown",
        prenatal_visits == 0 ~ "none_or_0",
        prenatal_visits >= 1 & prenatal_visits <= 3 ~ "1-3",
        prenatal_visits >= 4 & prenatal_visits <= 6 ~ "4-6",
        prenatal_visits >= 7 ~ ">=7",
        TRUE ~ "unknown"
      ),
      inadequate_prenatal_care = case_when(
        prenatal_visits < 4 ~ TRUE,
        prenatal_visits >= 4 ~ FALSE,
        TRUE ~ NA
      ),
      strict_inadequate_prenatal_care = case_when(
        prenatal_visits < 7 ~ TRUE,
        prenatal_visits >= 7 ~ FALSE,
        TRUE ~ NA
      ),
      plurality_cat = harmonize_plurality(country, plurality_raw),
      multiple_gestation = case_when(
        plurality_cat == "multiple" ~ TRUE,
        plurality_cat == "singleton" ~ FALSE,
        TRUE ~ NA
      ),
      delivery_mode = harmonize_delivery_mode(country, delivery_mode_raw),
      newborn_sex = harmonize_newborn_sex(country, newborn_sex_raw),
      gestational_age = if_else(gestational_age >= 22 & gestational_age <= 44, gestational_age, NA_real_),
      birth_weight = if_else(birth_weight >= 300 & birth_weight <= 7000, birth_weight, NA_real_),
      apgar5 = if_else(apgar5 >= 0 & apgar5 <= 10, apgar5, NA_real_),
      preterm_birth = gestational_age < 37,
      very_preterm_birth = gestational_age < 32,
      low_birth_weight = birth_weight < 2500,
      very_low_birth_weight = birth_weight < 1500,
      cesarean_delivery = delivery_mode == "cesarean",
      low_apgar5 = apgar5 < 7,
      term_low_birth_weight = gestational_age >= 37 & birth_weight < 2500,
      macrosomia = birth_weight >= 4000,
      risk_score = as.integer(age_risk) + as.integer(inadequate_prenatal_care) +
        as.integer(low_education) + as.integer(multiple_gestation),
      risk_score_cat = case_when(
        is.na(risk_score) ~ NA_character_,
        risk_score >= 3 ~ ">=3",
        TRUE ~ as.character(risk_score)
      ),
      risk_phenotype = classify_phenotype(age_risk, inadequate_prenatal_care, low_education, multiple_gestation)
    ) |>
    mutate(
      risk_score_cat = factor(risk_score_cat, levels = c("0", "1", "2", ">=3")),
      risk_phenotype = factor(
        risk_phenotype,
        levels = c(
          "low_risk", "age_risk_only", "inadequate_prenatal_care_only",
          "low_education_only", "multiple_only", "age_risk_plus_inadequate_care",
          "low_education_plus_inadequate_care", "multiple_plus_any_social_or_age_risk",
          "highest_risk"
        )
      )
    )
}

harmonize_education <- function(country, raw) {
  dplyr::case_when(
    country == "United States" & raw %in% c(1, 2) ~ "low",
    country == "United States" & raw %in% c(3, 4, 5, 6) ~ "middle",
    country == "United States" & raw %in% c(7, 8) ~ "high",
    country == "Brazil" & raw %in% c(0, 1, 2) ~ "low",
    country == "Brazil" & raw %in% c(3, 4) ~ "middle",
    country == "Brazil" & raw %in% c(5) ~ "high",
    TRUE ~ "unknown"
  )
}

harmonize_plurality <- function(country, raw) {
  dplyr::case_when(
    country == "United States" & raw == 1 ~ "singleton",
    country == "United States" & raw %in% c(2, 3, 4, 5) ~ "multiple",
    country == "Brazil" & raw == 1 ~ "singleton",
    country == "Brazil" & raw %in% c(2, 3) ~ "multiple",
    TRUE ~ "unknown"
  )
}

harmonize_delivery_mode <- function(country, raw) {
  dplyr::case_when(
    country == "United States" & raw == 1 ~ "vaginal",
    country == "United States" & raw == 2 ~ "cesarean",
    country == "Brazil" & raw == 1 ~ "vaginal",
    country == "Brazil" & raw == 2 ~ "cesarean",
    TRUE ~ "unknown"
  )
}

harmonize_newborn_sex <- function(country, raw) {
  raw_chr <- as.character(raw)
  dplyr::case_when(
    country == "United States" & raw_chr == "M" ~ "male",
    country == "United States" & raw_chr == "F" ~ "female",
    country == "Brazil" & raw_chr == "1" ~ "male",
    country == "Brazil" & raw_chr == "2" ~ "female",
    TRUE ~ "unknown"
  )
}

classify_phenotype <- function(age_risk, inadequate, low_education, multiple) {
  score <- as.integer(age_risk) + as.integer(inadequate) + as.integer(low_education) + as.integer(multiple)
  dplyr::case_when(
    is.na(score) ~ NA_character_,
    score >= 3 ~ "highest_risk",
    multiple & (age_risk | inadequate | low_education) ~ "multiple_plus_any_social_or_age_risk",
    age_risk & inadequate ~ "age_risk_plus_inadequate_care",
    low_education & inadequate ~ "low_education_plus_inadequate_care",
    multiple & !age_risk & !inadequate & !low_education ~ "multiple_only",
    inadequate & !age_risk & !low_education & !multiple ~ "inadequate_prenatal_care_only",
    age_risk & !inadequate & !low_education & !multiple ~ "age_risk_only",
    low_education & !age_risk & !inadequate & !multiple ~ "low_education_only",
    !age_risk & !inadequate & !low_education & !multiple ~ "low_risk",
    TRUE ~ NA_character_
  )
}
