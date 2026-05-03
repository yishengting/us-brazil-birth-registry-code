suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(scales)
})

source("R/utils.R")

pub_palette <- c("Brazil" = "#0072B2", "United States" = "#D55E00", "Pooled" = "#333333")

outcome_labels <- c(
  preterm_birth = "Preterm birth",
  low_birth_weight = "Low birth weight",
  cesarean_delivery = "Cesarean delivery",
  very_preterm_birth = "Very preterm birth",
  very_low_birth_weight = "Very low birth weight",
  low_apgar5 = "Low 5-minute Apgar",
  congenital_anomaly = "Congenital anomaly",
  term_low_birth_weight = "Term low birth weight",
  macrosomia = "Macrosomia"
)

risk_score_labels <- c(
  "risk_score_cat1" = "1 risk domain",
  "risk_score_cat2" = "2 risk domains",
  "risk_score_cat>=3" = ">=3 risk domains"
)

phenotype_labels <- c(
  low_risk = "Low risk",
  age_risk_only = "Age risk only",
  inadequate_prenatal_care_only = "Low prenatal-visit count only",
  low_education_only = "Low education only",
  multiple_only = "Multiple gestation only",
  age_risk_plus_inadequate_care = "Age risk + low visit count",
  low_education_plus_inadequate_care = "Low education + low visit count",
  multiple_plus_any_social_or_age_risk = "Multiple + any social/age risk",
  highest_risk = "Highest risk (>=3 domains)"
)

publication_theme <- function(base_size = 10) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle = element_text(size = base_size, color = "grey25"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey15"),
      strip.background = element_rect(fill = "grey92", color = "grey65", linewidth = 0.3),
      strip.text = element_text(face = "bold"),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.minor = element_blank()
    )
}

format_n <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE)
}

format_num <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits)
}

format_rr_ci <- function(estimate, low, high) {
  sprintf("%.2f (%.2f-%.2f)", estimate, low, high)
}

format_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

save_publication_figure <- function(plot, filename, width, height, dir = root_path("outputs", "publication", "figures")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(dir, paste0(filename, ".png"))
  pdf_path <- file.path(dir, paste0(filename, ".pdf"))
  tiff_path <- file.path(dir, paste0(filename, ".tiff"))
  ggsave(png_path, plot, width = width, height = height, dpi = 600, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  ggsave(tiff_path, plot, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
  invisible(c(png = png_path, pdf = pdf_path, tiff = tiff_path))
}

make_publication_flowchart <- function(counts) {
  total <- sum(counts$births, na.rm = TRUE)
  br <- sum(counts$births[counts$country == "Brazil"], na.rm = TRUE)
  us <- sum(counts$births[counts$country == "United States"], na.rm = TRUE)
  period_label <- paste0(min(counts$birth_year, na.rm = TRUE), "-", max(counts$birth_year, na.rm = TRUE))
  ggplot() +
    annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 2.75, ymax = 3.35, fill = "white", color = "grey20", linewidth = 0.45) +
    annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 1.65, ymax = 2.25, fill = "white", color = "grey20", linewidth = 0.45) +
    annotate("rect", xmin = 0.08, xmax = 0.92, ymin = 0.55, ymax = 1.15, fill = "white", color = "grey20", linewidth = 0.45) +
    annotate("segment", x = 0.5, xend = 0.5, y = 2.75, yend = 2.25, linewidth = 0.45, arrow = arrow(length = grid::unit(0.18, "cm"))) +
    annotate("segment", x = 0.5, xend = 0.5, y = 1.65, yend = 1.15, linewidth = 0.45, arrow = arrow(length = grid::unit(0.18, "cm"))) +
    annotate("text", x = 0.5, y = 3.05, label = sprintf("Public live birth registry records\\nUnited States and Brazil, %s\\nN = %s", period_label, format_n(total)), size = 3.6, lineheight = 0.95) +
    annotate("text", x = 0.5, y = 1.95, label = sprintf("Harmonized analytic records\\nUnited States: %s; Brazil: %s", format_n(us), format_n(br)), size = 3.6, lineheight = 0.95) +
    annotate("text", x = 0.5, y = 0.85, label = "Outcome-specific complete records used for adjusted models\\nMissing or unknown categories were not recoded as low risk", size = 3.4, lineheight = 0.95) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0.35, 3.55), expand = FALSE) +
    labs(title = "Figure 1. Study population flow") +
    theme_void(base_family = "Helvetica") +
    theme(plot.title = element_text(face = "bold", hjust = 0, size = 12, margin = margin(b = 8)))
}

make_publication_trends <- function(table2) {
  year_breaks <- seq(min(table2$birth_year, na.rm = TRUE), max(table2$birth_year, na.rm = TRUE))
  period_label <- paste0(min(year_breaks), "-", max(year_breaks))
  table2 |>
    select(country, birth_year, preterm_birth_rate_per_1000, low_birth_weight_rate_per_1000, cesarean_delivery_rate_per_1000) |>
    pivot_longer(ends_with("_rate_per_1000"), names_to = "outcome", values_to = "rate") |>
    mutate(
      outcome = recode(
        outcome,
        preterm_birth_rate_per_1000 = "Preterm birth",
        low_birth_weight_rate_per_1000 = "Low birth weight",
        cesarean_delivery_rate_per_1000 = "Cesarean delivery"
      ),
      outcome = factor(outcome, levels = c("Preterm birth", "Low birth weight", "Cesarean delivery"))
    ) |>
    ggplot(aes(x = birth_year, y = rate, color = country, group = country)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.1, stroke = 0.2) +
    facet_wrap(~ outcome, scales = "free_y", nrow = 1) +
    scale_color_manual(values = pub_palette) +
    scale_x_continuous(breaks = year_breaks) +
    labs(
      title = "Figure 2. Annual trends in adverse birth outcomes",
      subtitle = sprintf("Rates per 1,000 live births, United States and Brazil, %s", period_label),
      x = "Birth year",
      y = "Rate per 1,000 live births"
    ) +
    publication_theme(10)
}

make_publication_rr_forest <- function(table3) {
  table3 |>
    filter(country %in% c("Brazil", "United States"), term %in% names(risk_score_labels), outcome %in% c("preterm_birth", "low_birth_weight", "cesarean_delivery")) |>
    mutate(
      term = factor(risk_score_labels[term], levels = rev(unname(risk_score_labels))),
      outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight", "Cesarean delivery"))
    ) |>
    ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, color = country)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45", linewidth = 0.4) +
    geom_errorbarh(height = 0.18, position = position_dodge(width = 0.55), linewidth = 0.45) +
    geom_point(position = position_dodge(width = 0.55), size = 2.1) +
    facet_wrap(~ outcome, nrow = 1) +
    scale_color_manual(values = pub_palette) +
    scale_x_log10(breaks = c(0.5, 1, 2, 4, 8), labels = c("0.5", "1", "2", "4", "8")) +
    labs(
      title = "Figure 3. Adjusted risk ratios by maternal risk score",
      subtitle = "Reference group: 0 risk domains; error bars show 95% confidence intervals",
      x = "Adjusted risk ratio (log scale)",
      y = NULL
    ) +
    publication_theme(10)
}

make_publication_absolute_risk <- function(table5) {
  table5 |>
    filter(outcome %in% c("preterm_birth", "low_birth_weight", "cesarean_delivery")) |>
    mutate(
      outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight", "Cesarean delivery")),
      risk_phenotype = factor(phenotype_labels[risk_phenotype], levels = rev(unname(phenotype_labels)))
    ) |>
    ggplot(aes(x = adjusted_risk_per_1000, y = risk_phenotype, fill = country)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
    scale_fill_manual(values = pub_palette) +
    labs(
      title = "Figure 4. Adjusted absolute risk by registry risk-marker profile",
      subtitle = "Model-standardized risks per 1,000 live births",
      x = "Adjusted risk per 1,000 live births",
      y = NULL
    ) +
    publication_theme(9)
}

make_publication_paf <- function(paf) {
  paf |>
    filter(outcome %in% c("preterm_birth", "low_birth_weight", "cesarean_delivery", "low_apgar5", "congenital_anomaly")) |>
    mutate(
      outcome = factor(outcome_labels[outcome], levels = rev(c("Preterm birth", "Low birth weight", "Cesarean delivery", "Low 5-minute Apgar", "Congenital anomaly")))
    ) |>
    ggplot(aes(x = paf * 100, y = outcome, fill = country)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    scale_fill_manual(values = pub_palette) +
    labs(
      title = "Figure 5. Population attributable fraction for non-low-risk phenotypes",
      subtitle = "Attributable fraction estimated against the low-risk phenotype",
      x = "Population attributable fraction (%)",
      y = NULL
    ) +
    publication_theme(10)
}

write_rtf_table <- function(data, path, title, note = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("\\\\", "\\\\\\\\", x)
    x <- gsub("\\{", "\\\\{", x)
    x <- gsub("\\}", "\\\\}", x)
    x
  }
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines("{\\rtf1\\ansi\\deff0", con)
  writeLines("{\\fonttbl{\\f0 Arial;}}", con)
  writeLines("\\fs22", con)
  writeLines(paste0("\\b ", esc(title), "\\b0\\par"), con)
  writeLines("\\par", con)
  header <- paste(esc(names(data)), collapse = "\\tab ")
  writeLines(paste0("\\b ", header, "\\b0\\par"), con)
  apply(data, 1, function(row) {
    writeLines(paste(esc(row), collapse = "\\tab "), con)
  })
  if (!is.null(note)) {
    writeLines("\\par", con)
    writeLines(paste0("\\i Note. ", esc(note), "\\i0\\par"), con)
  }
  writeLines("}", con)
}

write_doc_csv <- function(data, path) {
  write_csv_safe(data, path)
}

make_pub_table1 <- function(table1) {
  table1 |>
    mutate(across(where(is.numeric), as.numeric)) |>
    transmute(
      Country = country,
      `Live births, n` = format_n(births),
      `Maternal age, mean years` = format_num(maternal_age_mean, 1),
      `Maternal age risk, %` = format_num(age_risk_pct, 1),
      `Low education, %` = format_num(low_education_pct, 1),
      `Low recorded prenatal-visit count, %` = format_num(inadequate_prenatal_care_pct, 1),
      `Multiple gestation, %` = format_num(multiple_gestation_pct, 1),
      `Preterm birth, %` = format_num(preterm_birth_pct, 1),
      `Low birth weight, %` = format_num(low_birth_weight_pct, 1),
      `Cesarean delivery, %` = format_num(cesarean_delivery_pct, 1)
    )
}

make_pub_table2 <- function(table2) {
  table2 |>
    arrange(country, birth_year) |>
    transmute(
      Country = country,
      Year = birth_year,
      `Live births, n` = format_n(births),
      `Preterm birth per 1,000` = format_num(preterm_birth_rate_per_1000, 1),
      `Low birth weight per 1,000` = format_num(low_birth_weight_rate_per_1000, 1),
      `Cesarean delivery per 1,000` = format_num(cesarean_delivery_rate_per_1000, 1),
      `Low Apgar per 1,000` = format_num(low_apgar5_rate_per_1000, 1),
      `Congenital anomaly per 1,000` = format_num(congenital_anomaly_rate_per_1000, 1)
    )
}

make_pub_table3 <- function(table3) {
  table3 |>
    filter(country %in% c("Brazil", "United States"), term %in% names(risk_score_labels), outcome %in% c("preterm_birth", "low_birth_weight", "cesarean_delivery")) |>
    mutate(
      Outcome = outcome_labels[outcome],
      Country = country,
      `Risk score` = risk_score_labels[term],
      `aRR (95% CI)` = format_rr_ci(estimate, conf.low, conf.high),
      `P value` = format_p(p.value)
    ) |>
    select(Outcome, Country, `Risk score`, `aRR (95% CI)`, `P value`)
}

make_pub_table4 <- function(table4) {
  table4 |>
    filter(str_detect(term, "^risk_phenotype"), !str_detect(term, ":")) |>
    mutate(
      Outcome = outcome_labels[outcome],
      `Risk phenotype` = str_remove(term, "^risk_phenotype"),
      `Risk phenotype` = phenotype_labels[`Risk phenotype`],
      `aRR (95% CI)` = format_rr_ci(estimate, conf.low, conf.high),
      `P value` = format_p(p.value)
    ) |>
    select(Outcome, `Risk phenotype`, `aRR (95% CI)`, `P value`)
}

make_pub_table5 <- function(table5) {
  table5 |>
    filter(outcome %in% c("preterm_birth", "low_birth_weight", "cesarean_delivery")) |>
    mutate(
      Outcome = outcome_labels[outcome],
      Country = country,
      `Risk phenotype` = phenotype_labels[risk_phenotype],
      `Adjusted risk per 1,000` = format_num(adjusted_risk_per_1000, 1),
      `Risk difference per 1,000` = format_num(risk_difference_per_1000, 1)
    ) |>
    select(Outcome, Country, `Risk phenotype`, `Adjusted risk per 1,000`, `Risk difference per 1,000`)
}
