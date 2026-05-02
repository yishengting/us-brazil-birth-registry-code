source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
})

submission_root <- Sys.getenv("SUBMISSION_ROOT", unset = "")
sub_dir <- if (nzchar(submission_root)) submission_root else root_path("submission")
fig_png <- file.path(sub_dir, "figures", "png")
fig_pdf <- file.path(sub_dir, "figures", "pdf")
fig_tiff <- file.path(sub_dir, "figures", "tiff")
tab_main <- file.path(sub_dir, "tables", "main")
tab_supp <- file.path(sub_dir, "tables", "supplementary")
tab_ready <- file.path(tab_main, "publication_ready")
out_dir <- root_path("outputs", "publication_ready_upgrade")
dir.create(fig_png, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_pdf, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_tiff, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_ready, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cfg <- load_config()
period_label <- paste0(min(cfg$project$years), "-", max(cfg$project$years))
year_breaks <- seq(min(cfg$project$years), max(cfg$project$years))

country_palette <- c("Brazil" = "#0072B2", "United States" = "#D55E00")
country_shapes <- c("Brazil" = 16, "United States" = 17)
outcome_labels <- c(
  preterm_birth = "Preterm birth",
  low_birth_weight = "Low birth weight",
  very_preterm_birth = "Very preterm birth",
  very_low_birth_weight = "Very low birth weight",
  term_low_birth_weight = "Term low birth weight"
)
profile_labels <- c(
  low_risk = "Low risk",
  age_only = "Age risk only",
  inadequate_care_only = "Low prenatal-visit count only",
  low_education_only = "Low education only",
  age_inadequate = "Age risk + low visit count",
  age_low_education = "Age risk + low education",
  education_inadequate = "Low education + low visit count",
  all_three = "All three domains"
)

parse_num <- function(x) as.numeric(gsub(",", "", as.character(x), fixed = FALSE))
fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt1 <- function(x) sprintf("%.1f", as.numeric(x))
fmt_rr <- function(est, lo, hi) sprintf("%.2f (%.2f-%.2f)", est, lo, hi)

write_csv_safe <- function(x, path) {
  utils::write.table(x, path, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE, qmethod = "double", na = "")
}

theme_journal <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      axis.title = element_text(face = "bold", color = "#111827"),
      axis.text = element_text(color = "#1F2937"),
      axis.line = element_line(color = "#6B7280", linewidth = 0.25),
      axis.ticks = element_line(color = "#6B7280", linewidth = 0.25),
      legend.position = "top",
      legend.title = element_blank(),
      legend.key.width = grid::unit(0.55, "cm"),
      strip.background = element_rect(fill = "#F3F4F6", color = "#9CA3AF", linewidth = 0.25),
      strip.text = element_text(face = "bold", color = "#111827"),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(8, 12, 8, 8)
    )
}

save_all <- function(plot, name, width, height) {
  png_path <- file.path(fig_png, paste0(name, ".png"))
  pdf_path <- file.path(fig_pdf, paste0(name, ".pdf"))
  tiff_path <- file.path(fig_tiff, paste0(name, ".tiff"))
  out_png <- file.path(out_dir, paste0(name, ".png"))
  out_pdf <- file.path(out_dir, paste0(name, ".pdf"))
  out_tiff <- file.path(out_dir, paste0(name, ".tiff"))
  ggsave(png_path, plot, width = width, height = height, dpi = 600, bg = "white")
  ggsave(pdf_path, plot, width = width, height = height, device = grDevices::cairo_pdf, bg = "white")
  ggsave(tiff_path, plot, width = width, height = height, dpi = 600, compression = "lzw", bg = "white")
  invisible(file.copy(png_path, out_png, overwrite = TRUE))
  invisible(file.copy(pdf_path, out_pdf, overwrite = TRUE))
  invisible(file.copy(tiff_path, out_tiff, overwrite = TRUE))
}

table2_raw <- read.csv(root_path("outputs", "revision", "tables", "table2_singleton_outcome_rates.csv"), stringsAsFactors = FALSE)
table3_raw <- read.csv(root_path("outputs", "revision", "tables", "table3_singleton_risk_score_models.csv"), stringsAsFactors = FALSE)
table4_raw <- read.csv(root_path("outputs", "revision", "tables", "table4_singleton_phenotype_models.csv"), stringsAsFactors = FALSE)
abs_risk <- read.csv(root_path("outputs", "revision2", "tables", "table5_absolute_risks_with_ci.csv"), stringsAsFactors = FALSE)
score_interactions <- read.csv(root_path("outputs", "revision2", "tables", "table3_country_interaction_ratios.csv"), stringsAsFactors = FALSE)
flow <- read.csv(root_path("outputs", "revision", "tables", "flow_counts.csv"), stringsAsFactors = FALSE)
profile_prev <- read.csv(file.path(sub_dir, "tables", "supplementary", "Supplementary_Table_9_risk_profile_prevalence.csv"), stringsAsFactors = FALSE, check.names = FALSE)

country_all <- read.csv(file.path(sub_dir, "data_provenance", "final_birth_counts_by_country_year.csv"), stringsAsFactors = FALSE) |>
  group_by(country) |>
  summarise(all_births = sum(births, na.rm = TRUE), .groups = "drop")
country_singleton <- read.csv(file.path(tab_main, "table1_singleton_baseline.csv"), stringsAsFactors = FALSE, check.names = FALSE) |>
  filter(Characteristic == "Singleton births, n") |>
  pivot_longer(cols = c("Brazil", "United States"), names_to = "country", values_to = "singleton_births") |>
  mutate(singleton_births = parse_num(singleton_births))
country_profile <- profile_prev |>
  rename(country = Country, total_births = `Total births`) |>
  mutate(total_births = parse_num(total_births)) |>
  distinct(country, total_births)
country_flow <- country_all |>
  left_join(country_singleton, by = "country") |>
  left_join(country_profile, by = "country") |>
  mutate(excluded_multiple = all_births - singleton_births)

all_n <- flow$n[flow$metric == "all_births"]
singleton_n <- flow$n[flow$metric == "singleton_births"]
multiple_n <- all_n - singleton_n
profile_n <- sum(country_flow$total_births, na.rm = TRUE)
profile_missing_n <- singleton_n - profile_n
preterm_n <- flow$n[flow$metric == "preterm_model_n"]
lbw_n <- flow$n[flow$metric == "lbw_model_n"]

flow_box <- function(xmin, xmax, ymin, ymax, fill = "white") {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, color = "#374151", linewidth = 0.42)
}

flow_plot <- ggplot() +
  flow_box(0.08, 0.68, 4.60, 5.30) +
  flow_box(0.08, 0.68, 3.30, 4.00) +
  flow_box(0.08, 0.68, 2.00, 2.70) +
  flow_box(0.08, 0.38, 0.62, 1.34, "#FBFCFD") +
  flow_box(0.46, 0.76, 0.62, 1.34, "#FBFCFD") +
  flow_box(0.73, 0.96, 3.62, 4.22, "#F9FAFB") +
  flow_box(0.73, 0.96, 2.30, 2.90, "#F9FAFB") +
  annotate("segment", x = 0.38, xend = 0.38, y = 4.60, yend = 4.00, color = "#374151", linewidth = 0.42, arrow = arrow(type = "closed", length = grid::unit(0.14, "cm"))) +
  annotate("segment", x = 0.38, xend = 0.38, y = 3.30, yend = 2.70, color = "#374151", linewidth = 0.42, arrow = arrow(type = "closed", length = grid::unit(0.14, "cm"))) +
  annotate("segment", x = 0.38, xend = 0.23, y = 2.00, yend = 1.34, color = "#374151", linewidth = 0.42, arrow = arrow(type = "closed", length = grid::unit(0.14, "cm"))) +
  annotate("segment", x = 0.38, xend = 0.61, y = 2.00, yend = 1.34, color = "#374151", linewidth = 0.42, arrow = arrow(type = "closed", length = grid::unit(0.14, "cm"))) +
  annotate("segment", x = 0.68, xend = 0.73, y = 3.65, yend = 3.92, color = "#6B7280", linewidth = 0.35) +
  annotate("segment", x = 0.68, xend = 0.73, y = 2.35, yend = 2.60, color = "#6B7280", linewidth = 0.35) +
  annotate("text", x = 0.38, y = 5.08, label = "All eligible live-birth registry records", fontface = "bold", size = 3.15, color = "#111827") +
  annotate("text", x = 0.38, y = 4.86, label = paste0(period_label, "; N = ", fmt_n(all_n)), size = 2.78, color = "#1F2937") +
  annotate("text", x = 0.38, y = 4.67, label = paste0("Brazil: ", fmt_n(country_flow$all_births[country_flow$country == "Brazil"]), "   United States: ", fmt_n(country_flow$all_births[country_flow$country == "United States"])), size = 2.45, color = "#4B5563") +
  annotate("text", x = 0.38, y = 3.78, label = "Singleton births in primary analysis", fontface = "bold", size = 3.15, color = "#111827") +
  annotate("text", x = 0.38, y = 3.56, label = paste0("N = ", fmt_n(singleton_n)), size = 2.78, color = "#1F2937") +
  annotate("text", x = 0.38, y = 3.37, label = paste0("Brazil: ", fmt_n(country_flow$singleton_births[country_flow$country == "Brazil"]), "   United States: ", fmt_n(country_flow$singleton_births[country_flow$country == "United States"])), size = 2.45, color = "#4B5563") +
  annotate("text", x = 0.38, y = 2.48, label = "Profile-classifiable singleton births", fontface = "bold", size = 3.15, color = "#111827") +
  annotate("text", x = 0.38, y = 2.26, label = paste0("N = ", fmt_n(profile_n), " (", fmt1(profile_n / singleton_n * 100), "% of singletons)"), size = 2.78, color = "#1F2937") +
  annotate("text", x = 0.38, y = 2.07, label = paste0("Brazil: ", fmt_n(country_flow$total_births[country_flow$country == "Brazil"]), "   United States: ", fmt_n(country_flow$total_births[country_flow$country == "United States"])), size = 2.45, color = "#4B5563") +
  annotate("text", x = 0.23, y = 1.08, label = "Preterm birth model", fontface = "bold", size = 2.85, color = "#111827") +
  annotate("text", x = 0.23, y = 0.86, label = paste0("Complete records\nN = ", fmt_n(preterm_n)), size = 2.55, lineheight = 0.95, color = "#1F2937") +
  annotate("text", x = 0.61, y = 1.08, label = "Low birth weight model", fontface = "bold", size = 2.85, color = "#111827") +
  annotate("text", x = 0.61, y = 0.86, label = paste0("Complete records\nN = ", fmt_n(lbw_n)), size = 2.55, lineheight = 0.95, color = "#1F2937") +
  annotate("text", x = 0.845, y = 4.03, label = "Excluded", fontface = "bold", size = 2.7, color = "#111827") +
  annotate("text", x = 0.845, y = 3.82, label = paste0("Multiple births\nn = ", fmt_n(multiple_n)), size = 2.45, lineheight = 0.95, color = "#374151") +
  annotate("text", x = 0.845, y = 2.70, label = "Not profile-classifiable", fontface = "bold", size = 2.7, color = "#111827") +
  annotate("text", x = 0.845, y = 2.49, label = paste0("Missing age, education,\nor visit-count domain\nn = ", fmt_n(profile_missing_n)), size = 2.35, lineheight = 0.9, color = "#374151") +
  coord_cartesian(xlim = c(0.03, 0.99), ylim = c(0.42, 5.42), expand = FALSE) +
  theme_void(base_family = "Helvetica") +
  theme(plot.margin = margin(8, 8, 8, 8))
save_all(flow_plot, "figure1_study_flow", 7.2, 7.4)

trend_data <- table2_raw |>
  select(country, birth_year, preterm_birth_rate_per_1000, low_birth_weight_rate_per_1000, very_preterm_birth_rate_per_1000, very_low_birth_weight_rate_per_1000) |>
  pivot_longer(ends_with("_per_1000"), names_to = "outcome", values_to = "rate") |>
  mutate(
    outcome = recode(
      outcome,
      preterm_birth_rate_per_1000 = "Preterm birth",
      low_birth_weight_rate_per_1000 = "Low birth weight",
      very_preterm_birth_rate_per_1000 = "Very preterm birth",
      very_low_birth_weight_rate_per_1000 = "Very low birth weight"
    ),
    outcome = factor(outcome, levels = c("Preterm birth", "Low birth weight", "Very preterm birth", "Very low birth weight"))
  )
trend_end <- trend_data |>
  group_by(outcome, country) |>
  filter(birth_year == max(birth_year, na.rm = TRUE)) |>
  ungroup()
trend_plot <- ggplot(trend_data, aes(birth_year, rate, color = country, group = country)) +
  geom_line(linewidth = 0.82) +
  geom_point(size = 1.8, stroke = 0.15) +
  geom_text(data = trend_end, aes(label = country), hjust = 0, nudge_x = 0.16, size = 2.65, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 2) +
  scale_color_manual(values = country_palette) +
  scale_x_continuous(breaks = year_breaks) +
  coord_cartesian(xlim = c(min(year_breaks), max(year_breaks) + 0.95), clip = "off") +
  labs(x = "Birth year", y = "Rate per 1,000 singleton live births") +
  theme_journal(9) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.margin = margin(8, 68, 8, 8)
  )
save_all(trend_plot, "figure2_outcome_trends", 8.7, 6.2)

score_terms <- paste0("risk_score3_cat", 1:3)
score_rows <- tidyr::expand_grid(
  outcome = c("preterm_birth", "low_birth_weight"),
  score_num = 1:3
) |>
  mutate(
    outcome_label = outcome_labels[outcome],
    score_label = paste(score_num, if_else(score_num == 1, "domain", "domains")),
    term = paste0("risk_score3_cat", score_num),
    y = case_when(
      outcome == "preterm_birth" ~ 7.4 - score_num * 0.62,
      TRUE ~ 4.5 - score_num * 0.62
    )
  )
score_forest <- table3_raw |>
  filter(country %in% names(country_palette), outcome %in% c("preterm_birth", "low_birth_weight"), term %in% score_terms) |>
  mutate(score_num = as.integer(str_remove(term, "risk_score3_cat"))) |>
  left_join(score_rows |> select(outcome, term, score_num, outcome_label, score_label, y), by = c("outcome", "term", "score_num")) |>
  mutate(
    y_plot = y + if_else(country == "Brazil", 0.09, -0.09),
    rr_label = fmt_rr(estimate, conf.low, conf.high)
  )
score_text <- score_rows |>
  left_join(
    score_forest |>
      select(outcome, term, country, rr_label) |>
      pivot_wider(names_from = country, values_from = rr_label),
    by = c("outcome", "term")
  ) |>
  left_join(
    score_interactions |>
      mutate(
        term = paste0("risk_score3_cat", risk_score),
        ratio_label = fmt_rr(ratio_of_aRR_US_vs_Brazil, conf.low, conf.high)
      ) |>
      select(outcome, term, ratio_label),
    by = c("outcome", "term")
  )

ylims <- c(1.95, 7.85)
label_panel <- ggplot() +
  annotate("text", x = 0.02, y = 7.58, label = "Preterm birth", hjust = 0, fontface = "bold", size = 3.2, color = "#111827") +
  annotate("text", x = 0.02, y = 4.68, label = "Low birth weight", hjust = 0, fontface = "bold", size = 3.2, color = "#111827") +
  geom_text(data = score_text, aes(x = 0.08, y = y, label = score_label), hjust = 0, size = 2.85, color = "#1F2937") +
  coord_cartesian(xlim = c(0, 1), ylim = ylims, expand = FALSE) +
  theme_void(base_family = "Helvetica") +
  theme(plot.margin = margin(5, 2, 28, 2))
forest_panel <- ggplot(score_forest, aes(estimate, y_plot, xmin = conf.low, xmax = conf.high, color = country, shape = country)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#6B7280", linewidth = 0.32) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", width = 0.08, linewidth = 0.48) +
  geom_point(size = 2.1) +
  scale_color_manual(values = country_palette) +
  scale_shape_manual(values = country_shapes) +
  scale_x_log10(breaks = c(1, 1.5, 2, 3, 4), labels = c("1", "1.5", "2", "3", "4"), limits = c(0.95, 4.1)) +
  coord_cartesian(ylim = ylims, expand = FALSE) +
  labs(x = "Adjusted risk ratio", y = NULL) +
  theme_journal(8.7) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "#F3F4F6", linewidth = 0.25),
    legend.position = "top",
    plot.margin = margin(5, 4, 5, 4)
  )
value_panel <- ggplot(score_text) +
  annotate("text", x = 0.03, y = 7.72, label = "Brazil aRR (95% CI)", hjust = 0, fontface = "bold", size = 2.65, color = "#111827") +
  annotate("text", x = 0.36, y = 7.72, label = "US aRR (95% CI)", hjust = 0, fontface = "bold", size = 2.65, color = "#111827") +
  annotate("text", x = 0.63, y = 7.72, label = "Ratio of aRRs\n(US/Brazil)", hjust = 0, fontface = "bold", size = 2.45, lineheight = 0.88, color = "#111827") +
  geom_text(aes(x = 0.03, y = y, label = Brazil), hjust = 0, size = 2.65, color = "#1F2937") +
  geom_text(aes(x = 0.36, y = y, label = `United States`), hjust = 0, size = 2.65, color = "#1F2937") +
  geom_text(aes(x = 0.63, y = y, label = ratio_label), hjust = 0, size = 2.65, color = "#1F2937") +
  coord_cartesian(xlim = c(0, 1.12), ylim = c(1.95, 8.05), expand = FALSE) +
  theme_void(base_family = "Helvetica") +
  theme(plot.margin = margin(5, 2, 28, 2))
score_plot <- label_panel + forest_panel + value_panel +
  plot_layout(widths = c(0.80, 1.15, 1.85), guides = "collect") &
  theme(legend.position = "top")
save_all(score_plot, "figure3_risk_score_forest", 10.5, 5.1)

profile_order_top <- c(
  "Age risk + low visit count",
  "All three domains",
  "Low prenatal-visit count only",
  "Low education + low visit count",
  "Age risk + low education",
  "Age risk only",
  "Low education only",
  "Low risk"
)
abs_plot_data <- abs_risk |>
  filter(outcome %in% c("preterm_birth", "low_birth_weight")) |>
  mutate(
    outcome = factor(outcome_labels[outcome], levels = c("Preterm birth", "Low birth weight")),
    profile = factor(profile_labels[exposure], levels = rev(profile_order_top)),
    core_profile = exposure %in% c("age_inadequate", "all_three"),
    y_nudge = if_else(country == "Brazil", 0.10, -0.10)
  )
abs_plot <- ggplot(abs_plot_data, aes(adjusted_risk_per_1000, profile, color = country, shape = country)) +
  geom_errorbar(
    data = abs_plot_data |> filter(!core_profile),
    aes(xmin = risk_ci_low, xmax = risk_ci_high),
    orientation = "y", width = 0.13, position = position_dodge(width = 0.55), linewidth = 0.34, alpha = 0.55
  ) +
  geom_point(
    data = abs_plot_data |> filter(!core_profile),
    position = position_dodge(width = 0.55), size = 1.65, alpha = 0.62
  ) +
  geom_errorbar(
    data = abs_plot_data |> filter(core_profile),
    aes(xmin = risk_ci_low, xmax = risk_ci_high),
    orientation = "y", width = 0.16, position = position_dodge(width = 0.55), linewidth = 0.62
  ) +
  geom_point(
    data = abs_plot_data |> filter(core_profile),
    position = position_dodge(width = 0.55), size = 2.35
  ) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_x") +
  scale_color_manual(values = country_palette) +
  scale_shape_manual(values = country_shapes) +
  labs(x = "Adjusted risk per 1,000 singleton live births", y = NULL) +
  theme_journal(9) +
  theme(
    panel.grid.major.x = element_line(color = "#F3F4F6", linewidth = 0.25),
    panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.3)
  )
save_all(abs_plot, "figure4_absolute_risk_with_ci", 10.2, 5.4)

table2_n_col <- if ("singleton_births" %in% names(table2_raw)) "singleton_births" else "births"
table2_cesarean_col <- if ("caesarean_delivery_rate_per_1000" %in% names(table2_raw)) {
  "caesarean_delivery_rate_per_1000"
} else {
  "cesarean_delivery_rate_per_1000"
}
table2_display <- table2_raw |>
  transmute(
    Country = country,
    `Birth year` = birth_year,
    `Singleton births, n` = fmt_n(.data[[table2_n_col]]),
    `Preterm birth per 1,000` = fmt1(preterm_birth_rate_per_1000),
    `Low birth weight per 1,000` = fmt1(low_birth_weight_rate_per_1000),
    `Very preterm birth per 1,000` = fmt1(very_preterm_birth_rate_per_1000),
    `Very low birth weight per 1,000` = fmt1(very_low_birth_weight_rate_per_1000),
    `Caesarean delivery per 1,000` = fmt1(.data[[table2_cesarean_col]])
  )
write_csv_safe(table2_display, file.path(tab_main, "table2_singleton_outcome_rates.csv"))

compact_model_table <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  x |>
    select(-any_of(c("Model N (Brazil)", "Model N (United States)", "Model N (Pooled)")))
}
first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) stop("None of these files exist: ", paste(paths, collapse = ", "))
  hit[[1]]
}
table3_compact <- compact_model_table(first_existing(c(
  file.path(tab_main, "table3_singleton_risk_score_models.csv"),
  file.path(tab_supp, "Supplementary_Table_16_risk_score_models.csv")
)))
table4_compact <- compact_model_table(first_existing(c(
  file.path(tab_main, "table4_singleton_risk_profile_models.csv"),
  file.path(tab_supp, "Supplementary_Table_17_risk_profile_models.csv")
)))
write_csv_safe(table3_compact, file.path(tab_main, "table3_singleton_risk_score_models.csv"))
write_csv_safe(table4_compact, file.path(tab_main, "table4_singleton_risk_profile_models.csv"))

table1_display <- read.csv(file.path(tab_main, "table1_singleton_baseline.csv"), stringsAsFactors = FALSE, check.names = FALSE)
table5_display <- read.csv(first_existing(c(
  file.path(tab_main, "table5_absolute_risks_with_ci.csv"),
  file.path(tab_supp, "Supplementary_Table_18_absolute_risks_with_ci.csv")
)), stringsAsFactors = FALSE, check.names = FALSE)

md_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x
}
md_table <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  header <- paste(md_escape(names(x)), collapse = " | ")
  divider <- paste(rep("---", ncol(x)), collapse = " | ")
  rows <- apply(x, 1, function(row) paste(md_escape(row), collapse = " | "))
  paste(c(paste0("| ", header, " |"), paste0("| ", divider, " |"), paste0("| ", rows, " |")), collapse = "\n")
}
table_notes <- list(
  table1 = "Values are percentages unless otherwise indicated. Percentages are calculated within country among singleton births in 2017 to 2024. Missing or implausible values are shown for key profile-defining and outcome variables.",
  table2 = "Rates are per 1,000 singleton live births. Caesarean delivery is summarised as a contextual service-delivery indicator rather than as a direct analogue of preterm birth or low birth weight.",
  table3 = "Reference group is 0 risk domains. Domains were maternal age risk, low recorded prenatal-visit count (<4 visits), and low maternal education. Ratio of aRRs compares the United States with Brazil using pooled interaction models. Model N for preterm birth was 20,007,217 in Brazil and 27,608,675 in the United States; model N for low birth weight was 20,214,871 in Brazil and 27,620,016 in the United States. Interaction P values are provided in Supplementary Table 2.",
  table4 = "Reference group is the low-risk profile. Visit-count profiles refer to low recorded prenatal-visit count (<4 visits), not a validated measure of care quality. Ratio of aRRs compares the United States with Brazil using pooled interaction terms. Model N for preterm birth was 20,007,217 in Brazil and 27,608,675 in the United States; model N for low birth weight was 20,214,871 in Brazil and 27,620,016 in the United States. Interaction P values are provided in Supplementary Table 14.",
  table5 = "Adjusted risks and risk differences are reported per 1,000 singleton live births with delta-method 95% confidence intervals. Estimates were standardised to the observed covariate distribution within each country and use outcome-specific complete-case analytic populations. Risk difference is reported as Reference for the low-risk profile."
)
table_titles <- list(
  table1 = "Table 1. Baseline characteristics of singleton births by country",
  table2 = "Table 2. Annual outcome rates among singleton births by country",
  table3 = "Table 3. Association between maternal risk score and primary outcomes among singleton births",
  table4 = "Table 4. Association between singleton maternal risk profile and primary outcomes",
  table5 = "Table 5. Adjusted absolute risks and risk differences by singleton maternal risk profile"
)
table_objects <- list(
  table1 = table1_display,
  table2 = table2_display,
  table3 = table3_compact,
  table4 = table4_compact,
  table5 = table5_display
)
write_table_md <- function(id, include_title = TRUE) {
  title <- if (include_title) paste0("## ", table_titles[[id]], "\n\n") else ""
  paste0(title, md_table(table_objects[[id]]), "\n\n", "*Note.* ", table_notes[[id]], "\n")
}
combined_md <- paste(
  "# Publication-ready main tables\n",
  paste(vapply(names(table_objects), write_table_md, character(1)), collapse = "\n\\newpage\n\n"),
  sep = "\n"
)
combined_md_path <- file.path(tab_ready, "main_tables_publication_ready.md")
writeLines(combined_md, combined_md_path, useBytes = TRUE)
for (id in names(table_objects)) {
  writeLines(write_table_md(id), file.path(tab_ready, paste0(id, "_publication_ready.md")), useBytes = TRUE)
}
pandoc <- Sys.which("pandoc")
if (nzchar(pandoc)) {
  system2(pandoc, c(combined_md_path, "-o", file.path(tab_ready, "main_tables_publication_ready.docx")))
  for (id in names(table_objects)) {
    system2(pandoc, c(file.path(tab_ready, paste0(id, "_publication_ready.md")), "-o", file.path(tab_ready, paste0(id, "_publication_ready.docx"))))
  }
} else {
  warning("Pandoc not found; Markdown tables were written but DOCX tables were not generated.")
}

message("Publication-ready figures and tables upgraded.")
