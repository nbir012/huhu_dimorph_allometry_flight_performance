# ============================================================================
# 04_tables.R
#
#   Table 1 — morphological & behavioural sexual dimorphism
#   Table 2 — trait allometry (SMA/OLS slopes vs pronotum width)
#   Table 3 — Gamma GLMs of male flight performance
#
# Run order: source 02_allometry.R and 03_flight_models.R first so that
# `dimorph_summary_table`, `dimorph_stats_table`, `allometry_slopes`,
# `model_distance`, `model_duration`, and `model_speed` exist in the session.
#
# Outputs:
#   - output/tables/table1_dimorphism.docx
#   - output/tables/table2_allometry.docx
#   - output/tables/table3_flight_glm.docx
# ============================================================================

# Packages --------------------------------------------------------------------

library(tidyverse)
library(here)
library(flextable)

# Make sure the output directory exists.
dir.create(
  here::here("output", "tables"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Global flextable defaults — Times New Roman, 10pt
set_flextable_defaults(
  font.family = "Times New Roman",
  font.size = 10,
  padding = 3
)

# make landscape
landscape <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape")
)


# ----------------------------------------------------------------------------
# Trait label lookup — maps internal column names to display names, units,
# and a fixed display order. Used by Tables 1 and 2.
# ----------------------------------------------------------------------------

trait_labels <- tibble::tribble(
  ~measurement          , ~label              , ~units     , ~order ,
  "lengthAntenna"       , "Antenna Length"    , "mm"       ,      1 ,
  "lengthF9"            , "Length F9"         , "mm"       ,      2 ,
  "lengthBodyTotal"     , "Body Length"       , "mm"       ,      3 ,
  "widthPronotum"       , "Pronotum Width"    , "mm"       ,      4 ,
  "lengthElytron"       , "Elytron Length"    , "mm"       ,      5 ,
  "lengthFemur"         , "Femur Length"      , "mm"       ,      6 ,
  "lengthTibia"         , "Tibia Length"      , "mm"       ,      7 ,
  "lengthWing"          , "Wing Length"       , "mm"       ,      8 ,
  "areaWing"            , "Wing Area"         , "mm\u00B2" ,      9 ,
  "wing_aspect_ratio"   , "Wing Aspect Ratio" , "-"        ,     10 ,
  "wing_loading_mg_mm2" , "Wing Loading"      , "-"        ,     11 ,
  "weightStart_mg"      , "Body Mass"         , "mg"       ,     12
)


# ============================================================================
# TABLE 1 — Sexual dimorphism in morphological traits
# ============================================================================

# Reshape dimorph_summary_table (long: sex x measurement) to wide, sex as
# column groups.
t1_wide <- dimorph_summary_table |>
  select(measurement, sex, sample_size, mean, sd, max, min, cv) |>
  pivot_wider(
    names_from = sex,
    values_from = c(sample_size, mean, sd, max, min, cv),
    names_glue = "{sex}_{.value}"
  )

# Join the Welch t-test results (statistic + BH-adjusted p).
t1_data <- trait_labels |>
  left_join(t1_wide, by = "measurement") |>
  left_join(
    dimorph_stats_table |> select(measurement, statistic, p.adj),
    by = "measurement"
  ) |>
  arrange(order)

# Assemble in display column order.
t1_display <- t1_data |>
  transmute(
    label,
    units,
    m_n = m_sample_size,
    m_mean,
    m_sd,
    m_max,
    m_min,
    m_cv,
    f_n = f_sample_size,
    f_mean,
    f_sd,
    f_max,
    f_min,
    f_cv,
    t = statistic,
    p = p.adj
  )

# Which row is the female antenna sample (for the damaged-antennae footnote).
antenna_row <- which(t1_display$label == "Antenna Length")
f9_row <- which(grepl("F9", t1_display$label))
wl_row <- which(t1_display$label == "Wing Loading")
bm_row <- which(t1_display$label == "Body Mass")

table1 <- t1_display |>
  flextable() |>
  set_header_labels(
    label = "",
    units = "Units",
    m_n = "n",
    m_mean = "Mean",
    m_sd = "SD",
    m_max = "Max",
    m_min = "Min",
    m_cv = "CV (%)",
    f_n = "n",
    f_mean = "Mean",
    f_sd = "SD",
    f_max = "Max",
    f_min = "Min",
    f_cv = "CV (%)",
    t = "t",
    p = "p"
  ) |>
  add_header_row(
    values = c("", "", "Males", "Females", "t-test"),
    colwidths = c(1, 1, 6, 6, 2)
  ) |>
  colformat_double(
    j = c(
      "m_mean",
      "m_sd",
      "m_max",
      "m_min",
      "m_cv",
      "f_mean",
      "f_sd",
      "f_max",
      "f_min",
      "f_cv",
      "t"
    ),
    digits = 2,
    big.mark = ""
  ) |>
  colformat_double(j = "p", digits = 3, big.mark = "") |>
  align(i = 1, part = "header", align = "center") |>
  align(
    j = c(
      "units",
      "m_n",
      "m_mean",
      "m_sd",
      "m_max",
      "m_min",
      "m_cv",
      "f_n",
      "f_mean",
      "f_sd",
      "f_max",
      "f_min",
      "f_cv",
      "t",
      "p"
    ),
    align = "center",
    part = "body"
  ) |>
  bold(part = "header", i = 1) |>
  footnote(
    i = c(antenna_row, antenna_row, f9_row, f9_row),
    j = c("m_n", "f_n", "m_n", "f_n"),
    value = as_paragraph(
      "Both antennae were damaged in some individuals so these were ",
      "excluded from analysis."
    ),
    ref_symbols = "*",
    part = "body"
  ) |>
  footnote(
    i = c(wl_row, bm_row),
    j = c("f_n", "f_n"),
    value = as_paragraph(
      "Body mass and wing loading were recorded only for the subset of females ",
      "allocated to the flight-mill experiment (n = 13); all other female traits ",
      "were measured on the full sample (n = 24)."
    ),
    ref_symbols = "\u2020",
    part = "body"
  ) |>
  add_footer_lines(
    as_paragraph(
      "CV = coefficient of variation. Differences in mean trait sizes ",
      "between sexes were assessed using Welch's t-tests. P-values were ",
      "corrected for multiple comparisons using the Benjamini-Hochberg method."
    )
  ) |>
  set_caption(
    "Table 1. Sexual dimorphism in morphological traits of Prionoplus reticularis."
  ) |>
  autofit()

save_as_docx(
  table1,
  path = here::here("output", "tables", "table1_dimorphism.docx"),
  pr_section = landscape
)


# ============================================================================
# TABLE 2 — Trait allometry: slopes vs pronotum width, by sex
# ============================================================================

# allometry_slopes is long (one row per trait x sex). Reshape to wide with
# sex as column groups. Trait names there differ slightly, so map them.
t2_trait_map <- tibble::tribble(
  ~trait              , ~label              , ~order ,
  "lengthAntenna"     , "Antennal Length"   ,      1 ,
  "lengthF9"          , "Length F9"         ,      2 ,
  "lengthFemur"       , "Femur Length"      ,      3 ,
  "lengthTibia"       , "Tibia Length"      ,      4 ,
  "lengthWing"        , "Wing Length"       ,      5 ,
  "areaWing_sqrt"     , "Wing Area"         ,      6 ,
  "wing_aspect_ratio" , "Wing Aspect Ratio" ,      7 ,
  "wing_loading"      , "Wing Loading"      ,      8
)

t2_wide <- allometry_slopes |>
  select(trait, sex, n, intercept, slope, r2, p_r2, r_vs_iso, p_vs_iso) |>
  pivot_wider(
    names_from = sex,
    values_from = c(n, intercept, slope, r2, p_r2, r_vs_iso, p_vs_iso),
    names_glue = "{sex}_{.value}"
  )

t2_display <- t2_trait_map |>
  left_join(t2_wide, by = "trait") |>
  arrange(order) |>
  transmute(
    label,
    m_n,
    m_intercept,
    m_slope,
    m_r2,
    m_p_r2,
    m_r_vs_iso,
    m_p_vs_iso,
    f_n,
    f_intercept,
    f_slope,
    f_r2,
    f_p_r2,
    f_r_vs_iso,
    f_p_vs_iso
  )

table2 <- t2_display |>
  flextable() |>
  set_header_labels(
    label = "",
    m_n = "n",
    m_intercept = "Intercept (\u03B1)",
    m_slope = "Slope (\u03B2)",
    m_r2 = "r\u00B2",
    m_p_r2 = "p",
    m_r_vs_iso = "r",
    m_p_vs_iso = "p",
    f_n = "n",
    f_intercept = "Intercept (\u03B1)",
    f_slope = "Slope (\u03B2)",
    f_r2 = "r\u00B2",
    f_p_r2 = "p",
    f_r_vs_iso = "r",
    f_p_vs_iso = "p"
  ) |>
  # The H0 spanners sit ABOVE the column labels (one per pair of test cols).
  add_header_row(
    values = c(
      "",
      "",
      "",
      "",
      "H0: uncorrelated",
      "H0: slope = 1",
      "",
      "",
      "",
      "H0: uncorrelated",
      "H0: slope = 1"
    ),
    colwidths = c(1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2)
  ) |>
  # The sex spanner sits ABOVE the H0 row (one per sex's 7 cols).
  add_header_row(
    values = c("", "Males", "Females"),
    colwidths = c(1, 7, 7)
  ) |>
  colformat_double(
    j = c(
      "m_intercept",
      "m_slope",
      "m_r2",
      "m_r_vs_iso",
      "f_intercept",
      "f_slope",
      "f_r2",
      "f_r_vs_iso"
    ),
    digits = 3
  ) |>
  colformat_double(
    j = c("m_p_r2", "m_p_vs_iso", "f_p_r2", "f_p_vs_iso"),
    digits = 3
  ) |>
  align(i = 1:3, part = "header", align = "center") |>
  align(
    j = setdiff(names(t2_display), "label"),
    align = "center",
    part = "body"
  ) |>
  bold(part = "header", i = 1) |>
  add_footer_lines(
    as_paragraph(
      "F9 = ninth flagellomere. Slopes were estimated by ordinary least ",
      "squares on log10-transformed traits regressed on log10 pronotum ",
      "width. Wing area was square-root transformed prior to log ",
      "transformation so that the isometric expectation is a slope of 1."
    )
  ) |>
  set_caption(
    "Table 2. Allometric scaling of traits against pronotum width in Prionoplus reticularis."
  ) |>
  autofit()

save_as_docx(
  table2,
  path = here::here("output", "tables", "table2_allometry.docx"),
  pr_section = landscape
)


# ============================================================================
# TABLE 3 — Gamma GLMs of male flight performance
# ============================================================================

# Build one tidy row-set per model, with exponentiated coefficients and the
# model-fit statistics, then stack the three models with a grouping column.
glm_table_one <- function(model, response_label) {
  rt <- report::report_table(model)

  coefs <- rt |>
    as_tibble() |>
    filter(!is.na(Coefficient)) |>
    transmute(
      response = response_label,
      term = Parameter,
      exp_coef = exp(Coefficient),
      exp_ci_low = exp(CI_low),
      exp_ci_hi = exp(CI_high),
      t = t,
      p = p
    )

  # Model-fit stats live in the Fit column of report_table.
  fit <- rt |>
    as_tibble() |>
    filter(!is.na(Fit)) |>
    select(Parameter, Fit) |>
    deframe()

  list(coefs = coefs, fit = fit)
}

# Friendlier predictor labels. report::report_table() converts column names
# like "wing_aspect_ratio" to "wing aspect ratio" before returning them, so
# the keys here must use the spaced form, not the underscored form.
term_labels <- c(
  "(Intercept)" = "Intercept",
  "widthPronotum" = "Pronotum Width",
  "wing aspect ratio" = "Wing Aspect Ratio",
  "wing loading mg mm2" = "Wing Loading"
)

m_dist <- glm_table_one(model_distance, "Average flight distance")
m_dur <- glm_table_one(model_duration, "Average flight duration")
m_spd <- glm_table_one(model_speed, "Average flight speed")

t3_display <- bind_rows(m_dist$coefs, m_dur$coefs, m_spd$coefs) |>
  mutate(term = recode(term, !!!term_labels))

# Pull fit stats into a per-response caption-style block. report_table's Fit
# rows are named; grab the ones the manuscript reports.
fit_row <- function(fit, response_label) {
  tibble(
    response = response_label,
    AIC = as.numeric(fit[["AIC"]]),
    AICc = as.numeric(fit[["AICc"]]),
    BIC = as.numeric(fit[["BIC"]]),
    R2_Nagelkerke = suppressWarnings(as.numeric(fit[["R2_Nagelkerke"]])),
    Sigma = suppressWarnings(as.numeric(fit[["Sigma"]]))
  )
}

t3_fit <- bind_rows(
  fit_row(m_dist$fit, "Average flight distance"),
  fit_row(m_dur$fit, "Average flight duration"),
  fit_row(m_spd$fit, "Average flight speed")
)

table3 <- t3_display |>
  select(response, term, exp_coef, exp_ci_low, exp_ci_hi, t, p) |>
  as_grouped_data(groups = "response") |>
  flextable() |>
  set_header_labels(
    response = "",
    term = "Predictor",
    exp_coef = "Exp. Coefficient",
    exp_ci_low = "Exp. CI low",
    exp_ci_hi = "Exp. CI high",
    t = "t",
    p = "p"
  ) |>
  colformat_double(
    j = c("exp_coef", "exp_ci_low", "exp_ci_hi", "t"),
    digits = 2,
    big.mark = ""
  ) |>
  colformat_double(j = "p", digits = 3, big.mark = "") |>
  align(
    j = c("exp_coef", "exp_ci_low", "exp_ci_hi", "t", "p"),
    align = "center",
    part = "body"
  ) |>
  align(part = "header", align = "center") |>
  bold(part = "header") |>
  bold(j = "response", part = "body") |>
  add_footer_lines(
    as_paragraph(
      "All models are generalised linear models with a Gamma family and ",
      "log link. Coefficients and confidence intervals are exponentiated ",
      "to the response scale. Model-fit statistics are reported separately ",
      "in Table 3b."
    )
  ) |>
  set_caption(
    "Table 3. Effects of body size and wing traits on male flight performance in Prionoplus reticularis."
  ) |>
  autofit()

# Companion model-fit table
table3_fit <- t3_fit |>
  flextable() |>
  set_header_labels(
    response = "Dependent Variable",
    AIC = "AIC",
    AICc = "AICc",
    BIC = "BIC",
    R2_Nagelkerke = "Nagelkerke's R\u00B2",
    Sigma = "Sigma"
  ) |>
  colformat_double(j = c("AIC", "AICc", "BIC"), digits = 2) |>
  colformat_double(j = c("R2_Nagelkerke", "Sigma"), digits = 2) |>
  align(
    j = c("AIC", "AICc", "BIC", "R2_Nagelkerke", "Sigma"),
    align = "center",
    part = "body"
  ) |>
  align(part = "header", align = "center") |>
  bold(part = "header") |>
  set_caption(
    "Table 3b. Model-fit statistics for the flight-performance GLMs."
  ) |>
  autofit()

save_as_docx(
  `Table 3` = table3,
  `Table 3b` = table3_fit,
  path = here::here("output", "tables", "table3_flight_glm.docx"),
  pr_section = landscape
)
