# ============================================================================
# 03_flight_models.R
#
# Sexual dimorphism in flight metrics (Results paragraph) and the influence
# of wing traits on male flight performance (Table 3, Figure 4).
#
# Reads:
#   - The `allometrySlopeWide` data frame and its `df_allometry_long` and
#     long-format derivatives, as built in 02_allometry.R. Source that
#     script first.
#
# Outputs:
#   - output/table_model_distance.csv
#   - output/table_model_duration.csv
#   - output/table_model_speed.csv
#   - output/plot_model_flight_perf.png  (Figure 4)
# ============================================================================

# Packages --------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(performance)


# Long-format flight averages for dimorphism tests ---------------------------

df_flight_long <- read_rds(here::here("data", "df_flightmill_output.RDS")) |>
  select(iD, sex, starts_with("average"), -average_rps) |>
  pivot_longer(
    cols = starts_with("average"),
    names_to = "measurement",
    values_to = "mm"
  )


# ----------------------------------------------------------------------------
# Sexual dimorphism in flight metrics (distance, duration, speed)
#
# Method note from manuscript: groups are unbalanced, so we test variance
# equality with Levene's test. Variances were unequal for distance and
# duration, equal for speed; we use Kruskal-Wallis throughout for
# consistency. p-values are BH-adjusted.
# ----------------------------------------------------------------------------

levene_dimorph_flight_stats <- df_flight_long |>
  mutate(sex = as.factor(sex)) |>
  group_by(measurement) |>
  drop_na(mm) |>
  rstatix::levene_test(mm ~ sex)

kruskal_dimorph_flight_stats <- df_flight_long |>
  group_by(measurement) |>
  drop_na(mm) |>
  rstatix::kruskal_test(mm ~ sex) |>
  rstatix::adjust_pvalue(method = "BH") |>
  rstatix::add_significance()

levene_dimorph_flight_stats
kruskal_dimorph_flight_stats


# ----------------------------------------------------------------------------
# Male-only data for the GLMs
# ----------------------------------------------------------------------------

df <- allometrySlopeWide |>
  select(
    id,
    sex,
    weightStart_mg,
    wing_aspect_ratio,
    wing_loading_mg_mm2,
    widthPronotum,
    average_distance,
    average_mps,
    average_duration,
    lengthWing,
    areaWing
  ) |>
  filter(sex == "m") |>
  drop_na()


# ----------------------------------------------------------------------------
# Table 3: Gamma GLMs with log link
#
# Three additive models predicting flight performance from body size
# (pronotum width), wing aspect ratio, and wing loading. Gamma family chosen
# because the response is continuous, positive, and right-skewed.
# ----------------------------------------------------------------------------

model_distance <- glm(
  average_distance ~ widthPronotum + wing_aspect_ratio + wing_loading_mg_mm2,
  data = df,
  family = Gamma(link = "log")
)

model_duration <- glm(
  average_duration ~ widthPronotum + wing_aspect_ratio + wing_loading_mg_mm2,
  data = df,
  family = Gamma(link = "log")
)

model_speed <- glm(
  average_mps ~ widthPronotum + wing_aspect_ratio + wing_loading_mg_mm2,
  data = df,
  family = Gamma(link = "log")
)

summary(model_distance)
summary(model_duration)
summary(model_speed)

report::report(model_distance)
report::report(model_duration)
report::report(model_speed)

# Diagnostic plots: linearity, residual normality, homoscedasticity,
# multicollinearity.
check_model(model_distance)
check_model(model_duration)
check_model(model_speed)


# ----------------------------------------------------------------------------
# Table 3 export: exponentiated coefficients and 95% CIs
# ----------------------------------------------------------------------------

exp_table <- function(model) {
  report::report_table(model) |>
    mutate(
      exp_coeff = exp(Coefficient),
      exp_CI_low = exp(CI_low),
      exp_CI_high = exp(CI_high)
    ) |>
    relocate(starts_with("exp_"), .after = "CI_high") |>
    select(Parameter, exp_coeff, exp_CI_low, exp_CI_high, t, p, Fit)
}

table_model_distance <- exp_table(model_distance)
table_model_duration <- exp_table(model_duration)
table_model_speed <- exp_table(model_speed)

write.csv(
  table_model_distance,
  here::here("output", "table_model_distance.csv")
)
write.csv(
  table_model_duration,
  here::here("output", "table_model_duration.csv")
)
write.csv(
  table_model_speed,
  here::here("output", "table_model_speed.csv")
)

writeLines(
  capture.output(report::report(model_distance)),
  here::here("output", "report_model_distance.txt")
)
writeLines(
  capture.output(report::report(model_duration)),
  here::here("output", "report_model_duration.txt")
)
writeLines(
  capture.output(report::report(model_speed)),
  here::here("output", "report_model_speed.txt")
)

# ----------------------------------------------------------------------------
# Figure 4: marginal effect of wing loading on each flight metric
# Pronotum width and aspect ratio are held at their reference values.
# ----------------------------------------------------------------------------

plot_model_distance <- ggeffects::ggpredict(
  model_distance,
  terms = "wing_loading_mg_mm2"
) |>
  plot(show_data = TRUE) +
  labs(y = "Average Distance (mm)", x = "Wing Loading", title = "") +
  theme_classic()

plot_model_duration <- ggeffects::ggpredict(
  model_duration,
  terms = "wing_loading_mg_mm2"
) |>
  plot(show_data = TRUE) +
  labs(y = "Average Duration (sec)", x = "Wing Loading", title = "") +
  theme_classic()

plot_model_speed <- ggeffects::ggpredict(
  model_speed,
  terms = "wing_loading_mg_mm2"
) |>
  plot(show_data = TRUE) +
  labs(y = "Average Speed (m/s)", x = "Wing Loading", title = "") +
  theme_classic()

plot_model_flight_perf <- plot_model_distance +
  plot_model_duration +
  plot_model_speed +
  plot_annotation(tag_levels = "A")

ggsave(
  filename = "plot_model_flight_perf.png",
  plot = plot_model_flight_perf,
  device = "png",
  path = here::here("output"),
  width = 10,
  height = 5,
  dpi = "retina"
)
