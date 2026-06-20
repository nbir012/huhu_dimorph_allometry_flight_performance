# ============================================================================
# 02_allometry.R
#
# Sexual dimorphism (Table 1) and trait allometry (Table 2, Figure 3) for
# Prionoplus reticularis.
#
# Reads:
#   - data/df_flightmill_output.RDS (from 01_clean_flightmill.R)
#   - data/raw/huhuFlightMillMetaData.csv
#   - data/allometry/wing_dimensions.csv
#   - data/allometry/measurements.csv
#
# Outputs:
#   - output/dimorph_summary_table.csv  (Table 1, descriptives)
#   - output/dimorph_stats_table.csv    (Table 1, t-test results)
#   - output/plot_dimorph.png           (Figure 3)
#   - output/allometry_slopes.csv       (Table 2)
# ============================================================================

# Packages --------------------------------------------------------------------

library(here)
library(tidyverse)
library(smatr)
library(patchwork)


# Load and join ---------------------------------------------------------------

df_flightmill <- read_rds(here::here("data", "df_flightmill_output.RDS")) |>
  rename(id = iD) |>
  # Drop `sex` here — it's already present in measurements.csv. Keeping
  # both would create sex.x / sex.y after the join.
  select(-sex)

df_flightmill_meta <- read.csv(
  here::here("data", "raw", "huhuFlightMillMetaData.csv")
) |>
  select(iD, weightStart, weightFinish) |>
  rename(id = iD)

df_wing_length <- read_csv(
  here::here("data", "allometry", "wing_dimensions.csv"),
  col_names = TRUE,
  col_select = c(-file)
)

# Trait measurements (wide format), with units converted to mm / mg and
# derived wing metrics added.
allometrySlopeWide <- read_csv(
  here::here("data", "allometry", "measurements.csv"),
  col_names = TRUE,
  col_select = c(-file, -notes)
) |>
  # measurements.csv stores lengths/widths in cm; convert to mm BEFORE
  # joining wing_dimensions.csv, which is already in mm.
  mutate(across(starts_with("length"), ~ . * 10)) |>
  mutate(across(starts_with("width"), ~ . * 10)) |>
  left_join(df_wing_length, by = join_by(id)) |>
  # Two wing-length columns post-join; keep the humeral-plate-to-RA4 one.
  select(-lengthWing.x) |>
  rename(lengthWing = lengthWing.y) |>
  left_join(df_flightmill, by = join_by(id)) |>
  left_join(df_flightmill_meta, by = join_by(id)) |>
  # Convert g to mg.
  mutate(
    weightStart_mg = weightStart * 1000,
    weightFinish_mg = weightFinish * 1000
  ) |>
  # Derived wing metrics. Wing loading uses total wing area (both wings);
  # aspect ratio uses single-wing length and area — matches manuscript
  # Table 1 (AR ~ 3.18 for males).
  mutate(
    wing_aspect_ratio = lengthWing^2 / areaWing,
    wing_loading_mg_mm2 = weightStart_mg / (2 * areaWing),
    sqrt_wing_area = sqrt(areaWing)
  )


# Long-format helper tables --------------------------------------------------

# For dimorphism summary stats: every numeric trait in one column.
df_allometry_long <- allometrySlopeWide |>
  select(-starts_with(c("mill", "sd", "max", "min")), -any_of("countSensf9")) |>
  pivot_longer(
    names_to = "measurement",
    cols = c(
      starts_with("width"),
      starts_with("length"),
      starts_with("area"),
      starts_with("wing"),
      starts_with("weight"),
      starts_with("average"),
      starts_with("sqrt")
    ),
    values_to = "mm"
  )

# For per-trait allometry plots against pronotum width.
df_allometry_long_PW <- allometrySlopeWide |>
  select(-starts_with(c("sd", "max", "min", "mill")), -any_of("countSensf9")) |>
  pivot_longer(
    cols = c(
      starts_with("length"),
      starts_with("area"),
      starts_with("wing"),
      starts_with("weight"),
      starts_with("average"),
      starts_with("sqrt")
    ),
    names_to = "measurement",
    values_to = "mm"
  )


# ----------------------------------------------------------------------------
# Table 1: descriptive stats and Welch t-tests with BH-adjusted p-values
# ----------------------------------------------------------------------------

dimorph_summary_table <- df_allometry_long |>
  filter(!grepl("^(lengthF[1-8]|lengthPedicel|lengthScape)$", measurement)) |>
  group_by(sex, measurement) |>
  drop_na(mm) |>
  summarise(
    sample_size = n(),
    mean = mean(mm),
    sd = sd(mm),
    max = max(mm),
    min = min(mm),
    cv = sd(mm) / mean(mm) * 100,
    .groups = "drop"
  )

write.csv(
  dimorph_summary_table,
  file = here::here("output", "dimorph_summary_table.csv"),
  row.names = FALSE
)

dimorph_stats_table <- df_allometry_long |>
  filter(!grepl("^(lengthF[1-8]|lengthPedicel|lengthScape)$", measurement)) |>
  group_by(measurement) |>
  rstatix::t_test(mm ~ sex) |>
  rstatix::adjust_pvalue(method = "BH") |>
  rstatix::add_significance()

write.csv(
  dimorph_stats_table,
  file = here::here("output", "dimorph_stats_table.csv"),
  row.names = FALSE
)


# ----------------------------------------------------------------------------
# Figure 3: log-log allometric scatterplots, both sexes
#
# For each trait, fit a pooled OLS regression of log10(trait) on
# log10(pronotum width). Draw this pooled fit as a dashed line, then
# overlay sex-specific points and per-sex regression lines.
# ----------------------------------------------------------------------------

make_panel <- function(meas, ylab_str, iso_slope = NULL) {
  d <- df_allometry_long_PW |>
    filter(measurement == meas) |>
    drop_na(mm, widthPronotum, sex)

  # Reference dashed line: pooled OLS fit by default; if iso_slope is
  # supplied, an isometry line of that slope passing through the data
  # centroid instead (slope = 1 for linear traits, 0 for
  # dimensionless ratios).
  if (is.null(iso_slope)) {
    fit <- lm(log10(mm) ~ log10(widthPronotum), data = d)
    ref_int <- coef(fit)[1]
    ref_slope <- coef(fit)[2]
  } else {
    ref_slope <- iso_slope
    ref_int <- mean(log10(d$mm)) - ref_slope * mean(log10(d$widthPronotum))
  }

  d |>
    mutate(mm = log10(mm), widthPronotum = log10(widthPronotum)) |>
    ggplot(aes(x = widthPronotum, y = mm, color = sex, shape = sex)) +
    geom_smooth(method = "lm") +
    geom_point() +
    geom_abline(intercept = ref_int, slope = ref_slope, lty = 2) +
    ylab(ylab_str) +
    xlab("Pronotum width (log10(mm))") +
    theme_classic()
}

p_tibia <- make_panel("lengthTibia", "Tibia Length (log10(mm))", iso_slope = 1)
p_antenna <- make_panel(
  "lengthAntenna",
  "Antenna Length (log10(mm))",
  iso_slope = 1
)
p_aspect <- make_panel(
  "wing_aspect_ratio",
  "Wing Aspect Ratio (log10)",
  iso_slope = 0
)
p_f9 <- make_panel("lengthF9", "F9 Length (log10(mm))", iso_slope = 1)
p_area <- make_panel(
  "sqrt_wing_area",
  "\u221A Wing Area (log10(mm))",
  iso_slope = 1
)
p_wlength <- make_panel("lengthWing", "Wing Length (log10(mm))", iso_slope = 1)

plot_dimorph <- (p_tibia + p_antenna + p_aspect) /
  (p_f9 + p_area + p_wlength) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  filename = "plot_dimorph.png",
  plot = plot_dimorph,
  device = "png",
  path = here::here("output"),
  width = 10,
  height = 6.5,
  dpi = "retina"
)


# ----------------------------------------------------------------------------
# Table 2: OLS slopes on log10-transformed traits vs pronotum width
#
# For each trait, fit sma() with slope.test = 1 or 0 (i.e. isometry depending on dimension) on log-log
# axes for males and females separately. The smatr summary gives the slope,
# its CI, r^2 against the null of no correlation, and the test statistic for
# slope = 1 or 0.
#
# Wing area is square-root transformed first so that the isometric
# expectation against a linear body-size measure is slope = 1.
# ----------------------------------------------------------------------------

# Traits and the column name to use for the fit (sqrt-transformed for area).
# Traits, the column name to use for the fit (sqrt-transformed for area),
# and the isometric expectation slope:
#   - Linear traits (lengths)     : slope = 1
#   - sqrt(wing area)             : slope = 1 (because sqrt(area) has dim length)
#   - Wing aspect ratio (dimensionless ratio): slope = 0
#   - Wing loading (mass/area, ~length^1)    : slope = 1
traits <- tibble::tribble(
  ~trait              , ~column               , ~iso_slope ,
  "lengthAntenna"     , "lengthAntenna"       ,          1 ,
  "lengthF9"          , "lengthF9"            ,          1 ,
  "lengthFemur"       , "lengthFemur"         ,          1 ,
  "lengthTibia"       , "lengthTibia"         ,          1 ,
  "lengthWing"        , "lengthWing"          ,          1 ,
  "areaWing_sqrt"     , "sqrt_wing_area"      ,          1 ,
  "wing_aspect_ratio" , "wing_aspect_ratio"   ,          0 ,
  "wing_loading"      , "wing_loading_mg_mm2" ,          1
)

fit_sma_one <- function(trait, column, iso_slope, sex_filter, data) {
  d <- data |> filter(sex == sex_filter)
  fml <- as.formula(paste0("`", column, "` ~ widthPronotum"))
  fit <- smatr::sma(
    fml,
    data = d,
    slope.test = iso_slope,
    na.action = na.omit,
    log = "xy",
    method = "OLS"
  )

  # smatr returns coefficients as a 2-row data frame: row 1 = elevation
  # (intercept), row 2 = slope; column 1 = estimate, columns 2-3 = lower
  # and upper CI bounds. Index numerically so column-name changes between
  # smatr versions don't silently return NA.
  co <- fit$coef[[1]]
  st <- fit$slopetest[[1]]
  tibble(
    trait = trait,
    sex = sex_filter,
    n = fit$n[[1]],
    intercept = co[1, 1],
    slope = co[2, 1],
    slope_lo = co[2, 2],
    slope_hi = co[2, 3],
    r2 = fit$r2[[1]],
    p_r2 = fit$pval[[1]],
    iso_slope = iso_slope,
    r_vs_iso = st$r,
    p_vs_iso = st$p
  )
}

allometry_slopes <- traits |>
  crossing(sex = c("m", "f")) |>
  pmap_dfr(\(trait, column, iso_slope, sex) {
    fit_sma_one(trait, column, iso_slope, sex, allometrySlopeWide)
  })

write.csv(
  allometry_slopes,
  file = here::here("output", "allometry_slopes.csv"),
  row.names = FALSE
)
