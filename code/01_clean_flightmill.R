# ============================================================================
# 01_clean_flightmill.R
#
# Combine raw flight-mill CSV output, attach metadata, and clean to the form
# used in the manuscript:
#
#   Birrell, Walker & Holwell. Sexual dimorphism, allometry, and flight
#   performance in the scramble competitor Prionoplus reticularis White
#   (Coleoptera: Cerambycidae).
#
# Output: data/df_flightmill_output.RDS
#
# Cleaning steps follow the Methods section "Data Processing":
#   - drop date stamps outside the experimentation window
#   - group by date-time, summarise to revolutions per second
#   - filter out rps > 3 (double the observed video maximum of 1.6 rps)
# ============================================================================

# Packages --------------------------------------------------------------------

library(tidyverse)
library(here)
library(data.table)
library(lubridate)


# Combine raw CSVs ------------------------------------------------------------

# Each CSV contains one night of flight-mill output. We need to combine them
# all and tag each row with its source filename so we can match it to the
# metadata sheet (which records which beetle was on which mill on which date).

all_paths <- list.files(
  path = here::here("data", "raw", "csvFiles"),
  pattern = "*.csv",
  full.names = TRUE
)

all_content <- lapply(all_paths, read_csv, col_names = FALSE)

all_filenames <- as.list(basename(all_paths))

all_lists <- mapply(c, all_content, all_filenames, SIMPLIFY = FALSE)

millData <- rbindlist(all_lists)
colnames(millData) <- c("millID", "unixTime", "dateTime", "fileName")


# Attach metadata -------------------------------------------------------------

# Metadata sheet maps (millID, fileName) to a beetle id and sex.

metadata <- read.csv(
  here::here("data", "raw", "huhuFlightMillMetaData.csv"),
  header = TRUE
)[, c("millID", "fileName", "iD", "sex")] |>
  # Beetle 133 has no sex recorded in metadata. Excluded here to keep
  # downstream sex-based analyses clean (no empty third group in tests).
  dplyr::filter(sex %in% c("m", "f"))

all_data_df <- inner_join(millData, metadata, by = c("fileName", "millID"))


# Flight-mill physical dimensions --------------------------------------------

# Radius of each of the six flight mills, measured in mm. Used downstream to
# convert revolutions to distance flown.

mill_dimensions <- data.frame(
  millID = 1:6,
  radii = c(129.23, 141.30, 137.37, 125.81, 129.88, 132.09)
)


# Tidy and filter -------------------------------------------------------------

# Drop rows outside the experimental window (a few stray timestamps from
# clock-reset events) and summarise to revolutions per second.

all_data_tidy <- all_data_df |>
  select(millID, iD, sex, dateTime) |>
  group_by(millID, iD, sex, dateTime) |>
  filter(dateTime <= as.POSIXct("2022-02-06 22:14:17")) |>
  filter(dateTime >= as.POSIXct("2021-11-22 14:43:35")) |>
  summarise(revolutions = n(), .groups = "drop_last") |>
  # Maximum observed rps from video was 1.6; we use 3 as a generous upper
  # bound, above which the magnet has likely come to rest on the sensor.
  filter(revolutions <= 3)


# Compute per-individual flight summaries -------------------------------------

# Identify continuous flight bouts (gaps > 1 s define a new interval), then
# summarise per beetle.

df_flightmill_output <- all_data_tidy |>
  mutate(
    dateTime = as.POSIXct(dateTime),
    time_difference = c(0, diff(dateTime)),
    is_new_interval = time_difference > 1,
    interval_id = cumsum(is_new_interval)
  ) |>
  group_by(millID, iD, sex, interval_id) |>
  summarise(
    start_time = first(dateTime),
    end_time = last(dateTime),
    # +1 to avoid zero-duration single-revolution intervals
    duration = as.numeric(difftime(end_time, start_time, units = "secs") + 1),
    revolutions = sum(revolutions),
    rps = revolutions / duration,
    .groups = "drop"
  ) |>
  left_join(mill_dimensions, by = join_by(millID)) |>
  mutate(
    circumference = 2 * pi * radii,
    distance = revolutions * circumference,
    mps = (distance / 1000) / duration
  ) |>
  group_by(millID, iD, sex) |>
  summarise(
    average_rps = mean(rps),
    sd_rps = sd(rps),
    max_rps = max(rps),
    min_rps = min(rps),
    average_distance = mean(distance),
    sd_distance = sd(distance),
    max_distance = max(distance),
    min_distance = min(distance),
    average_mps = mean(mps),
    sd_mps = sd(mps),
    max_mps = max(mps),
    min_mps = min(mps),
    average_duration = mean(duration),
    sd_duration = sd(duration),
    max_duration = max(duration),
    min_duration = min(duration),
    .groups = "drop"
  )


# Save ------------------------------------------------------------------------

write_rds(df_flightmill_output, here::here("data", "df_flightmill_output.RDS"))
