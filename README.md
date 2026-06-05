# Code for Birrell, Walker & Holwell — *Prionoplus reticularis* flight & allometry

R code accompanying:

> Birrell, N. W., Walker, L. A., & Holwell, G. I. Sexual dimorphism,
> allometry, and flight performance in the scramble competitor
> *Prionoplus reticularis* White (Coleoptera: Cerambycidae).

## Data availability
The raw flight-mill CSV files, morphological measurements, and metadata required to run this pipeline are available from the corresponding author on request. Once received, place them in the data/ directory following the layout described below.

## Software

- R (version 4.5.3; R Core Team, 2026)
- Key packages:
  - `data.table` (version 1.18.4; Barrett T et al., 2026)
  - `dplyr` (version 1.2.1; Wickham H et al., 2026)
  - `flextable` (version 0.9.12; Gohel D, Skintzos P, 2026)
  - `forcats` (version 1.0.1; Wickham H, 2025)
  - `ggplot2` (version 4.0.3; Wickham H, 2016)
  - `here` (version 1.0.2; Müller K, 2025)
  - `lubridate` (version 1.9.5; Grolemund G, Wickham H, 2011)
  - `patchwork` (version 1.3.2; Pedersen T, 2025)
  - `performance` (version 0.17.0; Lüdecke D et al., 2021)
  - `purrr` (version 1.2.2; Wickham H, Henry L, 2026)
  - `readr` (version 2.2.0; Wickham H et al., 2026)
  - `smatr` (version 3.4.8; Warton DI et al., 2012)
  - `stringr` (version 1.6.0; Wickham H, 2025)
  - `tibble` (version 3.3.1; Müller K, Wickham H, 2026)
  - `tidyr` (version 1.3.2; Wickham H et al., 2025)
  - `tidyverse` (version 2.0.0; Wickham H et al., 2019)

Installation:

This project uses `renv` for reproducible package management. To restore
the exact package versions used:

```r
renv::restore()
```

## How to run

The scripts are numbered and must be run in order. Each one depends on
objects created by the previous.

```r
source("01_clean_flightmill.R")   # produces data/df_flightmill_output.RDS
source("02_allometry.R")          # Table 1, Table 2, Figure 4
source("03_flight_models.R")      # Table 3, Figure 5 (uses objects from 02)
source("4_tables.R")              # formatted .docx tables (uses objects from 02 & 03)
```

## What each script does

### `01_clean_flightmill.R`

Combines the raw nightly flight-mill CSVs, joins them to the metadata
sheet that records which beetle was on which mill, and applies the
cleaning steps described in the Methods section "Data Processing":

- drops timestamps outside the experimental window;
- groups by date-time and summarises to revolutions per second;
- filters out `rps > 3` (the observed video maximum was 1.6 rps; values
  above 3 indicate the magnet has come to rest on the Hall-effect sensor);
- computes per-beetle summaries of distance, duration, speed, and rps.

Output: `data/df_flightmill_output.RDS`.

### `02_allometry.R`

Joins the cleaned flight-mill data with the morphological measurements
and produces:

- **Table 1** descriptive stats (`output/dimorph_summary_table.csv`) and
  Welch t-tests with Benjamini-Hochberg-adjusted p-values
  (`output/dimorph_stats_table.csv`).
- **Figure 3**: 2 × 3 panel of log-log allometric plots for tibia,
  antenna, wing aspect ratio, ninth flagellomere, wing area, and wing
  length, with isometry lines through the data centroid
  (`output/plot_dimorph.png`).
- **Table 2** `smatr::sma()` OLS slope tests against isometry for both
  sexes across all traits (`output/allometry_slopes.csv`).

### `03_flight_models.R`

Tests sexual dimorphism in flight metrics (Levene then Kruskal-Wallis,
following the manuscript), then fits three Gamma GLMs with a log link
predicting male flight distance, duration, and speed from pronotum
width, wing aspect ratio, and wing loading. Diagnostics via
`performance::check_model()`.

Produces **Table 3** exported coefficient tables
(`output/table_model_distance.csv`, `output/table_model_duration.csv`,
`output/table_model_speed.csv`), plain-text model reports
(`output/report_model_*.txt`), and **Figure 4** marginal-effect plots
of wing loading on each flight metric (`output/plot_model_flight_perf.png`).

### `4_tables.R`

Formats Tables 1–3 as publication-ready Word documents using `flextable`.
Requires objects created by `02_allometry.R` and `03_flight_models.R` to
be present in the session. Outputs:

- `output/tables/table1_dimorphism.docx`
- `output/tables/table2_allometry.docx`
- `output/tables/table3_flight_glm.docx` (contains Table 3 and Table 3b,
  the companion model-fit statistics)

## Expected directory layout

```
project/
├── code/
│   ├── 01_clean_flightmill.R
│   ├── 02_allometry.R
│   ├── 03_flight_models.R
│   └── 4_tables.R
├── data/
│   ├── df_flightmill_output.RDS         (created by 01)
│   ├── raw/
│   │   ├── csvFiles/                    (nightly flight-mill CSVs)
│   │   └── huhuFlightMillMetaData.csv
│   └── allometry/
│       ├── measurements.csv
│       └── wing_dimensions.csv
└── output/
    ├── dimorph_summary_table.csv        (Table 1, descriptives)
    ├── dimorph_stats_table.csv          (Table 1, t-tests)
    ├── allometry_slopes.csv             (Table 2)
    ├── plot_dimorph.png                 (Figure 3)
    ├── table_model_distance.csv         (Table 3, distance)
    ├── table_model_duration.csv         (Table 3, duration)
    ├── table_model_speed.csv            (Table 3, speed)
    ├── report_model_distance.txt
    ├── report_model_duration.txt
    ├── report_model_speed.txt
    ├── plot_model_flight_perf.png       (Figure 4)
    └── tables/
        ├── table1_dimorphism.docx
        ├── table2_allometry.docx
        └── table3_flight_glm.docx
```

The `here` package is used throughout for path resolution; the project
root is wherever `here::here()` finds the `.here` file or `.Rproj` file.
