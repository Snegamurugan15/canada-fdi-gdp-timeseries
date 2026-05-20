# Canadian FDI GDP Time Series Modeling

This repository is intentionally **R-only** because the original project was an R/econometrics workflow. It analyzes the relationship between Canadian foreign direct investment and GDP using interpolation, factor selection, time-series modeling, and VAR/VECM-style analysis.

## Project Purpose

The goal is to understand whether foreign direct investment and GDP move together over time, how lagged relationships can support forecasting, and how sector-level investment changes can inform policy or economic planning.

## R Workflow

- `r/FDI_data_Interpolation.R` - original interpolation workflow.
- `r/Factor_selection.R` - original factor-selection script.
- `r/Prediction_model.R` - original prediction/modeling script.
- `r/run_fdi_gdp_analysis.R` - cleaned portfolio analysis runner using ARIMA and VAR.
- `r/shiny_app.R` - optional R Shiny interface for exploring the time series.

## Data

- `data/FDI - proposal data.csv` - original proposal-stage FDI data.
- `data/GDP - proposal data.csv` - original proposal-stage GDP data.
- `data/fdi_gdp_sample.csv` - compact public sample for reproducible Shiny and analysis runs.

## Install R Packages

```r
source("requirements.R")
```

## Run Analysis

```r
source("r/run_fdi_gdp_analysis.R")
```

## Run Shiny App

```r
shiny::runApp("r/shiny_app.R")
```

## Portfolio Note

No Python dashboard is included here. Keeping this repository R-based better represents the original project and shows range beyond Python-only work.
