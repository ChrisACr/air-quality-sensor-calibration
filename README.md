# Air Quality Sensor Calibration

Sensor calibration and time drift analysis of urban air quality sensors using R.

## Overview

Low-cost metal oxide gas sensors are widely used in air quality monitoring but suffer 
from cross-sensitivity to co-pollutants and environmental conditions, as well as 
gradual performance degradation over time (sensor drift). This project builds and 
evaluates statistical calibration models that map raw sensor output to reference-grade 
CO concentrations, and assesses whether the sensor's calibration relationship remained 
stable over the deployment period.

## Data

This project uses the UCI Air Quality dataset collected at a roadside station in Italy 
between March 2004 and April 2005.

Download the dataset here:  
https://archive.ics.uci.edu/dataset/360/air+quality

Place `AirQualityUCI.xlsx` in the `data/` folder before running the code.

## Requirements

R with the following packages:
- `readxl`
- `dplyr`
- `hms`
- `ggplot2`
- `GGally`
- `gridExtra`
- `glmnet`
- `mgcv`

## Project Structure

air-quality-sensor-calibration/
├── code/
│   └── sensor-calibration.R   # full analysis pipeline
├── data/
│   └── README.md              # download instructions
├── plots/                     # exported visualizations
├── report/
│   └── report.pdf             # full project report
└── README.md

## Methods

Models were evaluated using RMSE on a chronological 70/30 train-test split:

- Linear regression (with and without cyclical time features)
- Ridge and LASSO regression
- PCA regression
- Generalized Additive Model (GAM)

All models were fit with and without lagged CO response features (lag1, lag2, lag3), 
motivated by strong residual autocorrelation identified in ACF diagnostics.

## Key Findings

- Best model: linear regression with 3 lag features (RMSE = 0.493)
- 65% reduction in error from the naive mean baseline (RMSE = 1.414)
- No statistically significant sensor drift detected (p = 0.37)
- Primary CO sensor coefficient stable within 4.1% across deployment period
- Persistent residual autocorrelation suggests ARIMA-type modeling as future work

## Report

See `report/report.pdf` for the full write-up including drift analysis, coefficient 
stability test, and model comparison.
