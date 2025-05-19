# Power Consumption Analysis Using Bayesian Hierarchical Modeling

## Project Overview
This project analyzes power consumption data collected from multiple zones over time to understand how environmental factors and temporal patterns affect electricity usage. A Bayesian hierarchical regression model was developed using the `brms` package in R, incorporating predictors like temperature, humidity, month, weekend indicator, and zone-level random effects.

## Dataset
- Source: UCI Machine Learning Repository (provide link)
- Description: Hourly power consumption data from three different zones along with weather and temporal features.
- Size: 157,248 observations with variables including Temperature, Humidity, Month, Weekend, Zone, and Power consumption.

## Methodology
- **Data Cleaning & Feature Engineering:** Performed in R including scaling continuous variables and encoding categorical features like Month and Weekend.
- **Exploratory Data Analysis (EDA):** Conducted correlation analysis, seasonal pattern visualization, and trend identification.
- **Modeling:** Used Bayesian hierarchical linear regression (`brms`) with:
  - Gaussian family likelihood
  - Predictors: Temperature_scaled, Humidity_scaled, Month_num, Weekend
  - Random intercepts for Zone
  - MCMC sampling with NUTS algorithm, 4 chains, 4000 iterations, warmup 1000
- **Model Diagnostics:** Checked convergence via R-hat, Effective Sample Size (ESS), and traceplots; addressed divergent transitions by tuning control parameters.

## Results
- Temperature positively associated with power consumption; humidity and month showed negative effects.
- The model successfully captured zone-level variations through random intercepts.
- Convergence diagnostics indicate reliable estimates (R̂ close to 1, high ESS).
- Posterior predictive checks support good model fit.

## How to Use
1. Load dataset `df_long` into R.
2. Run the data preprocessing scripts (`data_cleaning.R`).
3. Fit the model using `brms` as shown in `model_fit.R`.
4. Review diagnostics and plots saved in the `/plots` directory.
5. Explore posterior summaries using `summary(model)`.

## Requirements
- R (version >= 4.0)
- Packages: `brms`, `tidyverse`, `bayesplot`, `cmdstanr` (optional)

## References
- Dataset Source: [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/YourDatasetName)
- Bayesian Modeling: [brms Package](https://paul-buerkner.github.io/brms/)
- MCMC Diagnostics: [Stan Warnings and Troubleshooting](https://mc-stan.org/misc/warnings.html)
