# Assess model performance

Assess model performance

## Usage

``` r
assess_model(aeme, model, var_sim)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- var_sim:

  string; of variable to plot

## Value

Data frame with model performance statistics for each model and
variable. These include:

- bias - Bias

- mae - Mean absolute error

- rmse - Root mean square error

- nmae - Normalised mean absolute error

- nse - Nash-Sutcliffe efficiency

- d2 - Index of agreement model skill score Willmott index

- r - Pearson correlation coefficient

- rs - Spearman correlation coefficient

- r2 - R-squared value from linear model

- B - Bardsley coefficient

- n - number of observations
