# Plot observations

Plot observations

## Usage

``` r
plot_obs(aeme, var_sim = "HYD_temp", add_line = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- var_sim:

  string; of variable to plot

- add_line:

  logical, add line to the plot

## Value

ggplot object

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
plot_obs(aeme = aeme, var_sim = c("HYD_temp", "LKE_lvlwtr"))
```
