# Plot inflows and/or outflows

Plot inflows and/or outflows

## Usage

``` r
plot_flows(aeme, flow = c("inflow", "outflow"), var_sim = "HYD_flow")
```

## Arguments

- aeme:

  aeme; object.

- flow:

  character vector; "inflow", "outflow" or both. Default is both. For
  outflow, var_sim must be "HYD_flow".

- var_sim:

  character; column name in the data frame. Default is "HYD_flow".

## Value

A ggplot object

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
plot_flows(aeme = aeme)
```
