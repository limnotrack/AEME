# View the AED module interaction diagram

Opens an interactive HTML diagram of the interactions between the
modules of the Aquatic EcoDynamics (AED) library used by GLM-AED and
Simstrat-AED2. The diagram is shown in the RStudio/Positron viewer pane
when available, otherwise in the default web browser.

## Usage

``` r
view_aed_modules(browser = FALSE, output = NULL)
```

## Arguments

- browser:

  Logical; if `TRUE`, always open in the web browser instead of the
  viewer pane. Default is `FALSE`.

- output:

  Character; optional path to copy the HTML file to. If `NULL`
  (default), a temporary file is used.

## Value

Invisibly returns the path to the HTML file.

## Examples

``` r
if (FALSE) { # \dontrun{
view_aed_modules()
view_aed_modules(browser = TRUE)
} # }
```
