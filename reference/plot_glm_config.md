# Visualise GLM-AED model configuration

Reads the parsed GLM and AED configuration lists from an AEME object and
produces an interactive HTML visualisation showing the lake hypsograph,
sediment zones, module wiring, and key parameter values.

## Usage

``` r
plot_glm_config(aeme, path, output = NULL)
```

## Arguments

- aeme:

  An AEME object.

- path:

  Character. Path to AEME project.

- output:

  Character or NULL. Path for the output HTML file. If NULL, a temporary
  file is created and opened in the browser. Defaults to NULL.

## Value

Invisibly returns the path to the generated HTML file.
