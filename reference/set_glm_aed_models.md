# Set GLM-AED Models

Set the biogeochemical models to be used in a GLM-AED configuration
file.

## Usage

``` r
set_glm_aed_models(
  aeme,
  path,
  aed_models = c("aed_sedflux", "aed_oxygen", "aed_silica", "aed_nitrogen",
    "aed_phosphorus", "aed_organic_matter", "aed_phytoplankton", "aed_zooplankton",
    "aed_macrophyte", "aed_totals"),
  file = NULL,
  nml = NULL
)
```

## Arguments

- aeme:

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

- aed_models:

  Character vector of GLM-AED models to include. Default includes all
  available AED models: "aed_sedflux", "aed_oxygen", "aed_silica",
  "aed_nitrogen", "aed_phosphorus", "aed_organic_matter",
  "aed_phytoplankton", "aed_zooplankton", and "aed_macrophyte".

- file:

  Path to the GLM-AED configuration file. If NULL, the function will
  attempt to locate the file based on the provided Aeme object and path.

- nml:

  GLM-AED nml object. If provided, the function will modify this object
  directly instead of reading from a file.

## Value

If `nml` is provided, returns the modified nml object. Otherwise,
returns the input Aeme object with the updated GLM-AED configuration
file.
