# Set GLM-AED Models

Set the biogeochemical models to be used in a GLM-AED configuration
file.

## Usage

``` r
set_glm_aed_models(
  aeme,
  aed_models = c("aed_sedflux", "aed_oxygen", "aed_silica", "aed_nitrogen",
    "aed_phosphorus", "aed_organic_matter", "aed_phytoplankton", "aed_zooplankton",
    "aed_macrophyte"),
  path,
  file = NULL,
  nml = NULL
)
```

## Arguments

- aeme:

  aeme; object.

- aed_models:

  Character vector of GLM-AED models to include. Default includes all
  available AED models: "aed_sedflux", "aed_oxygen", "aed_silica",
  "aed_nitrogen", "aed_phosphorus", "aed_organic_matter",
  "aed_phytoplankton", "aed_zooplankton", and "aed_macrophyte".

- path:

  filepath; where input files are located relative to the current
  working directory.

- file:

  Path to the GLM-AED configuration file. If NULL, the function will
  attempt to locate the file based on the provided Aeme object and path.

- nml:

  GLM-AED nml object. If provided, the function will modify this object
  directly instead of reading from a file.

## Value

Modified GLM-AED nml object if \`nml\` is provided; otherwise, the
function writes the changes directly to the specified configuration
file.
