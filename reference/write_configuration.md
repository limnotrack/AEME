# Write model configuration from the aeme object

Write model configuration from the aeme object

## Usage

``` r
write_configuration(aeme, model, path)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

## Value

aeme object which was passed to the function,
