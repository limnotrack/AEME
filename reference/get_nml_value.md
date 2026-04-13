# gets a nml value according to an arg_name

This function returns an nml value according to the arg_name nml list
for GLM.

## Usage

``` r
get_nml_value(glm_nml = NA, arg_name, nml_file = NA, ...)
```

## Arguments

- glm_nml:

  a nml (a list) for GLM config

- arg_name:

  a string representing a valid field in glm_nml

- nml_file:

  a string with the path to the GLM glm2.nml file or `'template'` for
  loading the GLM template nml file with GLM3r (default)

- ...:

  additional arguments passed to `get_block`, such as warn=TRUE

## Value

arg_val value for the valid field in glm_nml specified by `arg_name`

## See also

[read_nml](read_nml.md), [set_nml](set_nml.md)

## Author

Jordan S. Read
