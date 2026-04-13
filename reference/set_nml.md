# sets values in nml object

This function sets values in nml object for GLM config.

## Usage

``` r
set_nml(glm_nml, arg_name, arg_val, arg_list = NULL)
```

## Source

glmtools package: https://github.com/GLEON/glmtools

## Arguments

- glm_nml:

  a nml (a list) for GLM config

- arg_name:

  a string representing a valid field in glm_nml

- arg_val:

  value for the valid field in glm_nml specified by `arg_name`

- arg_list:

  a list made up of valid `arg_name`s and `arg_val`s

## Value

glm_nml a modified nml

## See also

[read_nml](read_nml.md)

## Author

Jordan S. Read

## Examples

``` r
if (FALSE) { # \dontrun{
sim_folder <- run_example_sim(verbose = FALSE)
nml_file <- file.path(sim_folder, 'glm3.nml')
glm_nml <- read_nml(nml_file)
get_nml_value(glm_nml, arg_name = 'Kw')
glm_nml <- set_nml(glm_nml, arg_name = 'Kw', arg_val = 1.4)
glm_nml <- set_nml(glm_nml, arg_list = list('Kw' = 1.4))
print(glm_nml)
} # }
```
