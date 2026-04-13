# write GLM .nml for a GLM simulation

Creates a .nml file running a GLM simulation.  

## Usage

``` r
write_nml(glm_nml, file)
```

## Source

glmtools package: https://github.com/GLEON/glmtools

## Arguments

- glm_nml:

  a nml (a list) for GLM config

- file:

  a string with the path to the glm2.nml file to write

## See also

[get_nml_value](get_nml_value.md), [read_nml](read_nml.md)

## Author

Jordan S. Read

## Examples

``` r
if (FALSE) { # \dontrun{
glm_nml <- read_nml()
write_path <- paste0(tempdir(),'glm2.nml')
write_nml(glm_nml, file = write_path)
print(read_nml(write_path))
} # }
```
