# Check and manage file paths

Check and manage file paths

## Usage

``` r
check_path(path, create = FALSE, must_exist = FALSE)
```

## Arguments

- path:

  Character string specifying the file path to check.

- create:

  Logical indicating whether to create the directory if it does not
  exist. Default is FALSE.

- must_exist:

  Logical indicating whether the directory must exist. If TRUE and the
  directory does not exist, an error is thrown. Default is FALSE.

## Value

Normalized file path as a character string.

## Examples

``` r
check_path("aeme", create = TRUE)
#> Error in "lapply(text, glue_cmd, .envir = .envir)": ! Could not evaluate cli `{}` expression: `path`.
#> Caused by error in `eval(expr, envir = envir)`:
#> ! object 'path' not found
```
