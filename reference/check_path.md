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
#> Created missing directory: D:\a\AEME\AEME\docs\reference\aeme
#> [1] "D:\\a\\AEME\\AEME\\docs\\reference\\aeme"
```
