# Write a yaml object to file

Write the YAML representation of an R object (list) to a file. Taken
from the `yaml` package but added catch to replace 'yes' with 'true' and
'no' with 'false' and '~' with 'null' to make it compatible with running
the GOTM model.

## Usage

``` r
write_yaml(x, file, fileEncoding = "UTF-8", ...)
```

## Arguments

- x:

  The object to be converted.

- file:

  Either a character string naming a file or a
  [connection](https://rdrr.io/r/base/connections.html) open for
  writing.

- fileEncoding:

  Character string: if non-empty declares the encoding to be used on a
  file (not a connection) so the character data can be re-encoded as
  they are written. See
  [`file()`](https://rdrr.io/r/base/connections.html).

- ...:

  Arguments to
  [`as.yaml()`](https://yaml.r-lib.org/reference/as.yaml.html).

## Author

Jeremy Stephens <jeremy.f.stephens@vumc.org>, Tadhg Moore

## Examples

``` r

if (FALSE) { # \dontrun{
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
path <- file.path(tmpdir, "lake")
yaml <- yaml::read_yaml(file.path(path, "aeme.yaml"))

# Set inflows and outflows to NULL
yaml$inflows$data <- NULL
yaml$outflows$data <- NULL

write_yaml(yaml, file.path(path, "aeme_simple.yaml"))
} # }
```
