# Inform messages respecting the global AEME.inform option

Inform messages respecting the global AEME.inform option

## Usage

``` r
cli_inform_safe(..., .envir = parent.frame())
```

## Arguments

- ...:

  arguments passed to cli_inform_safe()

- .envir:

  environment in which to evaluate
  [`{}`](https://rdrr.io/r/base/Paren.html) expressions in the message.
  Defaults to the calling environment, matching
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).
  Forwarded explicitly because otherwise `cli` would interpolate against
  this wrapper's frame, where the caller's locals do not exist.
