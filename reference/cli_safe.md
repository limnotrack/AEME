# Inform messages respecting the global AEME.inform option

Inform messages respecting the global AEME.inform option

## Usage

``` r
cli_safe(..., FUN = cli::cli_bullets, indent = TRUE, .envir = parent.frame())
```

## Arguments

- ...:

  arguments passed to cli_inform_safe()

- FUN:

  function to use for messaging, default is cli::cli_inform

- indent:

  logical, whether to indent the message, default is FALSE

- .envir:

  Environment to evaluate the glue expressions in.
