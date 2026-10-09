# Inform messages respecting the global AEME.inform option

Used primarily as an internal helper to safely suppress messages to
console. Messages are printed if the global option is set to TRUE:
`options(AEME.inform = TRUE)`

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

  environment in which to evaluate
  [`{}`](https://rdrr.io/r/base/Paren.html) expressions in the message.
  Defaults to the calling environment; forwarded to `FUN` when it
  accepts a `.envir` argument so interpolation sees the caller's locals
  rather than this wrapper's frame.
