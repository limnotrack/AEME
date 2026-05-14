# Set knitr Hooks for ANSI Aware HTML Output

Configures [knitr](https://rdrr.io/pkg/knitr/man/knitr-package.html)
output hooks to convert ANSI sequences to HTML. This enables styled
[cli](https://cli.r-lib.org/reference/cli.html) output to render
correctly in HTML documents produced by
[knitr](https://rdrr.io/pkg/knitr/man/knitr-package.html), [R
Markdown](https://pkgs.rstudio.com/rmarkdown/reference/rmarkdown-package.html),
or [Quarto](https://quarto.org/).

## Usage

``` r
ansi_set_knit_hooks(
  hooks,
  which = c("output", "message", "warning", "error"),
  class = "ansi",
  style = TRUE,
  collapse = TRUE,
  palette = "vscode",
  colors = 256,
  preserve_newlines = FALSE
)
```

## Arguments

- hooks:

  The [knitr::knit_hooks](https://rdrr.io/pkg/knitr/man/knit_hooks.html)
  object.

- which:

  Character vector of hooks to replace. Defaults to
  `c("output", "message", "warning", "error")`.

- class:

  Character vector of CSS classes for output blocks, recycled to match
  `which`. Defaults to `"ansi"`.

- style:

  Whether to emit CSS styles from
  [`cli::ansi_html_style()`](https://cli.r-lib.org/reference/ansi_html_style.html).
  Defaults to `TRUE`.

- collapse:

  Whether to emit CSS to remove margins between consecutive output
  blocks. Defaults to `TRUE`.

- palette:

  Palette for
  [`cli::ansi_html_style()`](https://cli.r-lib.org/reference/ansi_html_style.html).
  Defaults to `"vscode"`.

- colors:

  Number of ANSI colors to set via
  [`cli::num_ansi_colors()`](https://cli.r-lib.org/reference/num_ansi_colors.html).
  Defaults to `256`. Set to `NULL` to leave unchanged.

- preserve_newlines:

  Whether to convert newlines to `<br>` tags to ensure they are
  preserved in the HTML output. Defaults to `FALSE`. Set to `TRUE` if
  your output is missing line breaks.

## Value

Invisibly returns the previous hooks as a named list.

## Details

The hook only processes output containing ANSI sequences (detected via
[`cli::ansi_has_any()`](https://cli.r-lib.org/reference/ansi_has_any.html)).
Other output is passed to the original hook unchanged.

For non-HTML output formats, ANSI sequences are stripped via
[`cli::ansi_strip()`](https://cli.r-lib.org/reference/ansi_strip.html).

## See also

[`cli::ansi_html()`](https://cli.r-lib.org/reference/ansi_html.html),
[`cli::ansi_html_style()`](https://cli.r-lib.org/reference/ansi_html_style.html),
[`cli::ansi_strip()`](https://cli.r-lib.org/reference/ansi_strip.html)
