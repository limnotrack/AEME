#' Set knitr Hooks for ANSI Aware HTML Output
#'
#' Configures [knitr][knitr::knitr-package] output hooks to convert ANSI
#' sequences to HTML. This enables styled [cli] output to render correctly
#' in HTML documents produced by [knitr], [R Markdown][rmarkdown::rmarkdown],
#' or [Quarto](https://quarto.org/).
#'
#' @param hooks The [knitr::knit_hooks] object.
#' @param which Character vector of hooks to replace. Defaults to
#'   `c("output", "message", "warning", "error")`.
#' @param class Character vector of CSS classes for output blocks, recycled
#'   to match `which`. Defaults to `"ansi"`.
#' @param style Whether to emit CSS styles from [cli::ansi_html_style()].
#'   Defaults to `TRUE`.
#' @param collapse Whether to emit CSS to remove margins between consecutive
#'   output blocks. Defaults to `TRUE`.
#' @param palette Palette for [cli::ansi_html_style()]. Defaults to `"vscode"`.
#' @param colors Number of ANSI colors to set via [cli::num_ansi_colors()].
#'   Defaults to `256`. Set to `NULL` to leave unchanged.
#' @param preserve_newlines Whether to convert newlines to `<br>` tags to
#'   ensure they are preserved in the HTML output. Defaults to `FALSE`.
#'   Set to `TRUE` if your output is missing line breaks.
#'
#' @return
#' Invisibly returns the previous hooks as a named list.
#'
#' @details
#' The hook only processes output containing ANSI sequences (detected via
#' [cli::ansi_has_any()]). Other output is passed to the original hook unchanged.
#'
#' For non-HTML output formats, ANSI sequences are stripped via [cli::ansi_strip()].
#'
#' @seealso
#' [cli::ansi_html()], [cli::ansi_html_style()], [cli::ansi_strip()]
#'
#' @export
ansi_set_knit_hooks <- function(
    hooks,
    which = c("output", "message", "warning", "error"),
    class = "ansi",
    style = TRUE,
    collapse = TRUE,
    palette = "vscode",
    colors = 256,
    preserve_newlines = FALSE
) {
  if (
    !is.list(hooks) ||
    !is.function(hooks[["get"]]) ||
    !is.function(hooks[["set"]])
  ) {
    cli::cli_abort(
      "{.arg hooks} must be {.fn knitr::knit_hooks} or a compatible object
       with {.field $get} and {.field $set} methods."
    )
  }
  
  valid_hooks <- c("output", "message", "warning", "error")
  if (!is.character(which) || !all(which %in% valid_hooks)) {
    cli::cli_abort(
      "{.arg which} must contain values from
       {.or {.val {valid_hooks}}}, not {.val {setdiff(which, valid_hooks)}}."
    )
  }
  
  class <- rep_len(class, length(which))
  
  if (!is.null(colors)) {
    options(cli.num_colors = colors, crayon.enabled = TRUE)
  }
  
  is_html <- isTRUE(getOption("knitr.in.progress")) &&
    requireNamespace("knitr", quietly = TRUE) &&
    knitr::is_html_output()
  
  old_hooks <- hooks$get(which)
  new_hooks <- Map(
    function(old_hook, cls) {
      force(old_hook)
      force(cls)
      if (is_html) {
        function(x, options) {
          if (!nzchar(trimws(cli::ansi_strip(x)))) {
            return(NULL)
          }
          if (!cli::ansi_has_any(x)) {
            return(old_hook(x, options))
          }
          html <- cli::ansi_html(x)
          if (preserve_newlines) {
            html <- gsub("\n", "<br>\n", html, fixed = TRUE)
          }
          sprintf('<pre class="%s"><code>%s</code></pre>', cls, html)
        }
      } else {
        function(x, options) {
          x <- cli::ansi_strip(x)
          if (!nzchar(trimws(x))) {
            return(NULL)
          }
          old_hook(x, options)
        }
      }
    },
    old_hooks,
    class
  )
  names(new_hooks) <- which
  do.call(hooks$set, new_hooks)
  
  if (is_html && (style || collapse)) {
    css <- character()
    if (style) {
      css <- c(css, format(cli::ansi_html_style(palette = palette)))
    }
    if (collapse) {
      css <- c(
        css,
        sprintf(
          paste0(
            "pre.%s { margin: 0; padding: 0; }\n",
            "pre.%s + pre.%s { border-top: none; }\n",
            ".cell-output-stderr { margin: 0; padding: 0; }\n",
            ".cell-output-stderr pre { margin: 0; padding: 0; }"
          ),
          class[1L],
          class[1L],
          class[1L]
        )
      )
    }
    writeLines(c("<style>", css, "</style>"))
  }
  
  invisible(old_hooks)
}
