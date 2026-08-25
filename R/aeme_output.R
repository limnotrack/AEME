#' Tag a model-output list with its `aeme_output`/`aeme_output_raw` class
#'
#' All of [read_glm_output()], [read_gotm_output()], [read_simstrat_output()],
#' [read_dy_output()], and [read_model_outputs()] return the same list shape
#' (`Date`, `LKE_depths`, `LKE_lvlwtr`, per-variable matrices/vectors/
#' `aeme_grouped_var`s, `ok`, `reason`) -- the value actually stored in an
#' `Aeme` object's [output()]. Tagging it with a class (rather than relying
#' on callers to duck-type `is.list(x) && "Date" %in% names(x)`) lets
#' consumers like [plot_model_output()] recognise it reliably, and lets them
#' tell a standardised (AEME names/units/depth grid) list apart from a raw
#' one (native model names/units/depths, produced by `raw_output = TRUE`)
#' without re-deriving that from the variable names themselves.
#'
#' @param out_list list; the model output list, fully built.
#' @param model character; the model that produced it, e.g. `"glm_aed"`.
#' @param raw logical; was this produced with `raw_output = TRUE`? Default
#'   `FALSE`.
#'
#' @return `out_list`, with class `c("aeme_output_raw", "list")` or
#'   `c("aeme_output", "list")` and a `model` attribute.
#' @noRd
.new_aeme_output <- function(out_list, model, raw = FALSE) {
  class(out_list) <- c(if (isTRUE(raw)) "aeme_output_raw" else "aeme_output",
                       "list")
  attr(out_list, "model") <- model
  out_list
}

#' Is `x` a model-output list (standardised or raw)?
#'
#' @param x object to test.
#' @return logical.
#' @export
is_aeme_output <- function(x) {
  inherits(x, c("aeme_output", "aeme_output_raw"))
}

#' Is `x` a *raw* (`raw_output = TRUE`) model-output list?
#'
#' @inheritParams is_aeme_output
#' @return logical.
#' @export
is_aeme_output_raw <- function(x) {
  inherits(x, "aeme_output_raw")
}

#' @export
print.aeme_output <- function(x, ...) {
  .print_aeme_output(x, raw = FALSE)
}

#' @export
print.aeme_output_raw <- function(x, ...) {
  .print_aeme_output(x, raw = TRUE)
}

#' @noRd
.print_aeme_output <- function(x, raw) {
  model <- attr(x, "model")
  dates <- x[["Date"]]
  vars  <- setdiff(names(x), c("Date", "LKE_depths", "ok", "reason"))

  cat(sprintf(
    "<%s>%s\n",
    if (raw) "aeme_output_raw" else "aeme_output",
    if (!is.null(model)) paste0(" model: ", model) else ""
  ))
  if (raw) {
    cat("  raw model units/names/depths (no AEME standardisation)\n")
  }
  if (length(dates) > 0) {
    cat(sprintf("  %d date%s (%s to %s)\n", length(dates),
               if (length(dates) == 1) "" else "s",
               format(min(dates)), format(max(dates))))
  }
  cat(sprintf("  %d variable%s: %s\n", length(vars),
             if (length(vars) == 1) "" else "s",
             paste(vars, collapse = ", ")))
  invisible(x)
}
