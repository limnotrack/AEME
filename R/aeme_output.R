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
#' @param var_units named character vector; each raw netCDF variable's
#'   `units` attribute, named by its key in `out_list` (see
#'   \code{\link{.nc_var_meta}}). Only meaningful when `raw = TRUE` -- a
#'   standardised variable may have been unit-converted away from its native
#'   netCDF units. Default `NULL` (no attribute attached).
#' @param var_long_name named character vector; each raw netCDF variable's
#'   `long_name` attribute, named the same way as `var_units`. Default `NULL`.
#'
#' @return `out_list`, with class `c("aeme_output_raw", "list")` or
#'   `c("aeme_output", "list")`, a `model` attribute, and (when supplied)
#'   `var_units`/`var_long_name` attributes.
#' @noRd
.new_aeme_output <- function(out_list, model, raw = FALSE, var_units = NULL,
                             var_long_name = NULL) {
  class(out_list) <- c(if (isTRUE(raw)) "aeme_output_raw" else "aeme_output",
                       "list")
  attr(out_list, "model") <- model
  if (!is.null(var_units)) attr(out_list, "var_units") <- var_units
  if (!is.null(var_long_name)) attr(out_list, "var_long_name") <- var_long_name
  out_list
}

#' Read a netCDF variable's `units`/`long_name` attributes, if present
#'
#' Used to label plots of raw (`raw_output = TRUE`) model output, where
#' variables are keyed by their native netCDF name and so have no entry in
#' the package's `key_naming` table to source a label from.
#'
#' @param nc open ncdf4 object.
#' @param v character; netCDF variable name.
#' @return list with elements `units` and `long_name`, each a length-1
#'   character (`""` if the variable or attribute isn't present).
#' @noRd
.nc_var_meta <- function(nc, v) {
  if (is.null(nc$var[[v]])) return(list(units = "", long_name = ""))
  get_att <- function(att) {
    # ncatt_get() returns value = 0 (not "") when the attribute is absent --
    # hasatt is the only reliable signal that it's actually present
    res <- tryCatch(ncdf4::ncatt_get(nc, v, att),
                    error = function(e) list(hasatt = FALSE))
    if (!isTRUE(res$hasatt) || is.null(res$value) || length(res$value) != 1) {
      return("")
    }
    as.character(res$value)
  }
  list(units = get_att("units"), long_name = get_att("long_name"))
}

#' Build a plot axis/fill label from a raw netCDF variable's units/long_name
#'
#' @param units character; `""` if unavailable.
#' @param long_name character; `""` if unavailable.
#' @param var_sim character; the variable's netCDF/list-key name, used as a
#'   fallback when `long_name` isn't available.
#' @return character; e.g. `"Ammonium nitrogen (mmol/m3)"`.
#' @noRd
.raw_var_label <- function(units, long_name, var_sim) {
  label <- if (!is.na(long_name) && nzchar(long_name)) long_name else var_sim
  if (!is.na(units) && nzchar(units)) label <- paste0(label, " (", units, ")")
  label
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
