#' Build the registry of thin, path-based wrapper functions for each model
#'
#' So `run_model_test()` can dispatch on `model` without special-casing each
#' one inline. `set_inflows`/`set_outflows` are adapted to a common `data`
#' first argument -- the underlying writers genuinely take differently named
#' arguments (`list_inf`/`inf_list`/`inf`), so the adapters only paper over
#' that naming, not any real behavioural difference. `set_param`/`set_init`
#' are used directly since their signatures already match across models.
#'
#' Built lazily (rather than as a top-level constant) so this file's load
#' order relative to `set_glm_param.R` etc. doesn't matter.
#'
#' @return named list, one entry per supported model
#' @noRd
.build_model_test_registry <- function() {
  list(
    glm_aed = list(
      set_param    = set_glm_param,
      set_init     = set_glm_init,
      set_inflows  = function(path, data, ...) set_glm_inflows(path, list_inf = data, ...),
      set_outflows = function(path, data, ...) set_glm_outflows(path, outf = data, ...),
      run          = run_glm_aed,
      read_output  = read_glm_output,
      outfile      = function(path) file.path(path, "output", "output.nc")
    ),
    gotm_wet = list(
      set_param    = set_gotm_param,
      set_init     = set_gotm_init,
      set_inflows  = function(path, data, ...) set_gotm_inflows(path, inf_list = data, ...),
      set_outflows = function(path, data, ...) set_gotm_outflows(path, outf = data, ...),
      run          = run_gotm_wet,
      read_output  = read_gotm_output,
      outfile      = function(path) file.path(path, "output", "output.nc")
    ),
    simstrat_aed2 = list(
      set_param    = set_simstrat_param,
      set_init     = set_simstrat_init,
      set_inflows  = function(path, data, ...) set_simstrat_inflows(path, inf = data, ...),
      set_outflows = function(path, data, heights_wdr, surface_elev, ...) {
        set_simstrat_outflows(path, outf = data, heights_wdr = heights_wdr,
                              surface_elev = surface_elev, ...)
      },
      run          = run_simstrat_aed2,
      read_output  = read_simstrat_output,
      outfile      = function(path) file.path(path, "output", "output.nc")
    ),
    simstrat_aed = list(
      set_param    = set_simstrat_param,
      set_init     = set_simstrat_init,
      set_inflows  = function(path, data, ...) set_simstrat_inflows(path, inf = data, ...),
      set_outflows = function(path, data, heights_wdr, surface_elev, ...) {
        set_simstrat_outflows(path, outf = data, heights_wdr = heights_wdr,
                              surface_elev = surface_elev, ...)
      },
      run          = run_simstrat_aed,
      read_output  = function(...) read_simstrat_output(..., model = "simstrat_aed"),
      outfile      = function(path) file.path(path, "output", "output.nc")
    )
  )
}

#' Get the thin-wrapper function set for a model
#'
#' @param model character; one of the names returned by
#'   `.build_model_test_registry()`
#'
#' @return named list of functions (`set_param`, `set_init`, `set_inflows`,
#'   `set_outflows`, `run`, `read_output`, `outfile`)
#' @noRd
.get_model_test_fns <- function(model) {
  registry <- .build_model_test_registry()
  fns <- registry[[model]]
  if (is.null(fns)) {
    cli::cli_abort(c(
      "Unsupported 'model': {model}.",
      "i" = "Must be one of: {paste(names(registry), collapse = ', ')}"
    ))
  }
  fns
}

#' Run a single what-if model scenario from a path alone
#'
#' Generic driver for quickly testing different parameters, nml/yaml/par
#' settings, inflow/outflow concentrations, or initial conditions against an
#' existing GLM-AED, GOTM-WET, Simstrat-AED or Simstrat-AED2 configuration
#' directory, without needing an `aeme` object. Dispatches to each model's
#' thin path-based wrapper functions (e.g. [set_glm_param()]/
#' [set_glm_init()]/[set_glm_inflows()]/[set_glm_outflows()]/
#' [run_glm_aed()]/[read_glm_output()] for `model = "glm_aed"`) so scenario
#' code can be written once and pointed at any supported model.
#'
#' Each `*_args` list is applied via `do.call()` against the corresponding
#' thin wrapper, with `path` supplied automatically as its first argument --
#' so the names inside each list must match that wrapper's own arguments.
#' `inflow_args`/`outflow_args` are the exception: their data.frame list
#' argument is always named `data` (regardless of the underlying wrapper's
#' own naming -- `list_inf`/`inf_list`/`inf`), and any extra arguments the
#' underlying outflow writer requires (e.g. Simstrat's `heights_wdr`/
#' `surface_elev`) are passed alongside it by name.
#'
#' @param model character; one of `"glm_aed"`, `"gotm_wet"`,
#'   `"simstrat_aed"`, `"simstrat_aed2"`.
#' @param path filepath; directory containing the existing model
#'   configuration to edit and run (as already written by [build_aeme()]).
#' @param param_overrides named list of parameter/value pairs, forwarded to
#'   the model's `set_*_param()` wrapper, e.g. `list(Kw = 0.5)` for GLM-AED
#'   or `` list(`time.dt` = 1800) `` for GOTM-WET. Empty list (default)
#'   leaves parameters unchanged.
#' @param init named list forwarded to the model's `set_*_init()` wrapper,
#'   e.g. `list(temp = seq(20, 10, length.out = 10), wq_init =
#'   list(NIT_amm = 0.5))`. Empty list (default) leaves initial conditions
#'   unchanged.
#' @param inflow_args named list forwarded to the model's `set_*_inflows()`
#'   wrapper; must include `data`, a named list of inflow data.frames (see
#'   [add_inflow()]). `NULL` (default) leaves inflows unchanged.
#' @param outflow_args named list forwarded to the model's
#'   `set_*_outflows()` wrapper; must include `data`, a named list of
#'   outflow data.frames (see [add_outflows()]), plus any further arguments
#'   the model requires (e.g. `heights_wdr`, and `surface_elev` for
#'   Simstrat). `NULL` (default) leaves outflows unchanged.
#' @param tgt_vars character vector of output variables to read back (passed
#'   as `vars_sim` to the model's `read_*_output()` wrapper). `NULL`
#'   (default) reads every variable the reader returns by default.
#' @param verbose logical; passed through to the model's `run_*()` wrapper.
#' @param safe logical; if `TRUE` (default), a failed edit or model run is
#'   caught and reported with `message()` instead of stopping -- useful when
#'   looping over many scenarios and one bad combination shouldn't abort the
#'   rest. Set `FALSE` to let errors propagate normally.
#'
#' @return A list of the requested output variables (as returned by the
#'   model's `read_*_output()` wrapper), or `NULL` if `safe = TRUE` and the
#'   edit/run/read failed.
#' @export
#'
#' @examples
#' \dontrun{
#' run_model_test("glm_aed", path_glm, param_overrides = list(Kw = 0.8),
#'                tgt_vars = "HYD_temp")
#' run_model_test("simstrat_aed2", path_simstrat,
#'                init = list(wq_init = list(NIT_amm = 0.5)),
#'                tgt_vars = "HYD_temp")
#' }

run_model_test <- function(model, path, param_overrides = list(),
                           init = list(), inflow_args = NULL,
                           outflow_args = NULL, tgt_vars = NULL,
                           verbose = FALSE, safe = TRUE) {

  fns <- .get_model_test_fns(model)

  # Failure is tracked via `failed` (set only by an actual thrown error, in
  # the tryCatch handler below) rather than by inspecting each step's return
  # value -- several of the underlying thin wrappers (e.g.
  # set_simstrat_outflows()) legitimately return NULL on success, which
  # would otherwise be indistinguishable from a caught error.
  failed <- FALSE

  step <- function(expr) {
    if (failed) return(invisible(NULL))
    if (!safe) return(expr)
    tryCatch(expr, error = function(e) {
      message("run_model_test error: ", conditionMessage(e))
      failed <<- TRUE
      invisible(NULL)
    })
  }

  if (length(param_overrides) > 0) {
    step(do.call(fns$set_param, c(list(path), param_overrides)))
  }
  if (length(init) > 0) {
    step(do.call(fns$set_init, c(list(path), init)))
  }
  if (!is.null(inflow_args)) {
    step(do.call(fns$set_inflows, c(list(path), inflow_args)))
  }
  if (!is.null(outflow_args)) {
    step(do.call(fns$set_outflows, c(list(path), outflow_args)))
  }
  if (failed) return(NULL)

  step(fns$run(sim_folder = path, verbose = verbose))
  if (failed) return(NULL)

  outfile <- fns$outfile(path)
  if (!file.exists(outfile)) {
    if (safe) {
      message("run_model_test error: expected output file not found: ",
              outfile)
      return(NULL)
    }
    cli::cli_abort("Expected output file not found: {outfile}")
  }

  step(fns$read_output(file = outfile, vars_sim = tgt_vars))
}
