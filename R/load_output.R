#' Load AEME output to the aeme object
#'
#' Reads the output of models that have already been run and, by default,
#' stores it in the `output` slot of the `Aeme` object. With
#' `return_type = "output"` the output is returned instead and the `Aeme` object
#' is left as it was, which is useful for looking at extra output (see
#' `extra_vars`) without storing it alongside the output already on the object.
#'
#' @inheritParams build_aeme
#' @inheritParams run_aeme
#' @inheritParams parallel::stopCluster
#' @param lake_dir Path to the lake AEME directory. If `NULL`, it is derived
#' from `aeme`/`path`.
#'
#' @param keep_diag logical; keep the `diag` (diagnostic/scalar) and
#' `sediment` sub-lists of each model's output (variables with dimensions other
#' than time or depth x time, keyed by raw model names). Default `TRUE`, which
#' keeps whatever was requested through `extra_vars` (nothing is requested by
#' default, so by default there is nothing to keep). Set `FALSE` to drop the
#' sub-lists even if they were requested.
#'
#' @param extra_vars character; for GLM-AED, which sediment, flux and diagnostic
#' variables beyond the standard set to read from `output.nc`. Default `"none"`:
#' only the standard variables (`vars_sim` and the lake fluxes) are read, so
#' nothing extra is loaded or stored unless asked for. Otherwise AEME names from
#' [glm_extra_vars] (e.g. `"SED_temp"`), raw netCDF names, the keywords
#' `"sediment"` / `"diag"` (every variable in that sub-list), or `"all"` for
#' every variable in the file (this was the behaviour before extra output had to
#' be requested). Unrequested variables are never read, which keeps loading fast
#' and ensemble output small. See [read_glm_output()].
#' @param sed_layers integer; GLM-AED sediment layers to keep for variables
#' with a sediment-layer dimension (`SED_temp`). `NULL` keeps all. See
#' [read_glm_output()].
#' @param zone_summary logical; for GLM-AED, also add an area-weighted
#' lake-wide series for each requested sediment-zone variable. See
#' [read_glm_output()]. Default `FALSE`.
#'
#' @param return_type character; `"aeme"` (default) returns the `Aeme` object
#' with the output stored in it. `"output"` returns the output without storing
#' it: a list named by model, with one `aeme_output` (see [is_aeme_output()]) per
#' model, read in the same way (simulation window, daily means, unit conversions)
#' as the output that is stored. Because nothing is stored, `ens_n` is ignored.
#' The result can be given to functions that take a model output directly, such
#' as [plot_model_output()], but not to those that need an `Aeme` object, such
#' as [get_var()].
#'
#' @return With `return_type = "aeme"`, the updated `Aeme` object with model
#' output. With `return_type = "output"`, a named list of `aeme_output` objects,
#' one per model.
#' @export
#'
#' @importFrom dplyr filter pull case_when
#' @importFrom parallel clusterExport parLapply stopCluster detectCores
#' @importFrom parallel makeCluster
#'
#'

load_output <- function(aeme, model, path = NULL, lake_dir = NULL, model_controls, 
                        parallel = FALSE, cl = NULL, ens_n = 1,
                        keep_diag = TRUE, extra_vars = "none", sed_layers = NULL,
                        zone_summary = FALSE,
                        return_type = c("aeme", "output")) {
  
  return_type <- match.arg(return_type)
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  if (missing(model_controls)) {
    model_controls <- get_model_controls(aeme)
    if (is.null(model_controls)) {
      model_controls <- get_model_controls()
    }
  }
  # path <- check_path(path = path, must_exist = TRUE)
  outp <- output(aeme)
  aeme_time <- time(aeme)
  output_hour <- 0
  spin_up <- aeme_time$spin_up
  # start_date <- as.Date(aeme_time$start)
  vars_sim <- get_vars_sim(model_controls = model_controls)
  
  # Extract model output fron netCDF files and return as a list
  if (parallel) {
    
    if (is.null(cl)) {
      ncores <- min(c(parallel::detectCores() - 1, length(model)))
      cl <- parallel::makeCluster(ncores)
      on.exit({
        parallel::stopCluster(cl)
      }, add = TRUE)
    }
    parallel::clusterExport(cl, varlist = list("aeme", "path", "vars_sim",  
                                               "lake_dir", "output_hour",
                                               "extra_vars", "sed_layers",
                                               "zone_summary"),
                            envir = environment())
    # message("Reading models in parallel... ", paste0("[", format(Sys.time()), "]"))
    cli_inform_safe(c("i" = paste0("Reading models in parallel...",
                                   "[", format(Sys.time()), "]")))
    mods <- parallel::parLapply(cl = cl, model, \(m) {
      
      read_model_nc(aeme = aeme, model = m, path = path, lake_dir = lake_dir,
                    vars_sim = vars_sim, incl_fluxes = TRUE, 
                    output_hour = output_hour, extra_vars = extra_vars,
                    sed_layers = sed_layers, zone_summary = zone_summary)
    })
    cli_inform_safe(c("v" = paste0("Model reading complete! ",
                                   "[", format(Sys.time()), "]")))
    
  } else {
    mods <- lapply(model, \(m) {
      read_model_nc(aeme = aeme, model = m, path = path, lake_dir = lake_dir,
                    vars_sim = vars_sim, incl_fluxes = TRUE, 
                    output_hour = output_hour, extra_vars = extra_vars,
                    sed_layers = sed_layers, zone_summary = zone_summary)
    })
  }
  names(mods) <- model
  if (!keep_diag) {
    mods <- lapply(mods, \(x) {
      if (is.list(x)) x[intersect(names(x), c("diag", "sediment"))] <- NULL
      x
    })
  }

  if (return_type == "output") {
    return(mods)
  }

  ens_lab <- format_ens_label(ens_n = ens_n)
  
  outp[[ens_lab]] <- list(dy_cd = mods[["dy_cd"]], glm_aed = mods[["glm_aed"]],
                          gotm_wet = mods[["gotm_wet"]],
                          simstrat_aed2 = mods[["simstrat_aed2"]],
                          simstrat_aed = mods[["simstrat_aed"]])
  outp$n_members <- sum(grepl("ens", names(outp)))
  
  output(aeme) <- outp
  
  return(aeme)
}
