#' Load AEME output to the aeme object
#'
#' @inheritParams build_aeme
#' @inheritParams run_aeme
#' @inheritParams parallel::stopCluster
#' @param lake_dir Path to the lake AEME directory. If `NULL`, it is derived
#' from `aeme`/`path`.
#'
#' @return Updated aeme object with model output
#' @export
#'
#' @importFrom dplyr filter pull case_when
#' @importFrom parallel clusterExport parLapply stopCluster detectCores
#' @importFrom parallel makeCluster
#'
#'

load_output <- function(aeme, model, path = NULL, lake_dir = NULL, model_controls, 
                        parallel = FALSE, cl = NULL, ens_n = 1) {
  
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
                                               "lake_dir", "output_hour"),
                            envir = environment())
    # message("Reading models in parallel... ", paste0("[", format(Sys.time()), "]"))
    cli_inform_safe(c("i" = paste0("Reading models in parallel...",
                                   "[", format(Sys.time()), "]")))
    mods <- parallel::parLapply(cl = cl, model, \(m) {
      
      read_model_nc(aeme = aeme, model = m, path = path, lake_dir = lake_dir,
                    vars_sim = vars_sim, incl_fluxes = TRUE, 
                    output_hour = output_hour)
    })
    cli_inform_safe(c("v" = paste0("Model reading complete! ",
                                   "[", format(Sys.time()), "]")))
    
  } else {
    mods <- lapply(model, \(m) {
      read_model_nc(aeme = aeme, model = m, path = path, lake_dir = lake_dir,
                    vars_sim = vars_sim, incl_fluxes = TRUE, 
                    output_hour = output_hour)
    })
  }
  names(mods) <- model

  ens_lab <- format_ens_label(ens_n = ens_n)
  
  outp[[ens_lab]] <- list(dy_cd = mods[["dy_cd"]], glm_aed = mods[["glm_aed"]],
                          gotm_wet = mods[["gotm_wet"]],
                          simstrat_aed2 = mods[["simstrat_aed2"]],
                          simstrat_aed = mods[["simstrat_aed"]])
  outp$n_members <- sum(grepl("ens", names(outp)))
  
  output(aeme) <- outp
  
  return(aeme)
}
