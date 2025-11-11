#' Load AEME output to the aeme object
#'
#' @inheritParams build_aeme
#' @inheritParams run_aeme
#' @inheritParams parallel::stopCluster
#' @param nlev numeric; number of levels to return in model output. If NULL,
#' calculates number of levels based on the `model_layer_structure`.
#'
#' @return Updated aeme object with model output
#' @export
#'
#' @importFrom dplyr filter pull case_when
#' @importFrom ncdf4 nc_open nc_close
#' @importFrom parallel clusterExport parLapply stopCluster detectCores
#' makeCluster
#'
#'

load_output <- function(model, aeme, path, model_controls, parallel = FALSE,
                        cl = NULL, nlev = NULL, ens_n = 1) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  path <- check_path(path = path, must_exist = TRUE)
  if (is.null(nlev)) {
    # inp <- input(aeme)
    # hyps <- inp$hypsograph
    # depth <- max(hyps$elev) - min(hyps$elev)
    # lake_dir <- get_lake_dir(aeme = aeme, path = path)
    lke <- lake(aeme)
    depth <- lke$depth
    sub_layers <- get_model_layers(depth = depth)
    nlev <- nrow(sub_layers)
  }
  outp <- output(aeme)
  aeme_time <- time(aeme)
  output_hour <- 0
  spin_up <- aeme_time$spin_up
  # start_date <- as.Date(aeme_time$start)
  vars_sim <- model_controls |>
    dplyr::filter(simulate) |>
    dplyr::pull(var_aeme)
  
  # Extract model output fron netCDF files and return as a list
  if (parallel) {
    
    if (is.null(cl)) {
      ncores <- min(c(parallel::detectCores() - 1, length(model)))
      cl <- parallel::makeCluster(ncores)
      on.exit({
        parallel::stopCluster(cl)
      }, add = TRUE)
    }
    parallel::clusterExport(cl, varlist = list("vars_sim", "aeme", "nlev",
                                               "output_hour", "path"),
                            envir = environment())
    # message("Reading models in parallel... ", paste0("[", format(Sys.time()), "]"))
    cli_inform_safe(c("i" = paste0("Reading models in parallel...",
                                   "[", format(Sys.time()), "]")))
    mods <- parallel::parLapply(cl = cl, model, \(m) {
      
      read_model_nc(model = m,
                    vars_sim = vars_sim,
                    aeme = aeme,
                    nlev = nlev,
                    output_hour = output_hour,
                    path = path)
    })
    cli_inform_safe(c("v" = paste0("Model reading complete! ",
                                   "[", format(Sys.time()), "]")))
    
  } else {
    mods <- lapply(model, \(m) {
      read_model_nc(model = m,
                    vars_sim = vars_sim,
                    aeme = aeme,
                    nlev = nlev,
                    output_hour = output_hour,
                    path = path)
    })
  }
  names(mods) <- model
  # lapply(mods, \(x) head(x$Date))
  # lapply(mods, \(x) tail(x$Date))
  # lapply(mods, \(x) x$HYD_temp[, 500])
  # lapply(mods, \(x) x$LKE_layers[, 10])
  
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  outp[[ens_lab]] <- list(dy_cd = mods[["dy_cd"]], glm_aed = mods[["glm_aed"]],
                          gotm_wet = mods[["gotm_wet"]])
  outp$n_members <- sum(grepl("ens", names(outp)))
  
  output(aeme) <- outp
  
  return(aeme)
}
