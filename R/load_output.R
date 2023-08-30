#' Load AEME output to the aeme object
#'
#' @inheritParams build_ensemble
#' @inheritParams run_aeme
#' @param nlev numeric; number of levels to return in model output. If NULL,
#' calculates number of levels based on lake depth as provided within the
#' hypsograph. If the lake is less thatn 3m deep, it outputs every 0.1m, else
#' if it is greater than 3m, it outputs every 0.33m. Defaults to NULL.
#'
#' @return Updated aeme object with model output
#' @export
#'
#' @importFrom dplyr filter pull case_when
#' @importFrom ncdf4 nc_open nc_close
#'

load_output <- function(model, aeme_data, path, mod_ctrls, parallel = FALSE,
                        nlev = NULL) {

  if (is.null(nlev)) {
    inp <- input(aeme_data)
    hyps <- inp$hypsograph
    depth <- max(hyps$elev) - min(hyps$elev)
    if (depth < 3) {
      div <- 0.1
    } else {
      div <- 0.33
    }
    nlev <- ceiling((depth) / div)
  }
  aeme_time <- time(aeme_data)
  spin_up <- aeme_time$spin_up
  vars_sim <- mod_ctrls |>
    dplyr::filter(simulate == 1) |>
    dplyr::pull(name)
  lke <- lake(aeme_data)
  lake_dir <- file.path(path, paste0(lke$id, "_",
                                    tolower(lke$name)))


  if (parallel) {

    ncores <- min(c(parallel::detectCores() - 1, length(model)))
    cl <- parallel::makeCluster(ncores)
    on.exit({
      parallel::stopCluster(cl)
    })
    parallel::clusterExport(cl, varlist = list("lake_dir", "nc_listify",
                                               "vars_sim", "spin_up", "nlev",
                                               "delagrangify"),
                            envir = environment())
    # parallel::clusterEvalQ(cl, expr = {library(LakeEnsemblR); library(gotmtools);
    # })
    message("Reading models in parallel... ", paste0("[", Sys.time(), "]"))
    mods <- parallel::parLapply(cl = cl, model, \(m) {
      out_file <- dplyr::case_when(m == "dy_cd" ~ file.path(lake_dir, m,
                                                            "DYsim.nc"),
                                   m == "glm_aed" ~ file.path(lake_dir, m,
                                                              "output",
                                                              "output.nc"),
                                   m == "gotm_wet" ~ file.path(lake_dir, m,
                                                               "output",
                                                               "output.nc")
      )

      if (!file.exists(out_file)) {
        message("No ", out_file, " present.")
        return(NULL)
      }

      nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
      if (nc$error) {
        stop("Could not open netCDF file: ", out_file)
      }
      on.exit({
        ncdf4::nc_close(nc)
      })
      nc_listify(nc = nc, model = m,
                 vars_sim = vars_sim,
                 spin_up = spin_up[[m]],
                 nlev = nlev)
    })

    message("Model reading complete!", paste0("[", Sys.time(), "]"))

  } else {
    mods <- lapply(model, \(m) {
      out_file <- dplyr::case_when(m == "dy_cd" ~ file.path(lake_dir, m,
                                                            "DYsim.nc"),
                                   m == "glm_aed" ~ file.path(lake_dir, m,
                                                              "output",
                                                              "output.nc"),
                                   m == "gotm_wet" ~ file.path(lake_dir, m,
                                                               "output",
                                                               "output.nc")
      )

      if (!file.exists(out_file)) {
        message("No ", out_file, " present.")
        return(NULL)
      }

      nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
      if (nc$error) {
        stop("Could not open netCDF file: ", out_file)
      }
      on.exit({
        ncdf4::nc_close(nc)
      })
      nc_listify(nc = nc, model = m,
                 vars_sim = vars_sim,
                 spin_up = spin_up[[m]],
                 nlev = nlev)
    })
  }
  names(mods) <- model
  new_output <- list(dy_cd = mods[["dy_cd"]], glm_aed = mods[["glm_aed"]],
                     gotm_wet = mods[["gotm_wet"]])

  output(aeme_data) <- new_output

  return(aeme_data)
}
