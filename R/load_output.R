#' Load AEME output to the aeme object
#'
#' @inheritParams build_aeme
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

load_output <- function(model, aeme, path, model_controls, parallel = FALSE,
                        nlev = NULL, ens_n = 1) {

  if (is.null(nlev)) {
    inp <- input(aeme)
    hyps <- inp$hypsograph
    depth <- max(hyps$elev) - min(hyps$elev)
    if (depth < 3) {
      div <- 0.1
    } else {
      div <- 0.33
    }
    nlev <- ceiling((depth) / div)
  }
  outp <- output(aeme)
  aeme_time <- time(aeme)
  output_hour <- 0
  spin_up <- aeme_time$spin_up
  # start_date <- as.Date(aeme_time$start)
  vars_sim <- model_controls |>
    dplyr::filter(simulate) |>
    dplyr::pull(var_aeme)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)

  # Extract model output fron netCDF files and return as a list
  if (parallel) {

    ncores <- min(c(parallel::detectCores() - 1, length(model)))
    cl <- parallel::makeCluster(ncores)
    on.exit({
      parallel::stopCluster(cl)
    })
    parallel::clusterExport(cl, varlist = list("lake_dir", "vars_sim",
                                               "aeme", "nlev"),
                            envir = environment())
    # parallel::clusterEvalQ(cl, expr = {library(LakeEnsemblR); library(gotmtools);
    # })
    message("Reading models in parallel... ", paste0("[", format(Sys.time()), "]"))
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
                 aeme = aeme,
                 nlev = nlev,
                 output_hour = output_hour,
                 path = path)
    })

    message("Model reading complete!", paste0("[", format(Sys.time()), "]"))

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

  ens_lab <- paste0("ens_", sprintf("%03d", ens_n))

  outp[[ens_lab]] <- list(dy_cd = mods[["dy_cd"]], glm_aed = mods[["glm_aed"]],
                          gotm_wet = mods[["gotm_wet"]])
  outp$n_members <- sum(grepl("ens", names(outp)))

  output(aeme) <- outp

  return(aeme)
}
