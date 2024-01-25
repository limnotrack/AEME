# write initial temperature profiles for the GOTM nml and yaml file setup

#' Write initial temperature profiles for GOTM-WET
#'
#' @inheritParams initialiseGLM
#' @param start_date Date; of start of simulation
#' @param path_gotm filepath; to GOTM directory
#'
#' @return updated initial profile files are written to the model directory.
#' @noRd
#'

initialiseGOTM <-  function(gotm, lvl_bottom, lvl_surf,
                            tmpwtr = 10, start_date,
                            tbl_obs = NULL,
                            path_gotm = "", use_bgc = FALSE, mod_ctrls) {

  # define the proTable (intial profiles for T and SAL)
  if (is.null(tbl_obs)) {
    tbl_obs <- data.frame(c(lvl_bottom, lvl_start),
                          c(tmpwtr, tmpwtr),
                          c(0, 0))
  }

  ndeps <- nrow(tbl_obs)
  df <- matrix(NA, nrow = 1 + nrow(tbl_obs), ncol = 2)
  df[1, 1] <- paste(start_date, "00:00:00")
  df[1, 2] <- paste(nrow(tbl_obs), " ", 2)
  df[(2):(1 + ndeps), 1] <- as.numeric(-tbl_obs[["depth"]])
  df[(2):(1 + ndeps), 2] <- as.numeric(tbl_obs[["temperature"]])
  write.table(df, file.path(path_gotm, "inputs", "t_prof_file.dat"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  # salinity initial
  df[(2):(1 + ndeps), 2] <- tbl_obs[["salt"]]
  write.table(df, file.path(path_gotm, "inputs", "s_prof_file.dat"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  if (use_bgc) {
    initialise_FABM(path_gotm = path_gotm, mod_ctrls = mod_ctrls)
  }


  #-------- Update the yaml file! ---------

  gotm$surface$sst$constant_value <- tmpwtr

  gotm$temperature$method <- 2
  gotm$temperature$file <- "inputs/t_prof_file.dat"
  gotm$temperature$column <- 1

  gotm$salinity$method <- 2
  gotm$salinity$file <- "inputs/s_prof_file.dat"
  gotm$salinity$column <- 1

  gotm
}
