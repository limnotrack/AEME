# write initial temperature profiles for the GOTM nml and yaml file setup

#' Write initial temperature profiles for GOTM-WET
#'
#' @inheritParams initialiseGLM
#' @param start_date Date; of start of simulation
#' @param path.gotm filepath; to GOTM directory
#'
#' @return updated initial profile files are written to the model directory.
#' @noRd
#'

initialiseGOTM <-  function(lvl_bottom, lvl_surf,
                            tmpwtr = 10, start_date,
                            tbl_obs = NULL,
                            path.gotm = "") {

  # define the proTable (intial profiles for T and SAL)
  if (is.null(tbl_obs)) {
    tbl_obs <- data.frame(c(lvl_bottom, lvl_start),
                          c(tmpwtr, tmpwtr),
                          c(0, 0))
  }

  ndeps <- nrow(tbl_obs)
  df <- matrix(NA, nrow = 1 + nrow(tbl_obs), ncol = 2)
  df[1, 1] <- paste(start_date, "09:00:00")
  df[1, 2] <- paste(nrow(tbl_obs), " ", 2)
  df[(2):(1 + ndeps), 1] <- as.numeric(-tbl_obs[["depth"]])
  df[(2):(1 + ndeps), 2] <- as.numeric(tbl_obs[["temperature"]])
  write.table(df, file.path(path.gotm, "inputs", "t_prof_file.dat"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  # salinity initial
  df[(2):(1 + ndeps), 2] <- tbl_obs[["salt"]]
  write.table(df, file.path(path.gotm, "inputs", "s_prof_file.dat"),
              quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")


  #-------- make the file! ---------
  path.yaml <- file.path(path.gotm, "gotm.yaml")
  gotm.yaml <- yaml::read_yaml(path.yaml)

  gotm.yaml$temperature$method <- 2
  gotm.yaml$temperature$file <- "inputs/t_prof_file.dat"
  gotm.yaml$temperature$column <- 1

  gotm.yaml$salinity$method <- 2
  gotm.yaml$salinity$file <- "inputs/s_prof_file.dat"
  gotm.yaml$salinity$column <- 1

  write_yaml(gotm.yaml, path.yaml)
}
