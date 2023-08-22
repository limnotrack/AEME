#' Run ensemble
#'
#' @param model
#' @param sim_folder
#' @param bin_path
#' @param verbose
#' @param parallel
#'
#' @return Runs the model
#' @export
#'
#' @importFrom parallel parLapply makeCluster detectCores clusterExport
#' stopCluster
#' @importFrom stats setNames
#'
#' @examples
run_ensemble <- function(config, model, verbose = FALSE, debug = FALSE,
                         timeout = 0, parallel = FALSE, dir = ".") {

  sim_folder <- file.path(dir, paste0(config$location$lake_id,"_",
                                      tolower(config$location$name)))
  run_model_args <- list(sim_folder = sim_folder, verbose = verbose,
                         debug = debug, timeout = timeout)

  if (parallel) {
    ncores <- min(c(parallel::detectCores() - 1, length(model)))
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl, varlist = list("run_model_args", "run_dy_cd",
                                               "run_glm_aed", "run_gotm_wet"),
                            envir = environment())
    # parallel::clusterEvalQ(cl, expr = {library(LakeEnsemblR); library(gotmtools);
    # })
    message("Running models in parallel... ", paste0("[", Sys.time(), "]"))
    model_out <- stats::setNames(
      parallel::parLapply(cl, model, function(mod_name) do.call(paste0("run_", mod_name),
                                                                   run_model_args)),
      model
    )
    parallel::stopCluster(cl)
    message("Model run complete!", paste0("[", Sys.time(), "]"))

  } else {
    message("Running models... (Have you tried parallelizing?) ",
            paste0("[", Sys.time(), "]"))
    model_out <- setNames(
      lapply(model, function(mod_name) do.call(paste0("run_", mod_name),
                                               run_model_args)),
      model
    )
    message("Model run complete!", paste0("[", Sys.time(), "]"))
  }

}


#' Run DYRESM-CAEDYM
#'
#' @param sim_folder
#' @param bin_path
#' @param verbose
#' @param debug
#' @param timeout
#'
#' @return
#' @noRd
#'
#' @examples
run_dy_cd <- function(sim_folder, verbose = FALSE, debug = FALSE,
                      timeout = 0) {

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- system.file('extbin/', package = "AEME")

  arg <- ifelse(debug, "> dycd.log", "")

  dy.prefix <- gsub(".stg", "", list.files(file.path(sim_folder, "dy_cd"),
                                           pattern = "stg"))

  setwd(file.path(sim_folder, "dy_cd"))
  ref_fils <- c(paste0(dy.prefix, c(".stg", ".met", ".inf", ".wdr")), "DYref.nc")
  sim_fils <- c(paste0(dy.prefix, c(".pro")),  "dyresm3p1.par", paste0(dy.prefix, c(".con")), "DYsim.nc")
  info_fils <- c("DYref.nc", "DYsim.nc", paste0(dy.prefix, c( ".cfg")))
  # Delete historic files
  unlink("DYsim.nc")
  unlink("DYref.nc")
  unlink("morphinterp.out")
  unlink("dy.log")

  stdout <- ifelse(verbose, "", TRUE)
  stderr <- ifelse(verbose, "", TRUE)

  # Create reference netcdf
  system2(file.path(bin_path, "dy_cd", "createDYref.exe"),
          wait = TRUE, stdout = stdout,
          stderr = stderr,
          args = ref_fils)

  # Create simulation file ----
  system2(file.path(bin_path, "dy_cd", "createDYsim.exe"),
          wait = TRUE, stdout = stdout,
          stderr = stderr,
          args = sim_fils)
  system2(file.path(bin_path, "dy_cd", "extractDYinfo.exe"),
          wait = TRUE, stdout = stdout,
          stderr = stderr,
          args = info_fils)
  if (verbose) {
    system2(file.path(bin_path, "dy_cd", "dycd.exe"),
            wait = TRUE, stdout = stdout,
            stderr = "", args = arg, timeout = timeout)
  } else {
    out <- system2(file.path(bin_path, "dy_cd", "dycd.exe"),
                   wait = TRUE, stdout = TRUE,
                   stderr = TRUE, args = arg, timeout = timeout)
  }
  out <- readLines("dy.log")
  success <- sum(grepl("END DYRESM-CAEDYM", out)) == 1
  if (success) {
    message("DYRESM-CAEDYM run successful! [", Sys.time(), "]")
  } else {
    print(tail(out, 20))
  }
}

#' Run GLM-AED
#'
#' @param sim_folder
#' @param bin_path
#' @param verbose
#' @param timeout
#'
#' @return
#' @noRd
#'
#' @examples
run_glm_aed <- function(sim_folder, bin_path, verbose = FALSE, debug = FALSE,
                        timeout = 0) {

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- system.file('extbin/', package = "AEME")
  setwd(file.path(sim_folder, "glm_aed"))
  unlink("output/output.nc")

  if (verbose) {
    system2(file.path(bin_path, "glm_aed", "glm.exe"),
            wait = TRUE, stdout = "",
            stderr = "", timeout = timeout)
  } else {
    out <- system2(file.path(bin_path, "glm_aed", "glm.exe"),
                   wait = TRUE, stdout = TRUE,
                   stderr = TRUE, timeout = timeout)
    success <- sum(grepl("Model Run Complete", out)) == 1
    if (success) {
      message("GLM-AED run successful! [", Sys.time(), "]")
    } else {
      print("FAILED")
      print(tail(out, 10))
    }
  }
}

#' Run GOTM-WET
#'
#' @param sim_folder
#' @param bin_path
#' @param verbose
#' @param timeout
#'
#' @return
#' @noRd
#'
#' @examples
run_gotm_wet <- function(sim_folder, verbose = FALSE, debug = FALSE,
                         timeout = 0) {

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  setwd(file.path(sim_folder, "gotm_wet"))
  unlink("output/output.nc")
  dir.create("output", showWarnings = FALSE)
  if(verbose) {
    system2(file.path(bin_path, "gotm_wet", "gotm_release.exe"), # "gotm_release.exe"
            wait = TRUE, stdout = "",
            stderr = "", timeout = timeout)
  } else {
    out <- system2(file.path(bin_path, "gotm_wet", "gotm_release.exe"),
                   wait = TRUE, stdout = TRUE,
                   stderr = TRUE, timeout = timeout)
    success <- sum(grepl("GOTM finished on", out)) == 1
    if (success) {
      message("GOTM-WET run successful! [", Sys.time(), "]")
    } else {
      print(tail(out, 10))
    }
  }
}

