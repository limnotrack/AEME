#' Run aquatic model ensemble
#'
#' @inheritParams build_aeme
#' @inheritParams base::system2
#' @param return logical; return model output within an `aeme` object? Defaults
#' to TRUE.
#' @inheritParams load_output
#' @param verbose logical; print model output to console. Defaults to FALSE.
#' @param debug logical; write debug log (Only DYRESM). Defaults to FALSE.
#' @param parallel logical; run models in parallel. Defaults to FALSE.
#' @param ncores integer; number of cores to use for parallelization. Defaults
#' to `min(c(detectCores() - 1, length(model)))`.
#' @param check_output logical; check model output after running? Defaults to
#' FALSE.
#' @param ens_n numeric; ensemble number to allocate to model output which is
#' loaded. Defaults to 1.
#'
#' @return an `aeme` object with model output loaded.
#' @export
#'
#' @importFrom parallel parLapply makeCluster detectCores clusterExport
#' stopCluster
#' @importFrom stats setNames
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model_controls <- get_model_controls()
#' model <- c("glm_aed")
#' aeme <- build_aeme(path = path, aeme = aeme, model = model,
#' model_controls = model_controls, ext_elev = 5)
#' aeme <- run_aeme(aeme = aeme, model = model, path = path)
#' plot_output(aeme, model = model)
run_aeme <- function(aeme, model, return = TRUE, ens_n = 1,
                     model_controls = NULL, nlev = NULL, verbose = FALSE,
                     debug = FALSE, timeout = 0, parallel = FALSE, ncores,
                     check_output = FALSE, path = ".") {

  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
  }

  if (return & is.null(model_controls)) {
    stop("`model_controls` need to be provided to load model output.")
  }

  sim_folder <- get_lake_dir(aeme = aeme, path = path)
  if (!dir.exists(sim_folder)) {
    stop("Simulation folder does not exist.")
  }

  # Check if model directories exist
  model_dir_chk <- !any(dir.exists(file.path(sim_folder, model)))
  if (model_dir_chk) {
    stop("Model folder does not exist.\n",
         file.path(sim_folder, model)[dir.exists(file.path(sim_folder, model))])
  }

  run_model_args <- list(sim_folder = sim_folder, verbose = verbose,
                         debug = debug, timeout = timeout)

  cl <- NULL # Initialize cluster object
  if (parallel) {
    if (missing(ncores)) {
      ncores <- min(c(parallel::detectCores() - 1, length(model)))
    }
    cl <- parallel::makeCluster(ncores)
    on.exit({
      parallel::stopCluster(cl)
    })
    parallel::clusterExport(cl, varlist = list("run_model_args", "run_dy_cd",
                                               "run_glm_aed", "run_gotm_wet"),
                            envir = environment())
    message("Running models in parallel... ", paste0("[", format(Sys.time()),
                                                     "]"))
    model_out <- stats::setNames(
      parallel::parLapply(cl, model, function(mod_name) {
        do.call(paste0("run_", mod_name), run_model_args)
      }),
      model
    )
    message("Model run complete!", paste0("[", format(Sys.time()), "]"))

  } else {
    message("Running models... (Have you tried parallelizing?) ",
            paste0("[", format(Sys.time()), "]"))
    model_out <- setNames(
      lapply(model, function(mod_name) do.call(paste0("run_", mod_name),
                                               run_model_args)),
      model
    )
    message("Model run complete!", paste0("[", format(Sys.time()), "]"))
  }

  if (check_output) {
    message("Checking model output...")
    chk <- sapply(model, \(m) {
      check_model_output(path = path, aeme = aeme, model = m)
    })
    if (any(chk)) {
      message("Models ", paste0(model[chk], collapse = ", "), " passed checks.")
    }
    if (any(!chk)) {
      message("Warning: Models ", paste0(model[!chk], collapse = ", "),
              " failed checks.")
    }
  }

  if (return) {
    aeme <- load_output(model = model, aeme = aeme, path = path,
                        model_controls = model_controls, parallel = parallel,
                        cl = cl, nlev = nlev, ens_n = ens_n)
    return(aeme)
  }

}


#' Run DYRESM-CAEDYM
#'
#' @param sim_folder the directory where simulation files are contained
#' @param verbose Logical: Should output of model be shown
#' @param debug Logical; save debug file. DYRESM only.
#' @inheritParams base::system2
#'
#' @importFrom utils tail
#' @return runs DYRESM-CAEDYM
#' @noRd

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
  ref_fils <- c(paste0(dy.prefix, c(".stg", ".met", ".inf", ".wdr")),
                "DYref.nc")
  sim_fils <- c(paste0(dy.prefix, c(".pro")),  "dyresm3p1.par",
                paste0(dy.prefix, c(".con")), "DYsim.nc")
  info_fils <- c("DYref.nc", "DYsim.nc", paste0(dy.prefix, c( ".cfg")))
  # Delete historic files
  unlink("DYsim.nc")
  unlink("DYref.nc")
  unlink("morphinterp.out")
  unlink("dy.log")

  stdout <- ifelse(verbose, "", TRUE)
  stderr <- ifelse(verbose, "", TRUE)
  message("DYRESM-CAEDYM running... [", format(Sys.time()), "]")
  # Create reference netcdf
  if (verbose) {
    system2(file.path(bin_path, "dy_cd", "createDYref.exe"),
            wait = TRUE, stdout = stdout,
            stderr = stderr,
            args = ref_fils)
  } else {
    out <- system2(file.path(bin_path, "dy_cd", "createDYref.exe"),
                   wait = TRUE, stdout = stdout,
                   stderr = stderr,
                   args = ref_fils, timeout = timeout)
    if (any(grepl("ERROR|Error", out))) {
      stop("Could not create DYRESM reference file:\n", paste0(out,
                                                               collapse = "\n"))
    }
  }

  # Create simulation file ----
  if (verbose) {
    system2(file.path(bin_path, "dy_cd", "createDYsim.exe"),
            wait = TRUE, stdout = stdout,
            stderr = stderr,
            args = sim_fils)
  } else {
    out <- system2(file.path(bin_path, "dy_cd", "createDYsim.exe"),
                   wait = TRUE, stdout = stdout, stderr = stderr,
                   args = sim_fils)

    if (any(grepl("ERROR|Error", out))) {
      stop("Could not create DYRESM simulation file:\n",
           paste0(out, collapse = "\n"))
    }
  }

  # Ext4act DYRESM info file ----
  if (verbose) {
    system2(file.path(bin_path, "dy_cd", "extractDYinfo.exe"),
            wait = TRUE, stdout = stdout,
            stderr = stderr,
            args = info_fils)
  } else {
    out <- system2(file.path(bin_path, "dy_cd", "extractDYinfo.exe"),
                   wait = TRUE, stdout = stdout, stderr = stderr,
                   args = info_fils)

    if (any(grepl("ERROR|Error", out))) {
      stop("Could not extract DYRESM-CAEDYM information:\n", paste0(out,
                                                             collapse = "\n"))
    }
  }

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
    message("DYRESM-CAEDYM run successful! [", format(Sys.time()), "]")
  } else {
    message("DYRESM-CAEDYM run FAILED! [", format(Sys.time()), "]\n",
            paste0(tail(out, 10), collapse = "\n"))
  }
}

#' Run GLM-AED
#'
#' @inheritParams run_dy_cd
#'
#' @importFrom utils tail
#' @return runs GLM-AED
#' @noRd

run_glm_aed <- function(sim_folder, verbose = FALSE, debug = FALSE,
                        timeout = 0) {

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- system.file('extbin/', package = "AEME")
  setwd(file.path(sim_folder, "glm_aed"))
  unlink("output/output.nc")
  message("GLM-AED running... [", format(Sys.time()), "]")
  sys_OS <- get_os()
  if (sys_OS == "windows") {
    bin_exec <- file.path(bin_path, "glm_aed", "glm.exe")
  } else if (sys_OS == "osx") {
    bin_exec <- file.path(bin_path, "glm_aed", "glm")
  }
  if (verbose) {
    system2(bin_exec,
            wait = TRUE, stdout = "",
            stderr = "", timeout = timeout)
  } else {
    out <- system2(bin_exec,
                   wait = TRUE, stdout = TRUE,
                   stderr = TRUE, timeout = timeout)
    success <- sum(grepl("Model Run Complete", out)) == 1
    if (success) {
      message("GLM-AED run successful! [", format(Sys.time()), "]")
    } else {
      print("FAILED")
      print(utils::tail(out, 10))
    }
  }
}

#' Run GOTM-WET
#'
#' @inheritParams run_dy_cd
#'
#' @return runs GOTM-WET
#' @noRd
#'
#' @importFrom utils tail

run_gotm_wet <- function(sim_folder, verbose = FALSE, debug = FALSE,
                         timeout = 0) {

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- system.file('extbin/', package = "AEME")
  setwd(file.path(sim_folder, "gotm_wet"))
  unlink("output/output.nc")
  dir.create("output", showWarnings = FALSE)
  message("GOTM-WET running... [", format(Sys.time()), "]")
  if (verbose) {
    system2(file.path(bin_path, "gotm_wet", "gotm.exe"),
            wait = TRUE, stdout = "",
            stderr = "", timeout = timeout)
  } else {
    out <- system2(file.path(bin_path, "gotm_wet", "gotm.exe"),
                   wait = TRUE, stdout = TRUE,
                   stderr = TRUE, timeout = timeout)
    success <- sum(grepl("GOTM-WET finished on|GOTM finished on", out)) == 1
    if (success) {
      message("GOTM-WET run successful! [", format(Sys.time()), "]")
    } else {
      print(utils::tail(out, 10))
    }
  }
}

#' Check model output
#' @noRd
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
