#' Run aquatic model ensemble
#'
#' @inheritParams build_aeme
#' @inheritParams processx::run
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
run_aeme <- function(aeme, model, 
                     return_type = c("aeme", "exec_result", "both", "none"),
                     ens_n = 1,
                     model_controls = NULL, verbose = FALSE,
                     debug = FALSE, timeout = Inf, parallel = FALSE, ncores,
                     check_output = FALSE, path = ".") {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  path <- check_path(path = path, must_exist = TRUE)
  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
  }
  return_type <- match.arg(return_type, choices = c("aeme", "exec_result",
                                                    "both", "none"))
  
  if (return_type == "aeme" & is.null(model_controls)) {
    cli::cli_abort(c("x" = "`model_controls` need to be provided to load model
                     output."))
  }
  
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  if (!dir.exists(lake_dir)) {
    # stop("Simulation folder does not exist.")
    cli::cli_abort(c("x" = "Simulation folder does not exist
                     {.path {lake_dir}}"))
  }
  sim_folder <- setNames(
    lapply(model, function(m) {
      file.path(lake_dir, m)
    }),
    model
  )
  
  # Check if model directories exist
  model_dir_chk <- !any(dir.exists(unlist(sim_folder)))
  if (model_dir_chk) {
    missing_model_dirs <- model[!dir.exists(unlist(sim_folder))]
    cli::cli_abort(c("x" = "Model folder(s) do not exist:
    {paste0(missing_model_dirs, collapse = ', ')}"))
  }
  
  # Delete previous model output if it exists
  model_output <- get_model_outfile(aeme = aeme, model = model, path = path)
  for (m in model) {
    if (any(file.exists(model_output[[m]]))) {
      unlink(model_output[[m]])
      cli_inform_safe(c("i" = paste0("Deleted previous output for model ",
                                     toggle_models(m, to = "display"),
                                     " at {.file ",
                                     model_output[[m]], "}")))
    }
  }
  
  # A lookup table of model runners
  model_funs <- list(
    dy_cd      = run_dy_cd,
    glm_aed    = run_glm_aed,
    gotm_wet   = run_gotm_wet
  )
  
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
    }, add = TRUE)
    parallel::clusterExport(cl,
                            varlist = c("model_funs", "run_model_args"),
                            envir = environment()
    )
    cli_inform_safe(c("i" = paste0("Running models in parallel... ", 
                                   "[", format(Sys.time()), "]")))
    exec_result <- setNames(
      parallel::parLapply(cl, model, function(m) {
        args <- run_model_args
        args$sim_folder <- args$sim_folder[[m]]
        do.call(model_funs[[m]], args)
      }),
      model
    )
    cli_inform_safe(c("v" = paste0("Model run complete! ",
                                   "[", format(Sys.time()), "]")))
    
  } else {
    cli_inform_safe(c("i" = paste0("Running models... (Have you tried ",
                                   "parallelizing?) ",
                                   "[", format(Sys.time()), "]")))
    exec_result <- Map(function(m, sim) {
      args <- run_model_args
      args$sim_folder <- sim
      do.call(model_funs[[m]], args)
    }, 
    model, 
    run_model_args$sim_folder[model]
    )
    
    cli_inform_safe(c("v" = paste0("Model run complete! ",
                                   "[", format(Sys.time()), "]")))
  }
  
  if ("none" %in% return_type) return(invisible(NULL))
  
  if (check_output) {
    cli_inform_safe(c("i" = "Checking model output..."))
    chk <- sapply(model, \(m) {
      check_model_output(path = path, aeme = aeme, model = m)
    })
    if (any(chk)) {
      cli_inform_safe(c("v" = paste0("Models ", paste0(model[chk],
                                                       collapse = ", "),
                                     " passed checks.")))
    }
    if (any(!chk)) {
      cli_inform_safe(c("!" = paste0("Warning: Models ",
                                     paste0(model[!chk], collapse = ", "),
                                     " failed checks.")))
    }
  }
  
  if ("aeme" %in% return_type | "both" %in% return_type) {
    aeme <- load_output(model = model, aeme = aeme, path = path,
                        model_controls = model_controls, parallel = parallel,
                        cl = cl, ens_n = ens_n)
  }
  
  # handle return type
  out <- switch(
    return_type,
    aeme    = aeme,
    exec_result = exec_result,
    both    = list(aeme = aeme, exec_result = exec_result),
    none    = NULL
  )
  return(out)
}


#' Run AEME models
#' 
#' @description
#' These functions provide a unified interface for running external
#' hydrodynamic or biogeochemical lake models from within R.  
#' Each function moves into the appropriate simulation directory,
#' executes the model's command-line binary, prints progress messages,
#' and reports whether the model run completed successfully.  
#' The functions are intended to be called for their side effects:
#' they do not return model results directly, but instead produce
#' model output files in the simulation folder.
#'
#' @param sim_folder the directory where simulation files are contained
#' @param verbose Logical: Should output of model be shown
#' @param debug Logical; save debug file. DYRESM only.
#' @inheritParams base::system2
#'
#' @return Invisibly returns `NULL`.
#' @export

run_dy_cd <- function(sim_folder, verbose = FALSE, debug = FALSE,
                      timeout = Inf) {
  
  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- system.file('extbin/', package = "AEME")
  
  arg <- ifelse(debug, "> dycd.log", "")
  
  dy.prefix <- gsub(".stg", "", list.files(sim_folder, pattern = "stg"))
  
  setwd(sim_folder)
  ref_fils <- c(paste0(dy.prefix, c(".stg", ".met", ".inf", ".wdr")),
                "DYref.nc")
  sim_fils <- c(paste0(dy.prefix, c(".pro")),  "dyresm3p1.par",
                paste0(dy.prefix, c(".con")), "DYsim.nc")
  info_fils <- c("DYref.nc", "DYsim.nc", paste0(dy.prefix, c( ".cfg")))
  # Delete historic files
  unlink("DYref.nc")
  unlink("morphinterp.out")
  unlink("dy.log")
  
  stdout <- ifelse(verbose, "", TRUE)
  stderr <- ifelse(verbose, "", TRUE)
  # message("DYRESM-CAEDYM running... [", format(Sys.time()), "]")
  cli_inform_safe(c(">" = paste0("DYRESM-CAEDYM running... ",
                                 "[", format(Sys.time()), "]")))
  # Create reference netcdf
  bin_exec <- file.path(bin_path, "dy_cd", "createDYref.exe")
  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = ref_fils,
      wd = sim_folder,
      echo = TRUE,               # print output live (closest to stdout="")
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    p <- processx::run(
      command = bin_exec,
      args = ref_fils,
      wd = sim_folder,
      spinner = FALSE,
      echo = FALSE,
      error_on_status = FALSE,  # so non-zero exit doesn't stop execution
      timeout = timeout
    )
    out <- unlist(strsplit(p$stdout, "\n", fixed = TRUE))
    if (any(grepl("ERROR|Error", out))) {
      cli::cli_abort(c("x" = "Could not create DYRESM reference file:\n",
                       paste0(out, collapse = "\n")))
    }
  }
  
  # Create simulation file ----
  bin_exec <- file.path(bin_path, "dy_cd", "createDYsim.exe")
  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = sim_fils,
      wd = sim_folder,
      echo = TRUE,               # print output live (closest to stdout="")
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    p <- processx::run(
      command = bin_exec,
      args = sim_fils,
      wd = sim_folder,
      spinner = FALSE,
      echo = FALSE,
      error_on_status = FALSE,  # so non-zero exit doesn't stop execution
      timeout = timeout
    )
    out <- unlist(strsplit(p$stdout, "\n", fixed = TRUE))
    
    if (any(grepl("ERROR|Error", out))) {
      cli::cli_abort(c("x" = "Could not create DYRESM simulation file:\n",
                       paste0(out, collapse = "\n")))
    }
  }
  
  # Extract DYRESM info file ----
  bin_exec <- file.path(bin_path, "dy_cd", "extractDYinfo.exe")
  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = info_fils,
      wd = sim_folder,
      echo = TRUE,               # print output live (closest to stdout="")
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    p <- processx::run(
      command = bin_exec,
      args = info_fils,
      wd = sim_folder,
      spinner = FALSE,
      echo = FALSE,
      error_on_status = FALSE,  # so non-zero exit doesn't stop execution
      timeout = timeout
    )
    out <- unlist(strsplit(p$stdout, "\n", fixed = TRUE))
    
    if (any(grepl("ERROR|Error", out))) {
      cli::cli_abort(c("x" = "Could not extract DYRESM-CAEDYM information:\n",
                       paste0(out, collapse = "\n")))
    }
  }
  
  bin_exec <- file.path(bin_path, "dy_cd", "dycd.exe")
  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = character(),
      wd = sim_folder,
      echo = TRUE,               # print output live (closest to stdout="")
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    p <- processx::run(
      command = bin_exec,
      args = character(),
      wd = sim_folder,
      spinner = FALSE,
      echo = FALSE,
      error_on_status = FALSE,  # so non-zero exit doesn't stop execution
      timeout = timeout
    )
    # p$stdout contains full captured output
    out <- unlist(strsplit(p$stdout, "\n", fixed = TRUE))
  }
  out <- readLines("dy.log")
  success <- sum(grepl("END DYRESM-CAEDYM", out)) == 1
  if (success) {
    cli_inform_safe(c("v" = paste0("DYRESM-CAEDYM run successful! ",
                                   "[", format(Sys.time()), "]")))
  } else {
    cli_inform_safe(c("!" = paste0("DYRESM-CAEDYM run FAILED! ",
                                   "[", format(Sys.time()), "]\n",
                                   paste0(tail(out, 10),
                                          collapse = "\n"))))
  }
  return(p)
}

#' @rdname run_dy_cd
#' @export
#' @importFrom processx run
run_glm_aed <- function(sim_folder, verbose = FALSE, debug = FALSE,
                        timeout = Inf) {
  
  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  setwd(sim_folder)
  cli_inform_safe(c(">" = paste0("GLM-AED running... ", "[",
                                 format(Sys.time()), "]")))
  
  # Allow user-specified executable path
  bin_exec <- getOption("AEME.glm_exec", default = NULL)
  if (is.null(bin_exec)) {
    bin_path <- system.file('extbin/', package = "AEME")
    sys_OS <- get_os()
    
    bin_exec <- switch(sys_OS,
                       "windows" = file.path(bin_path, "glm_aed", "windows", 
                                             "glm.exe"),
                       "osx" = file.path(bin_path, "glm_aed", "macos", "glm"),
                       "linux" = file.path(bin_path, "glm_aed", "linux", "glm")
    )
  }
  
  if (verbose) {
    # Stream stdout directly to console (similar to stdout = "")
    p <- processx::run(
      command = bin_exec,
      args = character(),
      wd = sim_folder,
      echo = TRUE,               # print output live (closest to stdout="")
      error_on_status = FALSE,
      timeout = timeout
    )
    # system2(bin_exec,
    #         wait = TRUE, stdout = "",
    #         stderr = "", timeout = timeout)
  } else {
    # Capture stdout/stderr (similar to stdout=TRUE, stderr=TRUE)
    p <- processx::run(
      command = bin_exec,
      args = character(),
      wd = sim_folder,
      spinner = FALSE,
      echo = FALSE,
      error_on_status = FALSE,  # so non-zero exit doesn't stop execution
      timeout = timeout
    )
    # p$stdout contains full captured output
    out <- unlist(strsplit(p$stdout, "\n", fixed = TRUE))
    success <- sum(grepl("Model Run Complete", out)) == 1
    if (success) {
      # message("GLM-AED run successful! [", format(Sys.time()), "]")
      cli_inform_safe(c("v" = paste0("GLM-AED2 run successful! ",
                                     "[", format(Sys.time()), "]")))
    } else {
      cli_inform_safe(c("!" = paste0("GLM-AED2 run FAILED! ",
                                     "[", format(Sys.time()), "]\n",
                                     paste0(tail(out, 10),
                                            collapse = "\n"))))
    }
  }
  return(p)
}

#' @rdname run_dy_cd
#' @export
run_gotm_wet <- function(sim_folder, verbose = FALSE, debug = FALSE,
                         timeout = Inf) {
  
  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- system.file('extbin/', package = "AEME")
  setwd(sim_folder)
  dir.create("output", showWarnings = FALSE)
  cli_inform_safe(c(">" = paste0("GOTM-WET running... ",
                                 "[", format(Sys.time()), "]")))
  bin_exec <- file.path(bin_path, "gotm_wet", "gotm.exe")
  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = character(),
      wd = sim_folder,
      echo = TRUE,               # print output live (closest to stdout="")
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    # Capture stdout/stderr (similar to stdout=TRUE, stderr=TRUE)
    p <- processx::run(
      command = bin_exec,
      args = character(),
      wd = sim_folder,
      spinner = FALSE,
      echo = FALSE,
      error_on_status = FALSE,  # so non-zero exit doesn't stop execution
      timeout = timeout
    )
    # p$stdout contains full captured output
    out <- p$stderr
    success <- sum(grepl("GOTM-WET finished on|GOTM finished on", out)) == 1
    if (success) {
      cli_inform_safe(c("v" = paste0("GOTM-WET run successful! ",
                                     "[", format(Sys.time()), "]")))
    } else {
      cli_inform_safe(c("!" = paste0("GOTM-WET run FAILED! ",
                                     "[", format(Sys.time()), "]\n",
                                     paste0(tail(out, 10),
                                            collapse = "\n"))))
    }
  }
  return(p)
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
  return(tolower(os))
}

#' Get GLM-AED model version
#' @return version string
#' @noRd
get_glm_aed_version <- function() {
  
  # Allow user-specified executable path
  bin_exec <- getOption("AEME.glm_exec", default = NULL)
  if (is.null(bin_exec)) {
    bin_path <- system.file('extbin/', package = "AEME")
    bin_exec <- file.path(bin_path, "glm_aed", get_os(), "glm")
    bin_exec <- ifelse(get_os() == "windows",
                       file.path(bin_path, "glm_aed", "windows", "glm.exe"),
                       bin_exec)
  }
  vers <- system2(bin_exec, args = "--version", stdout = TRUE)
  return(vers)
}

#' Get GOTM-WET model version
#' @return version string
#' @noRd
get_gotm_wet_version <- function() {
  bin_path <- system.file('extbin/', package = "AEME")
  gotm_exec <- ifelse(get_os() == "windows",
                      file.path(bin_path, "gotm_wet", "gotm.exe"),
                      file.path(bin_path, "gotm_wet", "gotm"))
  vers <- system2(gotm_exec, args = "--version", stdout = TRUE)
  return(vers)
}

#' Get DYRESM-CAEDYM model version
#' @return version string
#' @noRd
get_dy_cd_version <- function() {
  bin_path <- system.file('extbin/', package = "AEME")
  dycd_readme <- file.path(bin_path, "dy_cd", "README_DY3p1p0-CD3p1p0.txt")
  vers <- readLines(dycd_readme, n = 9)
  return(vers)
}

#' Get model version
#' @param model model name. Only "glm_aed", "gotm_wet", and "dy_cd" are
#' supported. 
#' @return version string
#' @export
get_model_version <- function(model) {
  if (length(model) > 1) {
    cli::cli_abort("Only one model can be checked at a time.")
  }
  model <- check_model(model = model)
  if (model == "glm_aed") {
    vers <- get_glm_aed_version()
  } else if (model == "gotm_wet") {
    vers <- get_gotm_wet_version()
  } else if (model == "dy_cd") {
    vers <- get_dy_cd_version()
  } else {
    vers <- NA
  }
  return(vers)
}
