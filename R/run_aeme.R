#' Run aquatic model ensemble
#'
#' @inheritParams build_aeme
#' @inheritParams processx::run
#' @param return_type character; one of `"aeme"` (default), `"exec_result"`,
#' `"both"`, or `"none"`. `"aeme"` returns the `aeme` object with model output
#' loaded; `"exec_result"` returns the raw `processx::run()` result(s) for
#' each model; `"both"` returns a list with both `aeme` and `exec_result`
#' elements; `"none"` returns `NULL` invisibly (useful when only the model
#' run's side effects, i.e. the output files, are wanted).
#' @inheritParams load_output
#' @param verbose logical; print model output to console. Defaults to
#'  `getOption("AEME.inform", FALSE)`.
#' @param debug logical; write debug log (Only DYRESM). Defaults to FALSE.
#' @param parallel logical; run models in parallel. Defaults to FALSE.
#' @param ncores integer; number of cores to use for parallelization. Defaults
#' to `min(c(detectCores() - 1, length(model)))`.
#' @param check_output logical; check model output after running? Defaults to
#' FALSE.
#' @param ens_n numeric; ensemble number to allocate to model output which is
#' loaded. Defaults to 1.
#'
#' @return Depends on `return_type` -- an `aeme` object with model output
#' loaded (`"aeme"`), the raw `processx::run()` result(s) (`"exec_result"`),
#' a list with both (`"both"`), or `NULL` invisibly (`"none"`).
#' @export
#'
#' @importFrom parallel parLapply makeCluster detectCores clusterExport
#' @importFrom parallel stopCluster
#' @importFrom stats setNames
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model_controls <- get_model_controls()
#' model <- c("glm_aed")
#' aeme <- build_aeme(path = path, aeme = aeme, model = model,
#'                    model_controls = model_controls, ext_elev = 5)
#' \dontrun{
#' aeme <- run_aeme(aeme)
#'
#' # Plot model output - temperature by default
#' plot_output(aeme)
#' }
run_aeme <- function(aeme, model, path, args = character(),
                     return_type = c("aeme", "exec_result", "both", "none"),
                     ens_n = 1,
                     model_controls = NULL, 
                     verbose = getOption("AEME.inform", FALSE),
                     debug = FALSE, timeout = Inf, parallel = FALSE, ncores,
                     check_output = FALSE) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  model <- check_model(model = model, os_valid = TRUE)
  aeme <- set_model(aeme = aeme, model = model)
  if (missing(path)) {
    path <- get_aeme_path(aeme)
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
  model_output <- get_model_outfile(aeme = aeme, model = model)
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
    gotm_wet   = run_gotm_wet,
    simstrat_aed2 = run_simstrat_aed2
  )
  
  run_model_args <- list(sim_folder = sim_folder, verbose = verbose,
                         debug = debug, args = args, timeout = timeout)
  
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
    glm_version <- getOption("AEME.glm_version", default = NULL)
    if (!is.null(glm_version)) {
      parallel::clusterCall(cl, function(v) options(AEME.glm_version = v),
                            glm_version)
    }
    cli_inform_safe(c("i" = paste0("Running models in parallel... ", 
                                   "[", format(Sys.time()), "]")))
    exec_result <- setNames(
      parallel::parLapply(cl, model, function(m) {
        args <- run_model_args
        args$sim_folder <- args$sim_folder[[m]]
        do.call(model_funs[[m]], args)
      }),
      names(model)
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
    
    model_check <- sapply(names(model), function(m) {
      exec_result[[m]]$status == 0
    })
    model_success <- model[model_check]
    if (length(model_success) < length(model)) {
      cli_inform_safe(c("!" = paste0("Warning: Some model runs failed and
                                     will not be loaded: ",
                                     paste0(model[!model_check],
                                            collapse = ", "))))
    }
    
    if (length(model_success) > 0) {
      aeme <- load_output(model = model, aeme = aeme, path = path,
                          model_controls = model_controls, parallel = parallel,
                          cl = cl, ens_n = ens_n)
    } else {
      cli::cli_warn(c("!" = "No model output loaded as all model runs failed."))
    }
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
#' @param args character vector of additional command-line arguments to pass to
#'  the model executable. Currently only used for GLM-AED. Options are: 
#'  "--xdisp" to plot the model output using the plots.nml settings.
#' @inheritParams base::system2
#' @param version character; specific version of the model to run. If not 
#' provided, the default version bundled with the package will be used. For 
#' GLM-AED and GOTM-WET, this can also be set via the `AEME.glm_version` or 
#' `AEME.gotm_version` options, respectively. For DYRESM-CAEDYM, use 
#' `AEME.dyresm_version`. Currently, only GLM-AED support version selection; 
#' GOTM-WET and DYRESM-CAEDYM always uses the bundled version.
#'
#' @return Invisibly returns `NULL`.
#' @export

run_dy_cd <- function(sim_folder, verbose = FALSE, debug = FALSE,
                      args = character(), timeout = Inf,
                      version = getOption("AEME.dyresm_version", default = NULL)) {
  
  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  bin_path <- dirname(.resolve_dy_cd_exec(version))

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
  bin_exec <- file.path(bin_path, "createDYref.exe")
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
      spinner = TRUE,
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
  bin_exec <- file.path(bin_path, "createDYsim.exe")
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
      spinner = TRUE,
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
  bin_exec <- file.path(bin_path, "extractDYinfo.exe")
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
      spinner = TRUE,
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
  
  bin_exec <- file.path(bin_path, "dycd.exe")
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
      spinner = TRUE,
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
    cli_inform_safe(c(
      "!" = paste0(
        "DYRESM-CAEDYM run FAILED! ",
        "[", format(Sys.time()), "]"
      )
    ))
    
    # Emit raw stderr safely (no cli wrapping)
    msg <- paste(tail(out, 10), collapse = "\n")
    
    # Strip ANSI just in case
    msg <- gsub("\033\\[[0-9;]*m", "", msg)
    
    message(msg)
    
  }
  return(p)
}

#' Resolve which GLM executable to run
#'
#' Picks the GLM binary to use, in priority order: an explicit
#' `AEME.glm_exec` option (unchanged, always wins), a specific downloaded
#' version (`version` argument or `AEME.glm_version` option, resolved via
#' [glm_exe_path()]), or - if neither is set - whatever version is already
#' installed on disk (checked directly rather than trusting session state).
#' There is no bundled fallback - GLM-AED binaries are only ever obtained
#' via [install_glm_aed()].
#'
#' @keywords internal
#' @noRd
.resolve_glm_exec <- function(version = NULL) {
  # 1. Explicit low-level override always wins.
  bin_exec <- getOption("AEME.glm_exec", default = NULL)
  if (!is.null(bin_exec)) {
    if (!file.exists(bin_exec)) {
      cli::cli_abort(
        "{.envvar AEME.glm_exec} points to {.path {bin_exec}}, but that file doesn't exist."
      )
    }
    return(.ensure_executable(bin_exec))
  }

  sys_OS <- .detect_os()

  # 2. A specific version was requested (explicit argument, or via a
  #    caller's own AEME.glm_version default, when that happens to be set).
  if (!is.null(version)) {
    return(.ensure_executable(glm_exe_path(version, os = sys_OS)))
  }

  # 3. Nothing requested. Don't depend on some earlier call having correctly
  #    set AEME.glm_version in *this* process/session - that's proven
  #    fragile across parallel workers, cache-hit install paths, and
  #    callers with their own hardcoded NULL defaults. Instead, check what's
  #    actually installed on disk, which is authoritative regardless of
  #    session state.
  latest <- .glm_latest_installed_version(sys_OS)
  if (!is.null(latest)) {
    options(AEME.glm_version = latest)  # sync session state for next time
    return(.ensure_executable(glm_exe_path(latest, os = sys_OS)))
  }

  cli::cli_abort(c(
    "x" = "No GLM-AED binary found for {.field {sys_OS}}.",
    "i" = "Install one with {.run install_glm_aed(version = \"3.9.108\")}."
  ))
}

#' Make sure a resolved GLM binary is actually executable
#'
#' Don't trust git/tar/R CMD build to have preserved the executable bit
#' through packaging - set it explicitly right before use, for every
#' source (bundled, downloaded, or user-supplied via AEME.glm_exec). A
#' no-op if it's already executable.
#' @keywords internal
#' @noRd
.ensure_executable <- function(path) {
  if (.detect_os() != "windows" && file.exists(path)) {
    Sys.chmod(path, mode = "0755")
  }
  path
}

#' Resolve which GOTM-WET executable to run
#'
#' Picks the GOTM-WET binary to use, in priority order: an explicit
#' `AEME.gotm_exec` option (always wins), a specific installed version
#' (`version` argument or `AEME.gotm_version` option, resolved via
#' [gotm_wet_exe_path()]), or - if neither is set - whatever version is
#' already installed on disk (checked directly rather than trusting session
#' state). Unlike GLM, there
#' is no bundled fallback - GOTM-WET binaries are only ever obtained via
#' [install_gotm_wet()].
#'
#' @keywords internal
#' @noRd
.resolve_gotm_exec <- function(version = NULL) {
  bin_exec <- getOption("AEME.gotm_exec", default = NULL)
  if (!is.null(bin_exec)) {
    if (!file.exists(bin_exec)) {
      cli::cli_abort(
        "{.envvar AEME.gotm_exec} points to {.path {bin_exec}}, but that file doesn't exist."
      )
    }
    return(.ensure_executable(bin_exec))
  }

  sys_OS <- .detect_os()

  if (!is.null(version)) {
    return(.ensure_executable(gotm_wet_exe_path(version, os = sys_OS)))
  }

  latest <- .gotm_latest_installed_version(sys_OS)
  if (!is.null(latest)) {
    options(AEME.gotm_version = latest)  # sync session state for next time
    return(.ensure_executable(gotm_wet_exe_path(latest, os = sys_OS)))
  }

  cli::cli_abort(c(
    "x" = "No GOTM-WET binary found for {.field {sys_OS}}.",
    "i" = "Install one with {.run install_gotm_wet()}."
  ))
}

#' Resolve which DYRESM-CAEDYM executable directory to run
#'
#' Picks the DYRESM-CAEDYM install to use, in priority order: an explicit
#' `AEME.dyresm_exec` option (pointing at `dycd.exe`, always wins), a
#' specific installed version (`version` argument or `AEME.dyresm_version`
#' option, resolved via [dy_cd_exe_path()]), or - if neither is set -
#' whatever version is already installed on disk. Unlike GLM, there is no
#' bundled fallback - DYRESM-CAEDYM binaries are only ever obtained via
#' [install_dy_cd()]. Returns the path to `dycd.exe`; its three companion
#' tools (`createDYref.exe`, `createDYsim.exe`, `extractDYinfo.exe`) sit
#' alongside it in the same directory.
#'
#' @keywords internal
#' @noRd
.resolve_dy_cd_exec <- function(version = NULL) {
  bin_exec <- getOption("AEME.dyresm_exec", default = NULL)
  if (!is.null(bin_exec)) {
    if (!file.exists(bin_exec)) {
      cli::cli_abort(
        "{.envvar AEME.dyresm_exec} points to {.path {bin_exec}}, but that file doesn't exist."
      )
    }
    return(.ensure_executable(bin_exec))
  }

  sys_OS <- .detect_os()

  if (!is.null(version)) {
    return(.ensure_executable(dy_cd_exe_path(version, os = sys_OS)))
  }

  latest <- .dy_cd_latest_installed_version(sys_OS)
  if (!is.null(latest)) {
    options(AEME.dyresm_version = latest)  # sync session state for next time
    return(.ensure_executable(dy_cd_exe_path(latest, os = sys_OS)))
  }

  cli::cli_abort(c(
    "x" = "No DYRESM-CAEDYM binary found for {.field {sys_OS}}.",
    "i" = "Install one with {.run install_dy_cd()}."
  ))
}

#' Resolve which Simstrat-AED2 executable to run
#'
#' Picks the Simstrat-AED2 binary to use, in priority order: an explicit
#' `AEME.simstrat_exec` option (always wins), a specific installed version
#' (`version` argument or `AEME.simstrat_version` option, resolved via
#' [simstrat_aed2_exe_path()]), or - if neither is set - whatever version is
#' already installed on disk. Unlike GLM, there is no bundled fallback -
#' Simstrat-AED2 binaries are only ever obtained via
#' [install_simstrat_aed2()].
#'
#' @keywords internal
#' @noRd
.resolve_simstrat_aed2_exec <- function(version = NULL) {
  bin_exec <- getOption("AEME.simstrat_exec", default = NULL)
  if (!is.null(bin_exec)) {
    if (!file.exists(bin_exec)) {
      cli::cli_abort(
        "{.envvar AEME.simstrat_exec} points to {.path {bin_exec}}, but that file doesn't exist."
      )
    }
    return(.ensure_executable(bin_exec))
  }

  sys_OS <- .detect_os()

  if (!is.null(version)) {
    return(.ensure_executable(simstrat_aed2_exe_path(version, os = sys_OS)))
  }

  latest <- .simstrat_latest_installed_version(sys_OS)
  if (!is.null(latest)) {
    options(AEME.simstrat_version = latest)  # sync session state for next time
    return(.ensure_executable(simstrat_aed2_exe_path(latest, os = sys_OS)))
  }

  cli::cli_abort(c(
    "x" = "No Simstrat-AED2 binary found for {.field {sys_OS}}.",
    "i" = "Install one with {.run install_simstrat_aed2()}."
  ))
}

#' @rdname run_dy_cd
#' @export
#' @importFrom processx run
run_glm_aed <- function(sim_folder, verbose = FALSE, debug = FALSE,
                        args = character(), timeout = Inf,
                        version = getOption("AEME.glm_version", default = NULL)) {
  
  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  setwd(sim_folder)
  cli_inform_safe(c(">" = paste0("GLM-AED running... ", "[",
                                 format(Sys.time()), "]")))
  
  bin_exec <- .resolve_glm_exec(version)
  
  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = args,
      wd = sim_folder,
      echo = TRUE,
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    p <- processx::run(
      command = bin_exec,
      args = args,
      wd = sim_folder,
      spinner = TRUE,
      echo = FALSE,
      error_on_status = FALSE,
      timeout = timeout
    )
    out <- unlist(strsplit(p$stdout, "\n", fixed = TRUE))
    success <- sum(grepl("Model Run Complete", out)) == 1
    if (success) {
      cli_inform_safe(c("v" = paste0("GLM-AED run successful! ",
                                     "[", format(Sys.time()), "]")))
    } else {
      cli_inform_safe(c(
        "!" = paste0(
          "GLM-AED run FAILED! ",
          "[", format(Sys.time()), "]"
        )
      ))
      msg <- paste(tail(out, 10), collapse = "\n")
      msg <- gsub("\033\\[[0-9;]*m", "", msg)
      message(msg)
    }
  }
  return(p)
}

#' @rdname run_dy_cd
#' @export
run_gotm_wet <- function(sim_folder, verbose = FALSE, debug = FALSE,
                         args = character(), timeout = Inf,
                         version = getOption("AEME.gotm_version", default = NULL)) {
  
  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  setwd(sim_folder)
  dir.create("output", showWarnings = FALSE)
  cli_inform_safe(c(">" = paste0("GOTM-WET running... ",
                                 "[", format(Sys.time()), "]")))
  bin_exec <- .resolve_gotm_exec(version)
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
      spinner = TRUE,
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
      cli_inform_safe(c(
        "!" = paste0(
          "GOTM-WET run FAILED! ",
          "[", format(Sys.time()), "]"
        )
      ))
      
      # Emit raw stderr safely (no cli wrapping)
      msg <- paste(tail(out, 10), collapse = "\n")
      
      # Strip ANSI just in case
      msg <- gsub("\033\\[[0-9;]*m", "", msg)
      
      message(msg)
    }
  }
  return(p)
}

#' @rdname run_dy_cd
#' @export
run_simstrat_aed2 <- function(sim_folder, verbose = FALSE, debug = FALSE,
                              args = character(), timeout = Inf,
                              version = getOption("AEME.simstrat_version", default = NULL)) {

  oldwd <- getwd()
  on.exit({
    setwd(oldwd)
  })
  setwd(sim_folder)
  cli_inform_safe(c(">" = paste0("Simstrat-AED2 running... ",
                                 "[", format(Sys.time()), "]")))

  bin_exec <- .resolve_simstrat_aed2_exec(version)

  if (verbose) {
    p <- processx::run(
      command = bin_exec,
      args = c("simstrat.par", args),
      wd = sim_folder,
      echo = TRUE,
      error_on_status = FALSE,
      timeout = timeout
    )
  } else {
    p <- processx::run(
      command = bin_exec,
      args = c("simstrat.par", args),
      wd = sim_folder,
      spinner = TRUE,
      echo = FALSE,
      error_on_status = FALSE,
      timeout = timeout
    )
  }
  out <- unlist(strsplit(c(p$stdout, p$stderr), "\n", fixed = TRUE))
  # Success is judged by exit status, not by the "SIMULATION COMPLETED"
  # banner -- that banner is only printed when Simulation.DisplaySimulation
  # != 0 in simstrat.par (see strat_outputfile.f90::log_close()), so it
  # cannot be relied on unconditionally.
  success <- isTRUE(p$status == 0)
  if (success) {
    # Occasionally (observed intermittently, cause not isolated -- possibly
    # antivirus/file-system interference with the freshly-written config
    # directory) the process exits with status 0 in ~1 second without
    # producing any output at all, instead of the ~15-25s a real run takes.
    # Treat that as a failure rather than let it cascade into a confusing
    # netCDF error downstream in load_output().
    nc_file <- tryCatch(
      write_simstrat_nc(sim_folder = sim_folder),
      error = function(e) {
        cli::cli_warn(c("!" = "Simstrat-AED2 ran successfully but converting
                        output to netCDF failed: {conditionMessage(e)}"))
        NULL
      }
    )
    if (is.null(nc_file)) {
      success <- FALSE
      p$status <- 1L
    } else {
      cli_inform_safe(c("v" = paste0("Simstrat-AED2 run successful! ",
                                     "[", format(Sys.time()), "]")))
    }
  }
  if (!success) {
    cli_inform_safe(c(
      "!" = paste0(
        "Simstrat-AED2 run FAILED! ",
        "[", format(Sys.time()), "]"
      )
    ))
    msg <- paste(utils::tail(out, 10), collapse = "\n")
    msg <- gsub("\033\\[[0-9;]*m", "", msg)
    message(msg)
  }
  return(p)
}

#' Check model output
#' @noRd
.detect_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "macos"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "macos"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

#' Get GLM-AED model version
#' @return version string
#' @importFrom processx run
#' @noRd
get_glm_aed_version <- function(version = NULL) {
  # Allow user-specified executable path
  bin_exec <- .resolve_glm_exec(version)
  res <- processx::run(
    command = bin_exec,
    args = "--version",
    error_on_status = FALSE
  )
  if (res$status != 0) {
    cli::cli_abort(c(
      "GLM exited with status {res$status} when run with {.code --version}.",
      "i" = "command: {.path {bin_exec}}",
      "i" = "stdout: {res$stdout}",
      "i" = "stderr: {res$stderr}"
    ))
  }
  cat(res$stdout)
  return(res$stdout)
}

#' Get GOTM-WET model version
#' @return version string
#' @importFrom processx run
#' @noRd
get_gotm_wet_version <- function() {
  gotm_exec <- .resolve_gotm_exec()
  res <- processx::run(
    command = gotm_exec,
    args = "--version",
    error_on_status = FALSE
  )
  if (res$status != 0) {
    cli::cli_abort(c(
      "GOTM exited with status {res$status} when run with {.code --version}.",
      "i" = "command: {.path {gotm_exec}}",
      "i" = "stdout: {res$stdout}",
      "i" = "stderr: {res$stderr}"
    ))
  }
  cat(res$stderr)
  return(res$stderr)
}

#' Get Simstrat-AED2 model version
#' @return version string
#' @noRd
get_simstrat_aed2_version <- function() {
  bin_exec <- .resolve_simstrat_aed2_exec()
  vers <- system2(bin_exec, stdout = TRUE)
  return(trimws(vers[grepl("Simstrat version", vers)]))
}

#' Get DYRESM-CAEDYM model version
#' @return version string
#' @noRd
get_dy_cd_version <- function() {
  bin_path <- dirname(.resolve_dy_cd_exec())
  dycd_readme <- file.path(bin_path, "README_DY3p1p0-CD3p1p0.txt")
  if (!file.exists(dycd_readme)) {
    vers <- getOption("AEME.dyresm_version", default = NA_character_)
    cat(vers)
    return(vers)
  }
  vers <- readLines(dycd_readme, n = 9)
  cat(vers)
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
  } else if (model == "simstrat_aed2") {
    vers <- get_simstrat_aed2_version()
  } else {
    cli::cli_abort(c("x" = "Model {.field {model}} is not supported for version
                     checking."))
  }
  return(vers)
}
