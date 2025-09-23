#' Check if the package is working correctly
#'
#' @returns TRUE if the package is working correctly
#' @export

check_AEME_pkg <- function() {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  suppressWarnings({
    suppressMessages({
    aeme <- yaml_to_aeme(path = path, "aeme.yaml")
    })
  })
  message("Check: Loading Aeme object complete! [", format(Sys.time()), "]")
  lke <- lake(aeme)
  model_controls <- get_model_controls(use_bgc = FALSE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd", "glm_aed", "gotm_wet")

  suppressWarnings({
    suppressMessages({
      aeme <- build_aeme(path = path, aeme = aeme, model = model,
                         model_controls = model_controls, ext_elev = 2)
    })
  })
  message("Check: Build Aeme model ensemble configuration complete! [",
          format(Sys.time()), "]")

  message("Check: Running Aeme model ensemble... [", format(Sys.time()), "]")
  suppressWarnings({
    suppressMessages({
      aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                   model_controls = model_controls, path = path,
                   parallel = TRUE, ncores = 2)
    })
  })
  message("Check: Run Aeme model ensemble complete! [", format(Sys.time()), "]")

  dy_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model[1], "DYsim.nc"))

  mod_chk <- all(file.exists(file.path(path, paste0(lke$id, "_",
                                                     tolower(lke$name)),
                                        model[-1], "output", "output.nc")))
  message("Check: Aeme model output present complete! [", format(Sys.time()),
          "]")

  all(c(dy_chk, mod_chk))
}
