#' Input model parameters
#'
#' @inheritParams build_aeme
#' @param param data.frame; parameters to input into the model
#' configuration files
#'
#' @return Aeme object with parameters input into model configuration files
#' @export
#'

input_model_parameters <- function(aeme, model, param, path) {
  
  # Function checks ----
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  path <- check_path(path = path, must_exist = TRUE)
  if (!is.data.frame(param))
    stop("'param' must be a data.frame.")
  
  # Collapse parameters
  param <- collapse_params(param)
  
  # Load AEME data
  lake_dir <- get_lake_dir(aeme, path)
  inp <- input(aeme)
  
  # Check model parameters in supplied parameters
  for (m in model) {
    if (!m %in% param[["model"]]) {
      cli::cli_warn(paste0("No parameters in 'param' for ", m, "."))
    }
  }
  
  sapply(model, \(m) {
    
    all_p <- param[param$model == m, ] # Subset parameters to model specific
    if (nrow(all_p) == 0) return(NULL)
    
    # Boundary-condition scaling ----
    # Scaled copies of the boundary data are written in the model's own
    # format (met, outflows and inflows are only written when a parameter
    # scales them); the data in `aeme` is left as it is.
    bnd <- apply_boundary_params(meteo = inp$meteo,
                                 outflows = outflows(aeme)[["data"]],
                                 param = all_p)
    write_boundary_data(
      aeme = aeme, model = m, lake_dir = lake_dir,
      meteo = bnd$meteo, outf = bnd$outflows,
      inf = if (!is.null(bnd$inf_factor)) inflows(aeme)[["data"]],
      inf_factor = if (is.null(bnd$inf_factor)) 1 else bnd$inf_factor
    )
    
    # Inputting model parameters ----
    # Pure config patch (apply_parameters()), then write back only the files
    # that changed. Skipped when the parameters only scale boundary conditions.
    if (any(!all_p$file %in% c("met", "inf", "wdr"))) {
      files <- locate_config_files(m, lake_dir)
      cfg <- read_config_for_params(model = m, lake_dir = lake_dir,
                                  labels = all_p$file, files = files)
      res <- apply_parameters(config = cfg, param = all_p, model = m)
      write_params(res, lake_dir = lake_dir, model = m, files = files)
    }

  })
  return(invisible(aeme))
}

#' Recycle scalar AED sediment fluxes to the number of sediment zones
#'
#' Parameter tables (including the shipped defaults) carry one `fsed_*` value,
#' but AED needs one per zone, so a scalar would otherwise leave
#' `aed_sed_const2d` internally inconsistent for a multi-zone lake.
#'
#' @param nml nml object for aed.nml.
#' @param arg_list named list of `block::name` values about to be set.
#' @return `arg_list`, with scalar `aed_sed_const2d::fsed_*` values recycled
#'   to `aed_sed_const2d$n_zones`.
#' @noRd
recycle_sed_fluxes <- function(nml, arg_list) {
  nz <- suppressWarnings(as.integer(nml[["aed_sed_const2d"]][["n_zones"]]))
  if (length(nz) != 1 || is.na(nz) || nz < 2) return(arg_list)
  for (nm in grep("^aed_sed_const2d::fsed_", names(arg_list), value = TRUE)) {
    if (length(arg_list[[nm]]) == 1) arg_list[[nm]] <- rep(arg_list[[nm]], nz)
  }
  arg_list
}

#' Warn when parameter keys are absent from a model nml
#'
#' `set_nml()` silently creates keys that don't exist, so a typo or a key
#' renamed between model versions would otherwise pass unnoticed.
#'
#' @param nml nml object read with `read_nml()`.
#' @param keys character; `block::name` keys about to be set.
#' @param file character; file name used in the warning.
#' @noRd
warn_missing_nml_keys <- function(nml, keys, file) {
  missing_keys <- Filter(function(k) {
    parts <- strsplit(k, "::")[[1]]
    length(parts) == 2 && !(parts[1] %in% names(nml) &&
                              parts[2] %in% names(nml[[parts[1]]]))
  }, keys)
  if (length(missing_keys) > 0) {
    cli::cli_warn(c(
      "Parameter(s) not found in {.file {file}}: {.val {unlist(missing_keys)}}.",
      "i" = "They will be added; check the names are correct."
    ))
  }
  invisible(missing_keys)
}

#' Warn when a nested parameter path is absent from a yaml/json config
#'
#' @param x list; parsed yaml/json config.
#' @param path character; nested key path.
#' @param file character; file name used in the warning.
#' @noRd
warn_missing_list_keys <- function(x, path, file) {
  node <- x
  for (k in path) {
    if (!is.list(node) || !k %in% names(node)) {
      cli::cli_warn(c(
        "Parameter {.val {paste(path, collapse = '/')}} not found in {.file {file}}.",
        "i" = "The key will be added; check the name is correct."
      ))
      return(invisible(FALSE))
    }
    node <- node[[k]]
  }
  invisible(TRUE)
}

#' Collapse model parameters
#'
#' @param param_df data.frame; parameters to collapse
#'
#' @return data.frame; collapsed parameters
#' @noRd
collapse_params <- function(param_df) {
  req_col_names <- c("model", "file", "name", "value", "min", "max", "group",
                     "index")
  if (!all(req_col_names %in% names(param_df))) {
    stop(paste0("param_df must contain the following columns: ",
                paste(req_col_names, collapse = ", "), "."))
  }
  
  param_df |>
    dplyr::group_by(model, file, name, group) |>
    dplyr::arrange(index, .by_group = TRUE) |>
    dplyr::summarise(
      value = list(value),
      min   = list(min),
      max   = list(max),
      .groups = "drop"
    ) |>
    tibble::as_tibble()
}

