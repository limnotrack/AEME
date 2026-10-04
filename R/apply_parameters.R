#' Apply model parameters to an in-memory model configuration
#'
#' Pure counterpart of the file-editing branches in
#' [input_model_parameters()]: takes a model's configuration as held in
#' `configuration(aeme)[[model]]` (or returned by [read_model_config()]) and
#' returns it with the parameter values set. Nothing is read from or written
#' to disk, and `config` is not modified, so applying the same parameters to
#' the same configuration always gives the same result.
#'
#' Only the configuration-file parameters are handled here. Parameters that
#' scale the boundary-condition data (`file` of `"met"`, `"inf"`, `"wdr"`) are
#' ignored.
#'
#' @param config list; model configuration with `hydrodynamic` and (optionally)
#'   `bgc` elements, as returned by [read_model_config()].
#' @param param data.frame; parameters in the AEME format. Either raw (one row
#'   per value) or already collapsed by `collapse_params()` (list-column
#'   `value`).
#' @param model character; model name, one of `"glm_aed"`, `"gotm_wet"`,
#'   `"simstrat_aed2"`, `"simstrat_aed"` or `"dy_cd"`.
#' @param strict logical; if `TRUE` (default), parameters for a file that is
#'   not in `config` (e.g. bgc parameters for a model built without bgc) are an
#'   error. If `FALSE` they are skipped with a warning.
#'
#' @return A list with
#'   \item{config}{the patched configuration.}
#'   \item{touched}{character; labels (as used in the `file` column of
#'   `param`, e.g. `"glm4.nml"`, `"aed.nml"`, `"aed_phyto_pars.csv"`) of the
#'   configuration files that were changed, so a caller can write back only
#'   those.}
#' @noRd
apply_parameters <- function(config, param, model, strict = TRUE) {
  model <- check_model(model)
  if (length(model) != 1) {
    cli::cli_abort("Please provide only one model at a time.")
  }
  if (!is.list(param[["value"]])) param <- collapse_params(param)
  param <- param[param$model == model, , drop = FALSE]

  switch(
    model,
    glm_aed = apply_params_glm_aed(config, param, strict),
    gotm_wet = apply_params_gotm_wet(config, param, strict),
    simstrat_aed2 = ,
    simstrat_aed = apply_params_simstrat(config, param, model, strict),
    dy_cd = apply_params_dy_cd(config, param, strict),
    cli::cli_abort("{.fn apply_parameters} does not yet support {.val {model}}.")
  )
}

#' @rdname apply_parameters
#' @param all_p collapsed parameters for the model.
#' @noRd
apply_params_glm_aed <- function(config, all_p, strict = TRUE) {
  touched <- character(0)
  if (nrow(all_p) == 0) return(list(config = config, touched = touched))

  # The parameter catalogue tags the GLM hydrodynamic nml by version
  # ("glm3.nml", historically; "glm4.nml" from newer catalogues /
  # calc_sed_temp()). AEME treats any `glm<version>.nml` as *the*
  # hydrodynamic nml, so accept whichever the table uses and route it to
  # the file the configuration was read from -- keeps calibration tables
  # working across the GLM v3 -> v4 rename regardless of which literal they
  # carry.
  glm_nml_actual <- config[["hydrodynamic_file"]]
  if (is.null(glm_nml_actual)) glm_nml_actual <- "glm3.nml"

  # Canonicalise every glm<version>.nml row to that name, then drop
  # duplicates a combined library can carry (same key under glm3.nml and
  # glm4.nml). Values are already collapsed per (model, file, name, group).
  is_glm_hydro <- grepl("^glm[0-9]+\\.nml$", all_p$file)
  if (any(is_glm_hydro)) {
    all_p$file[is_glm_hydro] <- glm_nml_actual
    dup <- duplicated(all_p[, c("file", "name", "group")]) & is_glm_hydro
    if (any(dup)) all_p <- all_p[!dup, , drop = FALSE]
  }

  # Get/set a configuration component by its file label
  get_cfg <- function(label) {
    if (label == glm_nml_actual) return(config[["hydrodynamic"]])
    config[["bgc"]][[tools::file_path_sans_ext(label)]]
  }
  set_cfg <- function(label, value) {
    if (label == glm_nml_actual) {
      config[["hydrodynamic"]] <<- value
    } else {
      config[["bgc"]][[tools::file_path_sans_ext(label)]] <<- value
    }
  }
  require_cfg <- function(label) {
    x <- get_cfg(label)
    if (is.null(x)) {
      config_missing(strict, c(
        "Parameters supplied for {.file {label}} but it is not in the model configuration.",
        "i" = "Was the model built with the matching {.arg use_bgc}?"
      ))
    }
    x
  }

  nml_files <- c("aed2.nml", "aed2_phyto_pars.nml", "aed2_zoop_pars.nml",
                 glm_nml_actual, "aed.nml")
  csv_files <- c("aed_phyto_pars.csv", "aed_zoop_pars.csv",
                 "aed_macrophyte_pars.csv")

  unhandled <- setdiff(unique(all_p$file),
                       c(nml_files, csv_files, "met", "inf", "wdr"))
  if (length(unhandled) > 0) {
    cli::cli_warn("Parameters for {.file {unhandled}} are not applied to \\
                  {.val glm_aed}: file not supported.")
  }

  # nml files ----
  for (f in nml_files[nml_files %in% all_p$file]) {
    idx <- which(all_p$file == f)
    nml <- require_cfg(f)
    if (is.null(nml)) next

    if (f %in% c("aed2_phyto_pars.nml", "aed2_zoop_pars.nml")) {
      aed <- require_cfg("aed2.nml")
      if (is.null(aed)) next
      grp <- ifelse(f == "aed2_phyto_pars.nml", "the_phytos", "the_zoops")
      grp_idx <- get_nml_value(aed, grp)

      if (length(grp_idx) > 1) {
        wid <- all_p |>
          dplyr::slice(idx) |>
          dplyr::mutate(value = unlist(value)) |>
          dplyr::select(name, value, group) |>
          tidyr::pivot_wider(names_from = "group",
                             values_from = "value") |>
          as.data.frame()

        names <- get_nml_value(nml, "pd%p_name")

        grp_idx <- grep(paste0(substr(names(wid)[-1], 1, 4),
                               collapse = "|"), names)

        names(grp_idx) <- names(wid)[-1]
        arg_list <- lapply(1:nrow(wid), \(p) {
          par <- strsplit(wid$name[p], "/")[[1]][2]
          vals <- get_nml_value(nml, par)
          for (v in 2:ncol(wid)) {
            if (!is.na(wid[p, v])) {
              vals[grp_idx[v-1]] <- wid[p, v]
            }
          }
          vals
        })
        names(arg_list) <- sapply(1:nrow(wid), \(p) {
          gsub("/", "::", wid$name[p])
        })
      } else {
        arg_list <- lapply(idx, \(p) {
          par <- strsplit(all_p$name[p], "/")[[1]][2]
          vals <- get_nml_value(nml, par)
          vals[grp_idx] <- unlist(all_p$value[p])
          vals
        })
        names(arg_list) <- sapply(idx, \(p) {
          gsub("/", "::", all_p$name[p])
        })
      }
    } else {
      arg_list <- lapply(idx, \(p) unlist(all_p$value[p]))
      names(arg_list) <- sapply(idx, \(p) gsub("/", "::", all_p$name[p]))
      arg_list <- recycle_sed_fluxes(nml, arg_list)
      warn_missing_nml_keys(nml, names(arg_list), f)
    }

    set_cfg(f, set_nml(nml, arg_list = arg_list))
    touched <- c(touched, f)
  }

  # csv files ----
  for (f in csv_files[csv_files %in% all_p$file]) {
    df <- require_cfg(f)
    if (is.null(df)) next
    set_cfg(f, set_aed_csv_params(df, all_p[all_p$file == f, , drop = FALSE], f))
    touched <- c(touched, f)
  }

  # The aed_totals block is derived from the phytoplankton parameters, so
  # keep it consistent with whatever was just set. Only when the config
  # already has one (it is added by build_aeme()/set_aed_totals()).
  phyto_changed <- any(c("aed.nml", "aed_phyto_pars.csv") %in% touched)
  if (phyto_changed && !is.null(config[["bgc"]][["aed"]][["aed_totals"]])) {
    config <- derive_aed_totals(config)
    touched <- union(touched, "aed.nml")
  }

  list(config = config, touched = touched)
}

#' Apply boundary-condition scaling parameters
#'
#' Pure counterpart of the met, outflow and inflow scaling in
#' [input_model_parameters()]. Parameters with `file` of `"met"`, `"wdr"` or
#' `"inf"` are multiplicative factors on the boundary-condition data rather
#' than values in a model configuration file. This computes the scaled data;
#' writing it to a model's own file format is left to the caller.
#'
#' @param meteo data.frame; the meteorology, `input(aeme)$meteo`.
#' @param outflows list; the outflow data, `outflows(aeme)$data`.
#' @param param data.frame; collapsed parameters (`collapse_params()`) for a
#'   single model.
#'
#' @return A list. Each element is `NULL` when `param` has no rows for that
#'   boundary condition.
#'   \item{meteo}{scaled meteorology. `MET_wndspd` also scales `MET_wnduvu`
#'   and `MET_wnduvv`, and `MET_cldcvr` is clamped to `[0, 1]`.}
#'   \item{outflows}{scaled outflow data (`outflow` for the water balance,
#'   `HYD_flow` otherwise).}
#'   \item{inf_factor}{numeric; inflow scaling factor(s). The inflow data
#'   itself is not scaled here -- the model-specific inflow writers apply the
#'   factor.}
#' @noRd
apply_boundary_params <- function(meteo, outflows, param) {
  if (!is.list(param[["value"]])) param <- collapse_params(param)
  out <- list(meteo = NULL, outflows = NULL, inf_factor = NULL)

  met_idx <- which(param$file == "met")
  if (length(met_idx) > 0) {
    met <- meteo
    for (v in met_idx) {
      value <- unlist(param[["value"]][v])
      if (param$name[v] == "MET_wndspd") {
        met[["MET_wnduvu"]] <- met[["MET_wnduvu"]] * value
        met[["MET_wnduvv"]] <- met[["MET_wnduvv"]] * value
      }
      met[[param$name[v]]] <- met[[param$name[v]]] * value
      if (param$name[v] == "MET_cldcvr") {
        met[[param$name[v]]][met[[param$name[v]]] < 0] <- 0
        met[[param$name[v]]][met[[param$name[v]]] > 1] <- 1
      }
    }
    out$meteo <- met
  }

  wdr_idx <- which(param$file == "wdr")
  if (length(wdr_idx) > 0) {
    wdr <- outflows
    value <- unlist(param[["value"]][wdr_idx])
    for (c in names(wdr)) {
      flow_col <- ifelse(c == "wbal", "outflow", "HYD_flow")
      wdr[[c]][[flow_col]] <- wdr[[c]][[flow_col]] * value
    }
    out$outflows <- wdr
  }

  inf_idx <- which(param$file == "inf")
  if (length(inf_idx) > 0) {
    out$inf_factor <- unlist(param[["value"]][inf_idx])
  }

  out
}
