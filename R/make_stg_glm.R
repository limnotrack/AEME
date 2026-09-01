#' Write a lake bathymetry to a GLM simulation
#'
#' @inheritParams set_nml
#' @param lakename string; name of lake
#' @param bathy data.frame; containing hypsograph
#' @param lat numeric; latitude of lake
#' @param lon numeric; longitude of lake
#' @param crest numeric; height of crest of lake
#' @param dims_lake numeric vector of length 2; containing basin length and
#' width.
#' @param update_sediment Logical; update the sediment block in the nml object?
#' @param use_bgc Logical; is the biogeochemistry (AED) library active? When
#'   `FALSE`, `sed_heat_model` is forced to 1 because GLM's dynamic
#'   soil-temperature solver (`sed_heat_model = 2`) requires an active WQ
#'   module and aborts without one.
#' @param obs_temp data.frame or `NULL`; observed water-column temperature
#'   profiles in the long AEME format. When supplied, per-zone
#'   sediment-temperature parameters are derived from it via `calc_sed_temp()`.
#' @param nml_file character; name of the GLM nml file, forwarded to
#'   `calc_sed_temp()` for the `file` column of its AEME parameter table.
#' @param sed_params data.frame or `NULL`; rows of `parameters(aeme)` for the
#'   GLM `&sediment` block (i.e. `model == "glm_aed"`, `name` like
#'   `"sediment/..."`). Any key found here is used verbatim (per `index` for
#'   per-zone keys) instead of being estimated; `sediment/zone_heights` or
#'   `sediment/n_zones` also drive the zone count.
#'
#' @return updated nml object
#' @noRd

make_stg_glm <- function(glm_nml, lakename, bathy, lat, lon, dims_lake, crest,
                        update_sediment = TRUE, use_bgc = TRUE,
                        obs_temp = NULL, nml_file = "glm4.nml",
                        sed_params = NULL) {

  bathy_glm <- bathy |>
    dplyr::arrange(elev)

  # find the area at the surface height
  len <- dims_lake[1]
  wid <- dims_lake[2]

  max_depth <- max(bathy_glm$elev) - min(bathy_glm$elev)
  sub_layers <- get_model_layers(depth = max_depth)
  min_layer_thick <- min(sub_layers$h)
  max_layer_thick <- max(sub_layers$h)
  max_layers <- ceiling(max_depth / min_layer_thick) + 10

  arg_list <- list(max_layers = max_layers,
                   min_layer_vol = 0.025,
                   min_layer_thick = min_layer_thick,
                   max_layer_thick = max_layer_thick,
                   crest_elev = crest,
                   density_model = 1, non_avg = TRUE,
                   lake_name = lakename, latitude = lat,
                   longitude = lon,
                   base_elev = min(bathy_glm$elev),
                   bsn_len = len, bsn_wid = wid, bsn_vals = nrow(bathy_glm),
                   H = bathy_glm$elev, A = bathy_glm$area)

  glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)

  if (update_sediment) {

    # Pull one &sediment key out of the supplied model-parameter rows: a
    # per-zone numeric vector ordered by `index`, a scalar when unindexed, or
    # NULL when the caller did not provide that key. The same key can appear
    # under more than one `file` (e.g. a combined glm3.nml + glm4.nml
    # parameter library), so collapse to one value per zone index.
    sp_val <- function(key, scalar = FALSE) {
      if (is.null(sed_params) || !nrow(sed_params)) return(NULL)
      rows <- sed_params[!is.na(sed_params$name) &
                           sed_params$name == paste0("sediment/", key), ,
                         drop = FALSE]
      if (!nrow(rows)) return(NULL)
      if (scalar) return(as.numeric(rows$value[[1]]))
      idx_rows <- rows[!is.na(rows$index), , drop = FALSE]
      if (!nrow(idx_rows)) return(as.numeric(rows$value[[1]]))
      idx_rows <- idx_rows[!duplicated(idx_rows$index), , drop = FALSE]
      as.numeric(idx_rows$value[order(idx_rows$index)])
    }

    # 1. Zone geometry -- parameters(aeme) override the bathymetry estimate.
    zh_param <- sp_val("zone_heights")
    nz_param <- sp_val("n_zones", scalar = TRUE)
    if (!is.null(nz_param)) nz_param <- as.integer(round(nz_param))
    if (!is.null(zh_param)) {
      sed_zones <- zh_param
      if (!is.null(nz_param) && nz_param != length(sed_zones))
        cli_inform_safe(c("!" = "sediment/n_zones ({nz_param}) \\
                          disagrees with sediment/zone_heights \\
                          (length {length(sed_zones)}); using zone_heights."))
    } else if (!is.null(nz_param)) {
      sed_zones <- estimate_sed_zones(hypsograph = bathy, n_zones = nz_param)
    } else {
      sed_zones <- estimate_sed_zones(hypsograph = bathy)
    }
    n_zones <- length(sed_zones)

    # 2. Sediment-temperature cycle (zone 1 = deepest). Keep any of the three
    # keys already supplied via parameters(aeme); estimate only the missing
    # ones from observed profiles (calc_sed_temp()), else fall back to
    # generic defaults.
    st_keys  <- c("sed_temp_mean", "sed_temp_amplitude", "sed_temp_peak_doy")
    st_param <- stats::setNames(lapply(st_keys, sp_val), st_keys)
    st_val   <- list(sed_temp_mean = rep(10, n_zones),
                     sed_temp_amplitude = rep(4, n_zones),
                     sed_temp_peak_doy = rep(10L, n_zones))
    if (any(vapply(st_param, is.null, logical(1))) &&
        !is.null(obs_temp) && nrow(obs_temp) > 0) {
      est <- tryCatch(
        calc_sed_temp(obs_temp = obs_temp, sed_zones = sed_zones,
                      max_depth = max_depth, hypsograph = bathy,
                      nml_file = nml_file, output = "nml", verbose = FALSE),
        error = function(e) {
          cli_inform_safe(c(
            "!" = "Could not estimate sediment temperatures from observations \\
                   ({conditionMessage(e)}); using defaults."
          ))
          NULL
        }
      )
      if (!is.null(est)) st_val[st_keys] <- est[st_keys]
    }
    for (k in st_keys) {
      if (!is.null(st_param[[k]])) st_val[[k]] <- st_param[[k]]
      st_val[[k]] <- rep_len(st_val[[k]], n_zones)
    }

    # 3. Remaining per-zone keys: parameter value if supplied, else default.
    zone_default <- c(sed_heat_Ksoil = 1.2, sed_temp_depth = 0.2,
                      sed_reflectivity = 0.1, sed_roughness = 0.1)
    resolve_zone <- function(k) {
      v <- sp_val(k)
      if (is.null(v)) v <- zone_default[[k]]
      rep_len(v, n_zones)
    }
    benthic_mode <- sp_val("benthic_mode", scalar = TRUE)
    if (is.null(benthic_mode)) benthic_mode <- 2

    managed <- list(
      sed_heat_Ksoil = resolve_zone("sed_heat_Ksoil"),
      sed_temp_depth = resolve_zone("sed_temp_depth"),
      sed_temp_mean = st_val$sed_temp_mean,
      sed_temp_amplitude = st_val$sed_temp_amplitude,
      sed_temp_peak_doy = st_val$sed_temp_peak_doy,
      benthic_mode = benthic_mode,
      n_zones = n_zones,
      zone_heights = sed_zones,
      sed_reflectivity = resolve_zone("sed_reflectivity"),
      sed_roughness = resolve_zone("sed_roughness")
    )

    # Merge rather than replace: keep any other keys already in the &sediment
    # block. In particular this preserves the expanded GLMv4 soil-column heat
    # model settings (sed_heat_model, n_sed_layers, sed_layer_depth, sed_vwc,
    # sed_spinup_days, sed_deep_temp, ...) that a glm4.nml template carries.
    sediment <- glm_nml[["sediment"]]
    if (is.null(sediment)) sediment <- list()

    # sed_heat_model = 2 (dynamic soil-column solver, zZSoilTemp) is provided
    # by the WQ library, so GLM aborts with it enabled when no WQ module is
    # active. Fall back to the analytical model (1) when biogeochemistry is
    # off. A glm4.nml template ships with sed_heat_model = 2.
    heat_model <- suppressWarnings(as.numeric(sediment[["sed_heat_model"]]))
    if (!isTRUE(use_bgc) && length(heat_model) == 1 && !is.na(heat_model) &&
        heat_model != 1) {
      cli_inform_safe(c(
        "!" = "Forcing {.field sed_heat_model} from {heat_model} to 1: \\
               {.field sed_heat_model = 2} needs an active WQ module and \\
               {.arg use_bgc} is {.val {FALSE}}."
      ))
      sediment[["sed_heat_model"]] <- 1
      heat_model <- 1
    }

    # Under the dynamic soil-column heat model (sed_heat_model = 2) GLM reads
    # sed_heat_Ksoil / sed_temp_depth as scalars, not per-zone vectors, so
    # leave whatever the template set rather than expanding them (mirrors the
    # length check in check_glm_nml()).
    if (length(heat_model) == 1 && !is.na(heat_model) && heat_model != 1) {
      managed[["sed_heat_Ksoil"]] <- NULL
      managed[["sed_temp_depth"]] <- NULL
    }

    sediment[names(managed)] <- managed
    glm_nml[["sediment"]] <- sediment
  }

  return(glm_nml)
}
