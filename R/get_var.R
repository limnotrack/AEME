#' Get variable from aeme
#'
#' @inheritParams build_aeme
#' @param aeme Aeme object, or an [AemeSummary][AemeSummary-class]. For a
#'   summary only `model` and `var_sim` apply, and the result has the surface and
#'   bottom series (`layer`) and every `stat`; filter [output()] for more
#'   control.
#' @inheritParams plot_output
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp").
#' @param use_obs logical; if TRUE, use observations to extract the variable at
#' time and depth of observations. Default is FALSE. Use this option if you
#' want to compare model output to observations. Observations recorded as a
#' height above the lake bed (see [add_obs()]) are compared with the model
#' profile at (the model's water level - that height) on each date, so they
#' follow the model's own level rather than the depth they were placed at when
#' added.
#' @param depth_ref character; what `depth` is measured from when a single
#'  depth is extracted. `"surface"` (default): metres below the water surface.
#'  `"bottom"`: metres above the lake bed, so `depth = 1` is the water 1 m above
#'  the bottom. That is `level - depth` metres below the surface, with `level`
#'  the modelled water level (`LKE_lvlwtr`) on each date, so it follows the
#'  surface as the level rises and falls. On a date when the level is below
#'  `depth` (the point would be above the surface) the value is `NA`. Ignored
#'  when `depth` is `NULL`. (In AEME 0.4.0 and earlier `"bottom"` was measured from the
#'  basin's full depth, which only matches when the level equals that depth.)
#' @param return_df logical; if TRUE, return a dataframe; if FALSE, return a
#' list. Default is TRUE.
#' @param depth numeric; depth of the variable to extract, referenced to
#' `depth_ref`. Default is NULL. If NULL, the variable profiles are extracted.
#' @param cumulative logical; if TRUE, return cumulative sum of variable
#' @param zone integer; for GLM-AED sediment-zone variables (those with an
#' `nzones` column, e.g. `"SED_Fsed_oxy"`, `"SED_temp"`), keep only these
#' sediment zones (GLM zone order, 1 = deepest). `NULL` (default) keeps all.
#' @param sed_layer integer; for variables with a sediment-layer dimension
#' (`"SED_temp"`), keep only these layers (1 = shallowest). `NULL` (default)
#' keeps all. Sediment variables are returned in long format with one column
#' per dimension (`nzones`, `sed_layers`); `depth_ref`/`depth` do not apply.
#'
#' @importFrom dplyr arrange filter left_join mutate select bind_rows case_when
#' @importFrom dplyr rename
#'
#' @return If `return_df = TRUE`, a data frame with the same leading columns
#'   whether or not `use_obs` is used: `Date`, `depth` (`NA` for variables
#'   without a depth dimension), `value`, `var_sim` and `Model`. With
#'   `use_obs = TRUE` the column `obs` (the observation, paired with the model
#'   `value`) is added after `value`, plus `depth_ref` and
#'   `height_above_bed` when height-above-bed observations are present. Variables
#'   with non-standard dimensions (e.g. sediment zones) carry one column per
#'   extra dimension instead of `depth`. If `return_df = FALSE`, a list with one
#'   such data frame per model.
#' @export

get_var <- function(aeme, model, var_sim, depth = NULL,
                    depth_ref = c("surface", "bottom"), return_df = TRUE,
                    ens_n = 1, use_obs = FALSE, remove_spin_up = TRUE,
                    cumulative = FALSE, zone = NULL, sed_layer = NULL) {

  if (is_aeme_summary(aeme)) {
    return(.get_var_summary_obj(aeme, model = if (missing(model)) NULL else model,
                                var_sim = var_sim))
  }
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  var_sim <- check_aeme_vars(var_sim, aeme = aeme)

  # Deprecation shim: LKE_V was renamed to LKE_vol.
  if (identical(var_sim, "LKE_V")) {
    cli::cli_warn(
      c("!" = "{.val LKE_V} has been renamed to {.val LKE_vol}.",
        "i" = "Please update your code to use {.code var_sim = \"LKE_vol\"}."),
      class = "aeme_warn_lke_v_renamed"
    )
    var_sim <- "LKE_vol"
  }
  depth_ref <- rlang::arg_match(depth_ref)

  inp <- input(aeme)
  bathy <- inp$hypsograph |>
    dplyr::filter(depth <= 0)
  bathy$depth <- max(bathy$elev) - bathy$elev
  max_dep <- max(bathy$depth)
  outp <- output(aeme)
  aeme_time <- time(aeme)
  names(model) <- model
  ens_lab <- format_ens_label(ens_n = ens_n)

  obs_sub <- NULL
  if (use_obs) {
    obs_sub <- .get_var_obs(aeme, var_sim = var_sim, bathy = bathy)
  }

  lst <- lapply(model, \(m) {
    out_m <- outp[[ens_lab]][[m]]
    m_lab <- toggle_models(m, to = "display")
    variable <- .get_output_var(out_m, var_sim)

    if (inherits(variable, "aeme_grouped_var")) {
      # Variable has dimensions other than (time) or (z, time) -- see
      # new_grouped_var(); return it long-format rather than forcing it into
      # the depth x time convention. use_obs/cumulative are not supported.
      if (use_obs) {
        cli_inform_safe(c("i" = paste0(var_sim, " has non-standard dimensions (",
                                       paste(variable$dim_names, collapse = ", "),
                                       "); use_obs comparison is not supported for it.")))
      }
      gdf <- as.data.frame(variable)
      gdf <- .filter_grouped_df(gdf, zone = zone, sed_layer = sed_layer)
      gdf$var_sim <- var_sim
      gdf$Model <- m_lab
      if (remove_spin_up && "Date" %in% names(gdf)) {
        gdf <- gdf[gdf$Date >= aeme_time$start & gdf$Date <= aeme_time$stop, ]
      }
      return(gdf)
    }

    df <- NULL
    if (var_sim == "z") {
      cli_inform_safe(c("i" = paste0(var_sim, " is a dimension for model ", m,
                                     ". Returning a dataframe with NA's.")))
    } else if (.var_is_empty(variable)) {
      cli_inform_safe(c("i" = paste0(var_sim, " is not in output for model ", m,
                                     ". Returning a dataframe with NA's.")))
    } else if (use_obs) {
      df <- .get_var_vs_obs(out_m, variable = variable, var_sim = var_sim,
                            obs_sub = obs_sub, hypsograph = inp$hypsograph,
                            max_dep = max_dep)
    } else {
      df <- .get_var_sim(out_m, variable = variable, depth = depth,
                         depth_ref = depth_ref, cumulative = cumulative,
                         remove_spin_up = remove_spin_up, aeme_time = aeme_time)
    }
    if (is.null(df)) {
      # Placeholder row when the variable is not available for this model
      df <- data.frame(Date = as.Date(NA), depth = NA_real_, value = NA_real_)
    }
    .standardise_var_df(df, var_sim = var_sim, model_label = m_lab,
                        use_obs = use_obs)
  })

  if (return_df) {
    return(lst |>
             dplyr::bind_rows() |>
             dplyr::filter(!is.na(Date)))
  }
  lst
}

#' TRUE if a variable pulled from model output has nothing in it
#' @noRd
.var_is_empty <- function(variable) {
  is.null(variable) || length(variable) == 0 ||
    (is.matrix(variable) && ncol(variable) == 0)
}

#' Give every get_var() data frame the same leading columns
#'
#' `Date`, `depth`, `value` (the model value), optionally `obs`, then `var_sim`,
#' `Model`, followed by any other columns (`depth_ref`, `height_above_bed`).
#' @noRd
.standardise_var_df <- function(df, var_sim, model_label, use_obs) {
  if (use_obs) {
    # model value goes in `value`, as without observations
    if ("sim" %in% names(df)) df$value <- df$sim
    df$sim <- NULL
    if (!"obs" %in% names(df)) df$obs <- NA_real_
  }
  if (!"depth" %in% names(df)) df$depth <- NA_real_
  if (!"value" %in% names(df)) df$value <- NA_real_
  df$var_sim <- var_sim
  df$Model <- model_label
  lead <- c("Date", "depth", "value", if (use_obs) "obs",
            "var_sim", "Model")
  df[, c(lead, setdiff(names(df), lead)), drop = FALSE]
}

#' Observations of `var_sim` within the simulation period
#'
#' Columns: `Date` (calendar day), `depth` (not for lake level), `obs`, and
#' `depth_ref`/`height_above_bed` when present.
#' @noRd
.get_var_obs <- function(aeme, var_sim, bathy) {
  obs <- observations(aeme)
  aeme_time <- time(aeme)
  start <- as.Date(aeme_time$start, tz = "UTC")
  stop <- as.Date(aeme_time$stop, tz = "UTC")

  if (var_sim == "LKE_lvlwtr") {
    if (is.null(obs$level)) {
      cli_inform_safe(c("i" = "No lake level observations found. Using bathymetry
                        [depth = 0 m] as lake level."))
      obs_sub <- data.frame(Date = seq.Date(as.Date(aeme_time$start),
                                            as.Date(aeme_time$stop), by = 1),
                            value = max(bathy$elev))
    } else {
      obs_sub <- obs$level |>
        dplyr::filter(as.Date(Date, tz = "UTC") >= start &
                        as.Date(Date, tz = "UTC") <= stop &
                        var_aeme %in% var_sim) |>
        dplyr::arrange(Date) |>
        dplyr::select(Date, dplyr::any_of(c("depth", "value")))
    }
  } else {
    if (is.null(obs$lake)) cli::cli_abort("No lake observations found.")
    obs_sub <- obs$lake |>
      dplyr::filter(as.Date(Date, tz = "UTC") >= start &
                      as.Date(Date, tz = "UTC") <= stop &
                      var_aeme %in% var_sim) |>
      dplyr::arrange(Date, depth) |>
      dplyr::select(Date, depth, value,
                    dplyr::any_of(c("depth_ref", "height_above_bed")))
  }
  if (nrow(obs_sub) == 0) {
    cli::cli_abort("No observations found for the model simulation period.")
  }
  # Observations are daily. Reduce the join key to a calendar Date so it
  # matches the model axis (also reduced to Date) regardless of whether the
  # stored obs Date is noon POSIXct or the model output is sub-daily.
  obs_sub |>
    dplyr::rename(obs = value) |>
    dplyr::mutate(Date = as.Date(Date, tz = "UTC"))
}

#' Model values at the dates (and depths) of the observations
#' @noRd
.get_var_vs_obs <- function(out_m, variable, var_sim, obs_sub, hypsograph,
                            max_dep) {
  # When model output is sub-daily POSIXct, snap the model axis to calendar
  # days -- but keep only ONE model step per obs day (the timestamp nearest
  # midnight), otherwise every obs would join to all 24 hourly steps of its day.
  mod_raw <- out_m[["Date"]]
  obs_dates <- unique(obs_sub$Date)
  if (inherits(mod_raw, "POSIXct")) {
    mod_date <- as.Date(mod_raw)
    cand <- which(mod_date %in% obs_dates)
    if (length(cand)) {
      tod <- as.numeric(mod_raw[cand]) %% 86400
      near_mid <- pmin(tod, 86400 - tod)
      date_index <- vapply(split(seq_along(cand), mod_date[cand]),
                           \(ii) cand[ii[which.min(near_mid[ii])]],
                           integer(1))
      date_index <- sort(unname(date_index))
    } else {
      date_index <- integer(0)
    }
  } else {
    mod_date <- mod_raw
    date_index <- which(mod_date %in% obs_dates)
  }

  if (var_sim == "LKE_lvlwtr") {
    sim <- out_m[["LKE_lvlwtr"]][date_index] + min(hypsograph$elev)
    return(data.frame(Date = mod_date[date_index], sim = sim) |>
             dplyr::left_join(obs_sub, by = "Date"))
  }
  if (is.vector(variable)) {
    df <- data.frame(Date = mod_date[date_index],
                     sim = variable[date_index]) |>
      dplyr::left_join(obs_sub, by = "Date")
    if (var_sim == "HYD_thmcln") df$sim[is.na(df$sim)] <- max_dep
    return(df)
  }

  # Depth-resolved variable. Observations recorded as a height above the lake
  # bed (see add_obs()) are compared with the model at (model level - height)
  # on each date; the rest by depth below the surface.
  is_bottom <- if ("depth_ref" %in% names(obs_sub)) {
    obs_sub$depth_ref %in% "bottom"
  } else {
    rep(FALSE, nrow(obs_sub))
  }
  obs_bot <- obs_sub[is_bottom, , drop = FALSE]
  obs_sfc <- obs_sub[!is_bottom, , drop = FALSE]

  obs_depths <- sort(unique(obs_sfc$depth))
  if (length(obs_depths) > 0) {
    out_depths <- matrix(obs_depths, nrow = length(obs_depths),
                         ncol = length(date_index))
    mids <- out_m[["LKE_depths"]][, date_index, drop = FALSE]
    value <- interp_static_grid(var = variable[, date_index, drop = FALSE],
                                midpoints = mids, out_depths = out_depths)
    mod <- data.frame(Date = rep(mod_date[date_index], each = length(obs_depths)),
                      depth = as.vector(out_depths),
                      sim = as.vector(value))
  } else {
    mod <- data.frame(Date = mod_date[0], depth = numeric(0), sim = numeric(0))
  }
  df <- dplyr::left_join(obs_sfc, mod, by = c("Date", "depth"))

  if (nrow(obs_bot) > 0) {
    level_d <- out_m[["LKE_lvlwtr"]][date_index]
    var_d <- variable[, date_index, drop = FALSE]
    mid_d <- out_m[["LKE_depths"]][, date_index, drop = FALSE]
    col <- match(obs_bot$Date, mod_date[date_index])
    # depth below the surface of each observation, in this member
    target <- level_d[col] - obs_bot$height_above_bed
    obs_bot$sim <- vapply(seq_len(nrow(obs_bot)), function(r) {
      j <- col[r]
      if (is.na(j) || is.na(target[r]) || target[r] < 0 ||
          target[r] > level_d[j]) {
        return(NA_real_)
      }
      as.numeric(interp_static_grid(
        var = var_d[, j, drop = FALSE], midpoints = mid_d[, j, drop = FALSE],
        out_depths = matrix(target[r], nrow = 1, ncol = 1)))
    }, numeric(1))
    df <- dplyr::bind_rows(df, obs_bot) |>
      dplyr::arrange(Date, depth)
  }
  df
}

#' Model values on the model's own time axis (and depths)
#' @noRd
.get_var_sim <- function(out_m, variable, depth, depth_ref, cumulative,
                         remove_spin_up, aeme_time) {
  dates <- out_m[["Date"]]

  if (is.null(dim(variable))) {
    df <- data.frame(Date = dates, depth = NA_real_, value = variable)
  } else {
    lake_level <- out_m[["LKE_lvlwtr"]]
    depths <- out_m[["LKE_depths"]]
    if (!is.null(depth)) {
      min_depth <- 0
      max_depth <- round(max(lake_level, na.rm = TRUE), 2)
      if (!is.finite(max_depth)) {
        cli::cli_abort(
          "No finite modelled lake levels ({.field LKE_lvlwtr}) to place {.arg depth} against.",
          class = "aeme_error_depth_out_of_range"
        )
      }
      if (depth > max_depth | depth < min_depth) {
        cli::cli_abort("Depth is outside the range of the modelled lake levels [{min_depth}, {max_depth} m].",
                       class = "aeme_error_depth_out_of_range")
      }
      # Depth below the surface to read the profile at, on each date. "bottom"
      # is a height above the lake bed, which is (level - height) below the
      # surface, and the level changes through the run -- so it is taken from
      # the modelled level on each date. A height above the surface (more than
      # the level) has no value.
      out_depths <- if (depth_ref == "surface") {
        matrix(depth, nrow = 1, ncol = ncol(variable))
      } else {
        below_surface <- lake_level - depth
        below_surface[!is.na(below_surface) & below_surface < 0] <- NA_real_
        matrix(below_surface, nrow = 1, ncol = ncol(variable))
      }
      value <- interp_static_grid(var = variable, midpoints = depths,
                                  out_depths = out_depths)
      df <- data.frame(Date = dates, depth = depth, value = value)
    } else {
      # Long dataframe for the 2D variable
      df <- data.frame(Date = rep(dates, each = nrow(variable)),
                       depth = as.vector(depths),
                       value = as.vector(variable),
                       stringsAsFactors = FALSE)
    }
  }

  # Trim off the spin up period
  if (remove_spin_up) {
    df <- df[which(df$Date >= aeme_time$start & df$Date <= aeme_time$stop), ]
  }
  if (cumulative) {
    if (!is.null(dim(variable))) {
      warning("Applying cumulative to a value with a depth component.")
    }
    df <- dplyr::mutate(df, value = cumsum(value))
  }
  df
}
