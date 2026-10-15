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
#' @return dataframe or list
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
  
  # Extract output from aeme ----
  inp <- input(aeme)
  bathy <- inp$hypsograph |>
    dplyr::filter(depth <= 0)
  bathy$depth <- max(bathy$elev) - bathy$elev
  max_dep <- max(bathy$depth)
  outp <- output(aeme)
  aeme_time <- time(aeme)
  date_index <- get_date_index(aeme = aeme, model = model,
                               remove_spin_up = remove_spin_up)
  names(model) <- model
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  if (use_obs) {
    obs <- observations(aeme)
    if (var_sim == "LKE_lvlwtr") {
      if (is.null(obs$level)) {
        cli_inform_safe(c("i" = "No lake level observations found. Using bathymetry
                        [depth = 0 m] as lake level."))
        obs_sub <- data.frame(Date = seq.Date(as.Date(aeme_time$start),
                                              as.Date(aeme_time$stop),
                                              by = 1),
                              var_aeme = var_sim,
                              value = max(bathy$elev))
        # stop("No observations of lake level found.")
      } else {
        obs_sub <- obs$level |>
          dplyr::filter(as.Date(Date, tz = "UTC") >= as.Date(aeme_time$start, tz = "UTC") &
                          as.Date(Date, tz = "UTC") <= as.Date(aeme_time$stop, tz = "UTC") &
                          var_aeme %in% var_sim) |>
          dplyr::arrange(Date)
      }
    } else {
      if (is.null(obs$lake)) stop("No lake observations found.")
      obs_sub <- obs$lake |>
        dplyr::filter(as.Date(Date, tz = "UTC") >= as.Date(aeme_time$start, tz = "UTC") &
                        as.Date(Date, tz = "UTC") <= as.Date(aeme_time$stop, tz = "UTC") &
                        var_aeme %in% var_sim) |>
        dplyr::arrange(Date, depth) |>
        dplyr::select(Date, var_aeme, depth, value,
                      dplyr::any_of(c("depth_ref", "height_above_bed")))
    }
    if (nrow(obs_sub) == 0) {
      cli::cli_abort("No observations found for the model simulation period.")
    }
    # Observations are daily. Reduce the join key to a calendar Date so it
    # matches the model axis (also reduced to Date below) regardless of whether
    # the stored obs Date is noon POSIXct or the model output is sub-daily.
    obs_sub <- obs_sub |>
      dplyr::rename(obs = value) |>
      dplyr::mutate(Date = as.Date(Date, tz = "UTC"))
  }
  
  # Loop through the models and extract the variable of interest ----
  lst <- lapply(model, \(m) {
    lapply(outp[[ens_lab]][[m]], dim)
    
    variable <- .get_output_var(outp[[ens_lab]][[m]], var_sim)
    # Empty dataframe to return if variable is not in output
    df <- data.frame(Date = as.Date(NA),
                     depth = NA_real_,
                     value = NA_real_, 
                     var_sim = var_sim,
                     lyr_top = NA,
                     value = NA,
                     Model = toggle_models(m, to = "display"),
                     lyr_thk = NA)
    
    if (var_sim == "z") {
      cli_inform_safe(c("i" = paste0(var_sim, " is a dimension for model ", m,
                                     ". Returning a dataframe with NA's.")))
      return(df)
    }
    if (is.null(variable)) {
      msg <- paste0(var_sim, " is not in output for model ", m,
                    ". Returning a dataframe with NA's.")
      cli_inform_safe(c("i" = msg))
      return(df)
    }
    if (inherits(variable, "aeme_grouped_var")) {
      # Variable has dimensions other than (time) or (z, time) -- see
      # new_grouped_var(); return it long-format rather than trying to
      # force it into the depth x time convention the rest of this
      # function assumes. use_obs/cumulative are not yet supported for
      # these variables.
      if (use_obs) {
        cli_inform_safe(c("i" = paste0(var_sim, " has non-standard dimensions (",
                                       paste(variable$dim_names, collapse = ", "),
                                       "); use_obs comparison is not supported for it.")))
      }
      gdf <- as.data.frame(variable)
      gdf <- .filter_grouped_df(gdf, zone = zone, sed_layer = sed_layer)
      gdf$var_sim <- var_sim
      gdf$Model <- toggle_models(m, to = "display")
      if (remove_spin_up && "Date" %in% names(gdf)) {
        gdf <- gdf[gdf$Date >= aeme_time$start & gdf$Date <= aeme_time$stop, ]
      }
      return(gdf)
    }
    if (is.matrix(variable)) {
      if (ncol(variable) == 0) {
        msg <- paste0(var_sim, " is not in output for model ", m,
                      ". Returning a dataframe with NA's.")
        cli_inform_safe(c("i" = msg))
        return(df)
      }
    }
    if (length(variable) == 0) {
      msg <- paste0(var_sim, " is not in output for model ", m,
                    ". Returning a dataframe with NA's.")
      cli_inform_safe(c("i" = msg))
      return(df)
    }
    
          # Build long dataframe for 2D variable
      # each <- nrow(outp[[ens_lab]][[m]][[var_sim]])
      # data.frame(
      #   var_sim = var_sim,
      #   Date     = rep(outp[[ens_lab]][[m]][["Date"]], each = each),
      #   depth    = as.vector(outp[[ens_lab]][[m]][["LKE_depths"]]),
      #   value    = as.vector(outp[[ens_lab]][[m]][[var_sim]]),
      #   stringsAsFactors = FALSE
      # )
    
    if (use_obs) {

      # Observations are daily. When model output is sub-daily POSIXct, snap
      # the model axis to calendar days -- but keep only ONE model step per
      # obs day (the timestamp nearest midnight, matching the historical
      # daily convention), otherwise every obs would join to all 24 hourly
      # steps of its day and blow the result up 24x.
      mod_raw <- outp[[ens_lab]][[m]][["Date"]]
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

        df <- data.frame(Date = mod_date[date_index],
                         sim = outp[[ens_lab]][[m]][["LKE_lvlwtr"]][date_index] +
                           min(inp$hypsograph$elev),
                         Model = toggle_models(m, to = "display")) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date"))
      } else if (is.vector(variable)) {
        df <- data.frame(Date = mod_date[date_index],
                         sim = outp[[ens_lab]][[m]][[var_sim]][date_index],
                         Model = toggle_models(m, to = "display")) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date")) |>
          dplyr::mutate(
            sim = dplyr::case_when(
              var_sim == "HYD_thmcln" & is.na(sim) ~ max_dep,
              .default = sim
            )
          )
      } else {
        
        # Observations recorded as a height above the lake bed (see add_obs())
        # are compared with the model at (model level - height) on each date;
        # the rest by depth below the surface.
        is_bottom <- if ("depth_ref" %in% names(obs_sub)) {
          obs_sub$depth_ref %in% "bottom"
        } else {
          rep(FALSE, nrow(obs_sub))
        }
        obs_bot <- obs_sub[is_bottom, , drop = FALSE]
        obs_sub <- obs_sub[!is_bottom, , drop = FALSE]

        depth <- obs_sub |>
          dplyr::distinct(depth) |>
          dplyr::arrange(depth) |>
          dplyr::pull(depth)
        if (length(depth) > 0) {
          out_depths <- matrix(depth, nrow = length(depth),
                               ncol = length(date_index))
          depths <- outp[[ens_lab]][[m]][["LKE_depths"]][, date_index,
                                                           drop = FALSE]

          value <- interp_static_grid(var = variable[, date_index, drop = FALSE],
                                      midpoints = depths,
                                      out_depths = out_depths)
          # Build long dataframe for 2D variable
          each <- length(depth)
          mod <- data.frame(
            Date     = rep(mod_date[date_index], each = each),
            depth    = as.vector(out_depths),
            sim    = as.vector(value),
            Model = toggle_models(m, to = "display"),
            stringsAsFactors = FALSE
          )
        } else {
          mod <- data.frame(Date = mod_date[0], depth = numeric(0),
                            sim = numeric(0), Model = character(0),
                            stringsAsFactors = FALSE)
        }
        
        
        # df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
        #                  depth = depth,
        #                  value = value,
        #                  Model = toggle_models(m, to = "display")
        # )
        # 
        # mod <- lapply(date_index, \(d) {
        #   depth <- outp[[ens_lab]][[m]][["LKE_depths"]][, d]
        #   v <- outp[[ens_lab]][[m]][[var_sim]][, d]
        #   obs_deps <- unique(obs_sub$depth_mid[obs_sub$Date == outp[[ens_lab]][[m]][["Date"]][d]])
        #   
        #   if (all(is.na(v)) | all(is.na(depth))) {
        #     return(data.frame(Date = outp[[ens_lab]][[m]][["Date"]][d],
        #                       depth_mid = obs_deps,
        #                       sim = NA,
        #                       Model = toggle_models(m, to = "display")))
        #   }
        #   # p <- approx(depth, v, obs_deps, rule = 2)$y
        #   data.frame(Date = outp[[ens_lab]][[m]][["Date"]][d],
        #              depth_mid = obs_deps,
        #              sim = p,
        #              Model = toggle_models(m, to = "display"))
        # }) |>
        #   dplyr::bind_rows()
        
        df <- dplyr::left_join(obs_sub, mod, by = c("Date", "depth"))

        if (nrow(obs_bot) > 0) {
          level_d <- outp[[ens_lab]][[m]][["LKE_lvlwtr"]][date_index]
          var_d <- variable[, date_index, drop = FALSE]
          mid_d <- outp[[ens_lab]][[m]][["LKE_depths"]][, date_index, drop = FALSE]
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
          obs_bot$Model <- toggle_models(m, to = "display")
          df <- dplyr::bind_rows(df, obs_bot) |>
            dplyr::arrange(Date, depth)
        }

      }
    } else if (is.null(dim(variable))) {
      df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                       depth = NA_real_,
                       value = variable,
                       Model = toggle_models(m, to = "display")
                       )
      # Trim off the spin up period ----
      if (remove_spin_up) {
        idx2 <- which(df$Date >= aeme_time$start & df$Date <= aeme_time$stop)
        df <- df[idx2, ]
      }
      if (cumulative) {
        df <- df |>
          dplyr::mutate(value = cumsum(value))
      }
    } else {
      
      lake_level <- outp[[ens_lab]][[m]][["LKE_lvlwtr"]]
      depths <- outp[[ens_lab]][[m]][["LKE_depths"]]
      # dep <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
      #                   depth = outp[[ens_lab]][[m]][["LKE_lvlwtr"]])
      # lyr <- outp[[ens_lab]][[m]][["LKE_layers"]]
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
          # stop(strwrap(paste0("Depth is outside the range of the modelled lake
          #                     levels [", min_depth, ", ", max_depth, "m]")))
        }
        # Depth below the surface to read the profile at, on each date.
        # "bottom" is a height above the lake bed, which is (level - height)
        # below the surface, and the level changes through the run -- so it is
        # taken from the modelled level on each date rather than from a fixed
        # basin depth. A height above the surface (more than the level) has no
        # value.
        out_depths <- if (depth_ref == "surface") {
          matrix(depth, nrow = 1, ncol = ncol(variable))
        } else {
          below_surface <- lake_level - depth
          below_surface[!is.na(below_surface) & below_surface < 0] <- NA_real_
          matrix(below_surface, nrow = 1, ncol = ncol(variable))
        }
        # out_depths <- matrix(depth, nrow = 1, ncol = ncol(variable))
        value <- interp_static_grid(var = variable, midpoints = depths,
                                    out_depths = out_depths)
        df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                         depth = depth,
                         value = value,
                         Model = toggle_models(m, to = "display")
                         )
      } else {
        # Build long dataframe for 2D variable
        each <- nrow(variable)
        df <- data.frame(
          Date     = rep(outp[[ens_lab]][[m]][["Date"]], each = each),
          depth    = as.vector(outp[[ens_lab]][[m]][["LKE_depths"]]),
          value    = as.vector(outp[[ens_lab]][[m]][[var_sim]]),
          Model = toggle_models(m, to = "display"),
          stringsAsFactors = FALSE
        )
      }
      
      
      # Trim off the spin up period ----
      if (remove_spin_up) {
        idx2 <- which(df$Date >= aeme_time$start & df$Date <= aeme_time$stop)
        df <- df[idx2, ]
      }
      
      if (cumulative) {
        warning("Applying cumulative to a value with a depth component.")
        df <- df |>
          dplyr::mutate(value = cumsum(value))
      }
    }
    df
  })
  
  if (return_df) {
    df <- lst |> 
      dplyr::bind_rows() |>
      dplyr::mutate(Model = toggle_models(Model, to = "display"),
                    var_sim = var_sim
      ) |>
      dplyr::filter(!is.na(Date))
    return(df)
  } else {
    return(lst)
  }
}
