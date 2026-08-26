#' Get variable from aeme
#'
#' @inheritParams build_aeme
#' @inheritParams plot_output
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp").
#' @param use_obs logical; if TRUE, use observations to extract the variable at
#' time and depth of observations. Default is FALSE. Use this option if you
#' want to compare model output to observations.
#' @param depth_ref character; reference depth for extracting variable profiles.
#'  Options are "surface" (default) or "bottom".
#' @param return_df logical; if TRUE, return a dataframe; if FALSE, return a
#' list. Default is TRUE.
#' @param depth numeric; depth of the variable to extract. Default is NULL. If
#' NULL, the variable profiles are extracted.
#' @param cumulative logical; if TRUE, return cumulative sum of variable
#'
#' @importFrom dplyr arrange filter left_join mutate select bind_rows case_when
#' @importFrom dplyr rename
#'
#' @return dataframe or list
#' @export

get_var <- function(aeme, model, var_sim, depth = NULL, 
                    depth_ref = c("surface", "bottom"), return_df = TRUE,
                    ens_n = 1, use_obs = FALSE, remove_spin_up = TRUE,
                    cumulative = FALSE) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  var_sim <- check_aeme_vars(var_sim, aeme = aeme)
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
          dplyr::filter(Date >= aeme_time$start & Date <= aeme_time$stop &
                          var_aeme %in% var_sim) |>
          dplyr::arrange(Date)
      }
    } else {
      if (is.null(obs$lake)) stop("No lake observations found.")
      obs_sub <- obs$lake |>
        dplyr::filter(Date >= aeme_time$start & Date <= aeme_time$stop &
                        var_aeme %in% var_sim) |>
        dplyr::mutate(depth = (depth_from + depth_to) / 2) |>
        dplyr::arrange(Date, depth) |>
        dplyr::select(Date, var_aeme, depth, value)
    }
    if (nrow(obs_sub) == 0) {
      cli::cli_abort("No observations found for the model simulation period.")
    }
    obs_sub <- obs_sub |>
      dplyr::rename(obs = value)
  }
  
  # Loop through the models and extract the variable of interest ----
  lst <- lapply(model, \(m) {
    lapply(outp[[ens_lab]][[m]], dim)
    
    variable <- outp[[ens_lab]][[m]][[var_sim]]
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
      
      obs_dates <- unique(obs_sub$Date)
      date_index <- which(outp[[ens_lab]][[m]][["Date"]] %in% obs_dates)
      
      if (var_sim == "LKE_lvlwtr") {
        
        df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]][date_index],
                         sim = outp[[ens_lab]][[m]][["LKE_lvlwtr"]][date_index] +
                           min(inp$hypsograph$elev),
                         Model = toggle_models(m, to = "display")) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date"))
      } else if (is.vector(variable)) {
        df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]][date_index],
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
        
        depth <- obs_sub |> 
          dplyr::distinct(depth) |> 
          dplyr::arrange(depth) |>
          dplyr::pull(depth)
        out_depths <- matrix(depth, nrow = length(depth),
                             ncol = length(date_index))
        depths <- outp[[ens_lab]][[m]][["LKE_depths"]][, date_index]
        
        value <- interp_static_grid(var = variable[, date_index],
                                    midpoints = depths,
                                    out_depths = out_depths)
        # Build long dataframe for 2D variable
        each <- length(depth)
        mod <- data.frame(
          Date     = rep(outp[[ens_lab]][[m]][["Date"]][date_index], each = each),
          depth    = as.vector(out_depths),
          sim    = as.vector(value),
          Model = toggle_models(m, to = "display"),
          stringsAsFactors = FALSE
        )
        
        
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
        max_depth <- round(max(lake_level), 2)
        if (depth > max_depth | depth < min_depth) {
          cli::cli_abort("Depth is outside the range of the modelled lake levels [{min_depth}, {max_depth} m].",
                         class = "aeme_error_depth_out_of_range")
          # stop(strwrap(paste0("Depth is outside the range of the modelled lake
          #                     levels [", min_depth, ", ", max_depth, "m]")))
        }
        # Convert surface-referenced depth to bottom-referenced
        out_depths <- if (depth_ref == "surface") {
          matrix(depth, nrow = 1, ncol = ncol(variable))
        } else {
          matrix(max_dep - depth, nrow = 1, ncol = ncol(variable))
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
