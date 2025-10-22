#' Get variable from aeme
#'
#' @inheritParams build_aeme
#' @inheritParams plot_output
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp").
#' @param use_obs logical; if TRUE, use observations to extract the variable at
#' time and depth of observations. Default is FALSE. Use this option if you
#' want to compare model output to observations.
#' @param return_df logical; if TRUE, return a dataframe; if FALSE, return a
#' list. Default is TRUE.
#' @param depth numeric; depth of the variable to extract. Default is NULL. If
#' NULL, the variable profiles are extracted.
#' @param cumulative logical; if TRUE, return cumulative sum of variable
#'
#' @importFrom dplyr arrange filter left_join mutate select bind_rows case_when
#' rename
#' @importFrom stats approx
#'
#' @return dataframe or list
#' @export

get_var <- function(aeme, model, var_sim, depth = NULL, return_df = TRUE,
                    ens_n = 1, use_obs = FALSE, remove_spin_up = TRUE,
                    cumulative = FALSE) {
  
  var_sim <- check_aeme_vars(var_sim)
  # Extract output from aeme ----
  inp <- input(aeme)
  bathy <- inp$hypsograph |>
    dplyr::filter(depth <= 0)
  bathy$depth <- max(bathy$elev) - bathy$elev
  max_dep <- max(bathy$depth)
  outp <- output(aeme)
  aeme_time <- time(aeme)
  names(model) <- model
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  if (use_obs) {
    obs <- observations(aeme)
    if (var_sim == "LKE_lvlwtr") {
      if (is.null(obs$level)) {
        message(strwrap("No lake level observations found. Using bathymetry
                        [depth = 0 m] as lake level.", width = 80))
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
        dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |>
        dplyr::arrange(Date, depth_mid) |>
        dplyr::select(Date, var_aeme, depth_mid, value)
    }
    if (nrow(obs_sub) == 0) {
      stop("No observations found for the model simulation period.")
    }
    obs_sub <- obs_sub |>
      dplyr::rename(obs = value)
  }
  
  # Loop through the models and extract the variable of interest ----
  lst <- lapply(model, \(m) {
    variable <- outp[[ens_lab]][[m]][[var_sim]]
    # Empty dataframe to return if variable is not in output
    df <- data.frame(Date = as.Date(NA),
                     lyr_top = NA,
                     value = NA,
                     Model = toggle_models(m, to = "display"),
                     lyr_thk = NA)
    
    if (is.null(variable)) {
      message(strwrap(paste0(var_sim, " is not in output for model ", m,
                             ". Returning a dataframe with NA's.")))
      return(df)
    }
    if (is.matrix(variable)) {
      if (ncol(variable) == 0) {
        message(strwrap(paste0(var_sim, " is not in output for model ", m,
                               ". Returning a dataframe with NA's.")))
        return(df)
      }
    }
    if (length(variable) == 0) {
      message(strwrap(paste0(var_sim, " is not in output for model ", m,
                             ". Returning a dataframe with NA's.")))
      return(df)
    }
    
    if (use_obs) {
      
      obs_dates <- unique(obs_sub$Date)
      date_ind <- which(outp[[ens_lab]][[m]][["Date"]] %in% obs_dates)
      
      if (var_sim == "LKE_lvlwtr") {
        
        df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]][date_ind],
                         sim = outp[[ens_lab]][[m]][["LKE_lvlwtr"]][date_ind] +
                           min(inp$hypsograph$elev),
                         Model = toggle_models(m, to = "display")) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date"))
      } else if (is.vector(variable)) {
        df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]][date_ind],
                         sim = outp[[ens_lab]][[m]][[var_sim]][date_ind],
                         Model = toggle_models(m, to = "display")) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date")) |>
          dplyr::mutate(
            sim = dplyr::case_when(
              var_sim == "HYD_thmcln" & is.na(sim) ~ max_dep,
              .default = sim
            )
          )
      } else {
        
        mod <- lapply(date_ind, \(d) {
          depth <- outp[[ens_lab]][[m]][["LKE_depths"]][, d]
          v <- outp[[ens_lab]][[m]][[var_sim]][, d]
          obs_deps <- unique(obs_sub$depth_mid[obs_sub$Date == outp[[ens_lab]][[m]][["Date"]][d]])
          
          if (all(is.na(v)) | all(is.na(depth))) {
            return(data.frame(Date = outp[[ens_lab]][[m]][["Date"]][d],
                              depth_mid = obs_deps,
                              sim = NA,
                              Model = toggle_models(m, to = "display")))
          }
          p <- stats::approx(depth, v, obs_deps, rule = 2)$y
          data.frame(Date = outp[[ens_lab]][[m]][["Date"]][d],
                     depth_mid = obs_deps,
                     sim = p,
                     Model = toggle_models(m, to = "display"))
        }) |>
          dplyr::bind_rows()
        
        df <- dplyr::left_join(obs_sub, mod, by = c("Date" = "Date",
                                                    "depth_mid" = "depth_mid"))
        
      }
    } else if (is.null(dim(variable))) {
      df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                       lyr_top = NA,
                       value = variable,
                       Model = toggle_models(m, to = "display"),
                       lyr_thk = NA)
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
      dep <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                        depth = outp[[ens_lab]][[m]][["LKE_lvlwtr"]])
      lyr <- outp[[ens_lab]][[m]][["LKE_layers"]]
      if (!is.null(depth)) {
        min_depth <- 0
        max_depth <- round(max(dep$depth), 2)
        if (depth > max_depth | depth < min_depth) {
          stop(strwrap(paste0("Depth is outside the range of the modelled lake
                              levels [", min_depth, ", ", max_depth, "m]")))
        }
        value <- sapply(1:ncol(variable), \(i) {
          if (all(is.na(variable[, i]))) {
            return(NA)
          }
          p <- stats::approx((dep$depth[i] - lyr[, i]), variable[, i], depth,
                             rule = 2)$y
          return(p)
        })
        df <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                         lyr_top = depth,
                         value = value,
                         Model = toggle_models(m, to = "display"),
                         lyr_thk = NA)
      } else {
        df <- data.frame(Date = rep(outp[[ens_lab]][[m]][["Date"]],
                                    each = nrow(variable)),
                         lyr_top = as.vector(lyr),
                         value = as.vector(variable),
                         Model = toggle_models(m, to = "display")) |>
          dplyr::mutate(lyr_thk = ifelse(c(-999, diff(lyr_top)) < 0, # | is.na(-c(NA,diff(lyr_top))),
                                         c(diff(lyr_top),NA),
                                         c(NA,diff(lyr_top))))
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
