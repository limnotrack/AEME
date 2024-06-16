#' Get variable from aeme
#'
#' @inheritParams plot_output
#' @param use_obs logical; if TRUE, use observations to extract the variable at
#' time and depth of observations. Default is FALSE. Use this option if you
#' want to compare model output to observations.
#' @param return_df logical; if TRUE, return a dataframe; if FALSE, return a
#' list. Default is TRUE.
#' @param cumulative logical; if TRUE, return cumulative sum of variable
#'
#' @importFrom dplyr arrange filter left_join mutate select bind_rows case_when
#' rename
#' @importFrom stats approx
#'
#' @return dataframe or list
#' @export

get_var <- function(aeme, model, var_sim, return_df = TRUE,
                    use_obs = FALSE, remove_spin_up = TRUE, cumulative = FALSE) {

  # Extract output from aeme ----
  inp <- input(aeme)
  outp <- output(aeme)
  aeme_time <- time(aeme)
  names(model) <- model

  if (use_obs) {
    obs <- observations(aeme)
    if (var_sim == "LKE_lvlwtr") {
      if (is.null(obs$level)) stop("No observations of lake level found.")
      obs_sub <- obs$level |>
        dplyr::filter(Date >= aeme_time$start & Date <= aeme_time$stop &
                        var_aeme %in% var_sim) |>
        dplyr::arrange(Date)
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
    variable <- outp[[m]][[var_sim]]
    # Empty dataframe to return if variable is not in output
    df <- data.frame(Date = as.Date(NA),
                     lyr_top = NA,
                     value = NA,
                     Model = m,
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
      date_ind <- which(outp[[m]][["Date"]] %in% obs_dates)

      if (var_sim == "LKE_lvlwtr") {

        df <- data.frame(Date = outp[[m]][["Date"]][date_ind],
                         sim = outp[[m]][["LKE_lvlwtr"]][date_ind] +
                           min(inp$hypsograph$elev),
                         Model = m) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date"))
      } else if(is.vector(variable)) {
        df <- data.frame(Date = outp[[m]][["Date"]][date_ind],
                         sim = outp[[m]][[var_sim]][date_ind],
                         Model = m) |>
          dplyr::left_join(obs_sub, by = c("Date" = "Date"))
      } else {

        mod <- lapply(date_ind, \(d) {
          depth <- outp[[m]][["LKE_depths"]][, d]
          v <- outp[[m]][[var_sim]][, d]
          obs_deps <- unique(obs_sub$depth_mid[obs_sub$Date == outp[[m]][["Date"]][d]])

          if (all(is.na(v)) | all(is.na(depth))) {
            return(data.frame(Date = outp[[m]][["Date"]][d],
                              depth_mid = obs_deps,
                              sim = NA,
                              Model = m))
          }
          p <- stats::approx(depth, v, obs_deps, rule = 2)$y
          data.frame(Date = outp[[m]][["Date"]][d],
                     depth_mid = obs_deps,
                     sim = p,
                     Model = m)
        }) |>
          do.call(rbind, args = _)

        df <- dplyr::left_join(obs_sub, mod, by = c("Date" = "Date",
                                                    "depth_mid" = "depth_mid"))

      }
    } else if (is.null(dim(variable))) {
      df <- data.frame(Date = outp[[m]][["Date"]],
                       lyr_top = NA,
                       value = variable,
                       Model = m,
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
      depth <- data.frame(Date = outp[[m]][["Date"]],
                          depth = outp[[m]][["LKE_lvlwtr"]])
      lyr <- outp[[m]][["LKE_layers"]]
      df <- data.frame(Date = rep(outp[[m]][["Date"]], each = nrow(variable)),
                       lyr_top = as.vector(lyr),
                       value = as.vector(variable),
                       Model = m) |>
        dplyr::mutate(lyr_thk = ifelse(c(-999, diff(lyr_top)) < 0, # | is.na(-c(NA,diff(lyr_top))),
                                       c(diff(lyr_top),NA),
                                       c(NA,diff(lyr_top))))

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
    dplyr::bind_rows(lst) |>
    # do.call(rbind, lst) |>
      dplyr::mutate(Model = dplyr::case_when(
        Model == "dy_cd" ~ "DYRESM-CAEDYM",
        Model == "glm_aed" ~ "GLM-AED",
        Model == "gotm_wet" ~ "GOTM-WET"
        ),
        var_sim = var_sim
      )
  } else {
    lst
  }
}
