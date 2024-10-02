#' Summarise the Aeme object
#'
#' @inheritParams build_aeme
#' @inheritParams run_aeme
#'
#' @return Aeme object
#' @noRd
#'
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom lubridate dmonths yday month
#' @importFrom tidyr pivot_longer
#'

summarise_aeme <- function(aeme, ens_n = 1) {

  # Observation summary ----
  # obs <- observations(aeme)
  # if (!is.null(obs$lake)) {
  #   lake_summ <- obs$lake |>
  #     dplyr::mutate(doy = lubridate::yday(Date), month = lubridate::month(Date),
  #                   adj_Date = Date - lubridate::dmonths(5),
  #                   depth_mid = (depth_from + depth_to) / 2,
  #                   adj_doy = lubridate::yday(adj_Date)) |>
  #     dplyr::group_by(var_aeme, adj_doy, depth_mid) |>
  #     dplyr::summarise(
  #       doy = doy[1], month = month[1],
  #       mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
  #       median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
  #       n = sum(!is.na(value)), .groups = "keep") |>
  #     dplyr::ungroup() |>
  #     as.data.frame()
  # }
  # if (!is.null(obs$level)) {
  #   level_summ <- obs$level |>
  #     dplyr::mutate(doy = lubridate::yday(Date), month = lubridate::month(Date),
  #                   adj_Date = Date - lubridate::dmonths(5),
  #                   # depth_mid = (depth_from + depth_to) / 2,
  #                   adj_doy = lubridate::yday(adj_Date)) |>
  #     dplyr::group_by(var_aeme, adj_doy) |>
  #     dplyr::summarise(
  #       doy = doy[1], month = month[1],
  #       mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
  #       median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
  #       n = sum(!is.na(value)), .groups = "keep") |>
  #     dplyr::ungroup() |>
  #     as.data.frame()
  # }
  #
  # # Met summary ----
  # inp <- input(aeme)
  #
  # met_summ <- inp$meteo |>
  #   tidyr::pivot_longer(cols = -c(Date), names_to = "meteo", values_to = "value") |>
  #   dplyr::mutate(doy = lubridate::yday(Date), month = lubridate::month(Date),
  #                 adj_Date = Date - lubridate::dmonths(5),
  #                 adj_doy = lubridate::yday(adj_Date)) |>
  #   dplyr::group_by(meteo, adj_doy) |>
  #   dplyr::summarise(
  #     doy = doy[1], month = month[1],
  #     mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
  #     median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
  #     n = sum(!is.na(value)), .groups = "keep") |>
  #   dplyr::ungroup() |>
  #   as.data.frame()
  #
  # # Inflow summary ----
  # inf <- inflows(aeme)
  # if (!is.null(inf$data)) {
  #   inf_names <- names(inf$data)
  #   inf_summ <- lapply(inf_names, \(i) {
  #     dat <- inf$data[[i]]
  #     dat_summ <- dat |>
  #       tidyr::pivot_longer(cols = -c(Date), names_to = "var_aeme", values_to = "value") |>
  #       dplyr::mutate(doy = lubridate::yday(Date), month = lubridate::month(Date),
  #                     adj_Date = Date - lubridate::dmonths(5),
  #                     adj_doy = lubridate::yday(adj_Date)) |>
  #       dplyr::group_by(var_aeme, adj_doy) |>
  #       dplyr::summarise(
  #         doy = doy[1], month = month[1],
  #         mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
  #         median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
  #         n = sum(!is.na(value)), .groups = "keep") |>
  #       dplyr::ungroup() |>
  #       as.data.frame()
  #   })
  #   names(inf_summ) <- inf_names
  # }
  #
  # # Outflow summary ----
  # outf <- outflows(aeme)
  # if (!is.null(outf$data)) {
  #   outf_names <- names(outf$data)
  #   outf_summ <- lapply(outf_names, \(o) {
  #     dat <- outf$data[[o]]
  #     dat_summ <- dat |>
  #       tidyr::pivot_longer(cols = -c(Date), names_to = "var_aeme", values_to = "value") |>
  #       dplyr::mutate(doy = lubridate::yday(Date), month = lubridate::month(Date),
  #                     adj_Date = Date - lubridate::dmonths(5),
  #                     adj_doy = lubridate::yday(adj_Date)) |>
  #       dplyr::group_by(var_aeme, adj_doy) |>
  #       dplyr::summarise(
  #         doy = doy[1], month = month[1],
  #         mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
  #         median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
  #         n = sum(!is.na(value)), .groups = "keep") |>
  #       dplyr::ungroup() |>
  #       as.data.frame()
  #   })
  #   names(outf_summ) <- outf_names
  # }

  # Water balance summary ----
  # wb <- water_balance(aeme)

  # ggplot(met_summ) +
  #   geom_line(aes(x = adj_doy, y = mean, color = meteo)) +
  #   geom_ribbon(aes(x = adj_doy, ymin = mean - sd, ymax = mean + sd, fill = meteo), alpha = 0.2) +
  #   facet_wrap(~meteo, scales = "free_y") +
  #   labs(title = "Meteo summary", x = "Adj. day of year", y = "Value") +
  #   theme_minimal()


  # Output summary ----
  outp <- output(aeme)
  # Filter out NULLs in list
  ens_lab <- paste0("ens_", sprintf("%03d", ens_n))
  out <- outp[[ens_lab]][!sapply(outp[[ens_lab]], is.null)]
  model <- names(out)
  tgt_vars <- get_mod_obs_vars(aeme, model = model, ens_n = ens_n)

  # obs_vars <- get_obs_vars(aeme)
  # out_vars <- get_output_vars(aeme, model = model)
  # tgt_vars <- obs_vars[obs_vars %in% out_vars]
  if (length(tgt_vars) > 0) {
    model_obs_df <- lapply(tgt_vars, \(v) {
      get_var(aeme = aeme, model = model, var_sim = v, use_obs = TRUE)
    }) |>
      dplyr::bind_rows() |>
      dplyr::select(Date, Model, var_aeme, depth_mid, obs, sim)
    outp$model_obs_df <- model_obs_df
  }
  aeme_time <- time(aeme)

  # Calculate seasonal profiles ----
  seas_pro <- calc_seasonal_profiles(aeme = aeme, model = model, ens_n = 1)
  outp$seasonal_profiles <- seas_pro

  ensembles <- names(outp)[grepl("ens", names(outp))]
  outp_summ <- lapply(ensembles, \(ens) {
    model <- names(outp[[ens]])
    mod_summ <- lapply(model, \(m) {
      dates <- outp[[ens]][[m]]$Date
      idx <- which(dates >= aeme_time$start & dates <= aeme_time$stop)
      out <- outp[[ens]][[m]]
      vars <- names(out)
      v_list <- lapply(vars, \(v) {
        va <- out[[v]]
        if (is.matrix(va)) {
          epidep <- out[["HYD_epidep"]][idx]
          depths <- out[["LKE_depths"]][, idx]
          na_idx <- which(is.na(epidep))
          epidep[na_idx] <- apply(depths[, na_idx], 2, \(x) max(x))
          epidep[is.na(epidep)]
          epidep <- ifelse(is.na(epidep), 0, epidep)
          dat <- lapply(1:ncol(va[, idx]), \(c) {
            btm <- which(depths[, c] > epidep[c])
            surf <- which(depths[, c] <= epidep[c])
            s <- mean(va[surf, c], na.rm = TRUE)
            if (length(btm) > 0) {
              b <- mean(va[btm, c], na.rm = TRUE)
            } else {
              b <- NA
            }
            matrix(c(s, b), nrow = 1)
          })
          mat <- do.call(rbind, dat)
          return(mat)
        } else if (is.vector(va)) {
          return(va[idx])
          # df <- data.frame(Date = dates[idx], value = va[idx],
          #                  adj_Date = dates[idx] - lubridate::dmonths(5),
          #                  month = lubridate::month(dates[idx]),
          #                  doy = lubridate::yday(dates[idx])) |>
          #   dplyr::mutate(adj_doy = lubridate::yday(adj_Date))
          #
          # suppressWarnings({
          #   summ <- df |>
          #     dplyr::group_by(adj_doy) |>
          #     dplyr::summarise(
          #       doy = doy[1], month = month[1],
          #       mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
          #       median = median(value, na.rm = TRUE), min = min(value, na.rm = TRUE),
          #       n = sum(!is.na(value)), .groups = "keep") |>
          #     dplyr::ungroup() |>
          #     as.data.frame()
          # })
        }
      })
      names(v_list) <- vars
      v_list$Date <- dates[idx]

      # Filter out NULLs in list
      v_list <- v_list[!sapply(v_list, is.null)]
      return(v_list)
    })
    names(mod_summ) <- model
    return(mod_summ)
  })
  names(outp_summ) <- ensembles

  # inp$meteo <- met_summ
  # input(aeme) <- inp
  #
  # inf$data <- inf_summ
  # inflows(aeme) <- inf
  #
  # outf$data <- outf_summ
  # outflows(aeme) <- outf

  for (n in names(outp_summ)) {
    outp[[n]] <- outp_summ[[n]]
  }
  output(aeme) <- outp

  return(aeme)
}

#' Check if AEME is a summary
#' @param aeme Aeme object
#' @param model Character vector of model names
#' @param ens_n Integer of ensemble number
#' @return Logical vector
#' @noRd
check_if_summary <- function(aeme, model, var_sim, ens_n = 1) {

  outp <- output(aeme)

  ens_lab <- paste0("ens_", sprintf("%03d", ens_n))

  # Check if var_sim is in output
  chk <- sapply(model, \(m) {
    dat <- outp[[ens_lab]][[m]][[var_sim]]
    if (is.vector(dat)) {
      return(FALSE)
    }
    ncol(dat) == 2
  })
  return(all(chk))
}
