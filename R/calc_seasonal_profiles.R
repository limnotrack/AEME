#' Calculate seasonal profiles
#'
#' @inheritParams build_aeme
#'
#' @return Data frame with seasonal profiles for each model and variable
#' @noRd
calc_seasonal_profiles <- function(aeme, model, ens_n = 1) {

  # Check output is in aeme
  if (!is(aeme, "Aeme")) stop("aeme is not an `Aeme` object")

  inp <- AEME::input(aeme)
  hyps <- inp$hyps |>
    dplyr::mutate(area_Ha = area / 10000)
  btm_elev <- min(hyps$elev)
  max_lake_depth <- abs(min(hyps$depth))
  total_lake_benthic_area <- sum(hyps$area)
  outp <- AEME::output(aeme)
  ens_lab <- format_ens_label(ens_n = ens_n)

  # Check if var_sim is in output
  chk <- sapply(model, \(m){
    is.null(outp[[ens_lab]][[m]])
  })

  if (any(chk)) stop("model is not in Aeme")

  # Extract modelled temperature and oxygen profiles
  vars_sim <- c("HYD_temp", "CHM_oxy")

  df <- lapply(vars_sim, \(v) {
    AEME::get_var(aeme = aeme, model = model, var_sim = v, ens_n = ens_n)
  }) |>
    dplyr::bind_rows()

  strat_stats <- df |>
    dplyr::filter(var_sim == "HYD_temp") |>
    dplyr::group_by(Date, Model) |>
    dplyr::summarise(
      Ts = value[which.max(lyr_top)],
      Tb = value[which.min(lyr_top)],
      Tdiff = Ts - Tb,
      strat = abs(Tdiff) > 1, .groups = "drop"
    )

  strat_stats_yr <- strat_stats |>
    dplyr::mutate(adj_Date = Date + lubridate::dmonths(5),
                  adj_year = lubridate::year(adj_Date)) |>
    dplyr::group_by(adj_year, Model) |>
    dplyr::summarise(
      strat_days = sum(strat),
      strat_days_prop = strat_days / dplyr::n(),
      .groups = "drop"
    )

  # Designate seasons
  seasons <- data.frame(
    season = c(rep("Spring", 3), rep("Summer", 3), rep("Autumn", 3), rep("Winter", 3)),
    month = c(9:11, 12, 1, 2, 3:5, 6:8)
  ) |>
    dplyr::mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))

  thick <- plyr::round_any(max(df$lyr_thk), 0.1, ceiling)
  n_intervals <- ceiling(max_lake_depth / thick)
  intv <- 1 / n_intervals

  new_hyps <- data.frame(depth = seq(min(hyps$depth), max(hyps$depth), by = thick)) |>
    dplyr::mutate(
      area = approx(hyps$depth, hyps$area,
                    xout = depth, rule = 2)$y
    ) |>
    dplyr::arrange(dplyr::desc(depth))

  if (!0 %in% new_hyps$depth) {
    new_hyps <- data.frame(depth = 0) |>
      dplyr::mutate(area = approx(hyps$depth, hyps$area,
                                  xout = depth, rule = 2)$y) |>
      dplyr::bind_rows(new_hyps)
  }

  lake_benthic_area <- sapply(2:nrow(new_hyps), \(i) {
    ((new_hyps$area[i] + new_hyps$area[i - 1]) / 2) *
      (new_hyps$depth[i - 1] - new_hyps$depth[i])
  }) |>
    sum()
  # new_hyps <- new_hyps |>
  #   dplyr::mutate(benthic_area = c(lake_benthic_area, min(hyps$area)))

  df_adj <- df |>
    dplyr::group_by(Date, Model, var_sim) |>
    dplyr::mutate(elev = btm_elev + cumsum(lyr_thk),
                  level = sum(lyr_thk),
                  depth = max(lyr_top) - lyr_top,
                  depth_centre = dplyr::case_when(
                    lyr_top == max(lyr_top) ~ max(lyr_top),
                    .default = lyr_top - 0.5 * lyr_thk,
                  ),
                  prop_depth = depth_centre / level,
                  # fdepth = cut(depth, breaks = seq(0, max(depth) + 0.5, 0.5)),
                  fprop_depth = cut(prop_depth, breaks = seq(0, 1, intv),
                                    labels = FALSE),
    )
  summary(df_adj)

  season_avg_profile <- df_adj |>
    dplyr::mutate(month = lubridate::month(Date)) |>
    dplyr::left_join(seasons, by = "month") |>
    dplyr::group_by(season, fprop_depth, Model, var_sim) |>
    dplyr::summarise(
      mean = mean(value, na.rm = TRUE),
      std_dev = sd(value, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      depth = max_lake_depth - ((max_lake_depth * fprop_depth) / n_intervals)
    ) |>
    as.data.frame()

  return(season_avg_profile)
}
