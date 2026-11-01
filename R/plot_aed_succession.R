#' Plot phytoplankton succession
#'
#' Two-panel summary of phytoplankton succession. The top panel is a time series
#' of total chlorophyll a (`PHY_tchla`); the bottom panel is a monthly composite
#' showing the relative contribution (0-100 %) of each phytoplankton group to
#' the total biomass, with one stacked bar per month. The panels share a date
#' axis and the bottom panel takes two thirds of the height.
#'
#' @inheritParams plot_phytos
#' @param groups character vector of phytoplankton group names, matching the
#' `PHY_<group>` variables simulated in `aeme` (e.g. `c("cyano", "green",
#' "diatom")`). Default is `NULL`, which uses all simulated `PHY_` variables
#' other than `PHY_tchla`.
#' @param depth numeric; depth (m, surface-referenced) at which the variables
#' are evaluated. Default `0` (surface).
#'
#' @return A patchwork object of two ggplot panels.
#' @seealso [aed_succession_index()]
#' @importFrom patchwork plot_layout
#' @export
#'
plot_aed_succession <- function(aeme, model, groups = NULL, depth = 0,
                                remove_spin_up = TRUE, ens_n = 1) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)[1]
  } else {
    model <- check_model(model = model)
  }
  if (length(model) > 1) {
    cli::cli_abort("{.arg model} must be a single model; got {.val {model}}.")
  }

  if (is.null(groups)) {
    phy_vars <- model_controls |>
      dplyr::filter(grepl("^PHY_", var_aeme) & simulate) |>
      dplyr::pull(var_aeme)
    phy_vars <- setdiff(unique(phy_vars), "PHY_tchla")
    groups <- sub("^PHY_", "", phy_vars)
  } else {
    phy_vars <- paste0("PHY_", groups)
  }
  if (length(groups) == 0) {
    cli::cli_abort("No phytoplankton groups found; supply {.arg groups}.")
  }

  get_ts <- function(var) {
    get_var(aeme = aeme, model = model, var_sim = var, depth = depth,
            depth_ref = "surface", return_df = TRUE, ens_n = ens_n,
            use_obs = FALSE, remove_spin_up = remove_spin_up) |>
      dplyr::transmute(Date = lubridate::as_date(Date), value)
  }

  # Total chlorophyll a
  chla <- get_ts("PHY_tchla")

  # Monthly composition by group
  mon <- lapply(seq_along(groups), function(i) {
    dplyr::mutate(get_ts(phy_vars[i]), group = groups[i])
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(month = lubridate::floor_date(Date, "month")) |>
    dplyr::group_by(month, group) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop_last") |>
    dplyr::mutate(total = sum(value, na.rm = TRUE),
                  pct = dplyr::if_else(total > 0, 100 * value / total,
                                       NA_real_)) |>
    dplyr::ungroup() |>
    dplyr::mutate(group = factor(group, levels = groups))

  # Same colours as plot_phytos(); other groups fall back to Set2
  known_cols <- c(cyano = "#56B4E9", green = "#009E73", crypto = "#CC79A7",
                  diatom = "#E69F00")
  grp_cols <- known_cols[groups]
  unknown <- is.na(grp_cols)
  if (any(unknown)) {
    grp_cols[unknown] <- rep_len(RColorBrewer::brewer.pal(8, "Set2"),
                                 sum(unknown))
  }
  names(grp_cols) <- groups

  y_lab <- eval(parse(text = "Chlorophyll~(mg~chla~m^-3)"))

  # Shared date axis so the two panels line up
  x_lim <- range(chla$Date, mon$month, na.rm = TRUE)
  x_scale <- ggplot2::scale_x_date(limits = x_lim, expand = c(0, 0))

  p1 <- ggplot2::ggplot(chla, ggplot2::aes(x = Date, y = value)) +
    ggplot2::geom_line(colour = "#000000") +
    x_scale +
    ggplot2::labs(x = NULL, y = y_lab) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  p2 <- ggplot2::ggplot(mon, ggplot2::aes(x = month, y = pct, fill = group)) +
    ggplot2::geom_col(width = 31) +
    x_scale +
    ggplot2::scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = grp_cols) +
    ggplot2::labs(x = "Date", y = "Contribution (%)", fill = "Group") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  patchwork::wrap_plots(p1, p2, ncol = 1, heights = c(1, 2))
}
