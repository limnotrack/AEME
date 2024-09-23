#' Plot a tile plot of meteorological data
#'
#' @inheritParams build_aeme
#' @param var_inp Character. Variable to plot. Can be one of:
#' \itemize{
#' \item \code{"MET_tmpair"}: Air temperature
#' \item \code{"MET_pprain"}: Rainfall
#' \item \code{"MET_wndspd"}: Wind speed
#' \item \code{"MET_humrel"}: Relative humidity
#' \item \code{"MET_radswd"}: Shortwave radiation
#' \item \code{"MET_radlwd"}: Longwave radiation
#' \item \code{"MET_pres"}: Atmospheric pressure
#' \item \code{"MET_ppsnow"}: Snowfall
#' \item \code{"MET_wnddir"}: Wind direction
#' }
#' @param use_hydro_year Logical. If \code{TRUE}, the hydrological year is used.
#'
#' @importFrom ggplot2 geom_tile facet_wrap labs scale_fill_viridis_c
#' @importFrom dplyr case_when left_join filter mutate group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate year month yday
#' @importFrom patchwork wrap_plots
#'
#' @return A ggplot object
#' @export
#'

plot_met_tile <- function(aeme, var_inp = "MET_tmpair", use_hydro_year = TRUE) {

  # Check if aeme is a Aeme object
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be a Aeme object")
  }

  # Load Rdata
  utils::data("key_naming", package = "AEME", envir = environment())

  # Load lake data for hydrological year
  lke <- lake(aeme)
  lat <- lke$latitude

  # Load input slot
  inp <- input(aeme)
  if (!is.data.frame(inp$meteo)) {
    stop("No meteo data found in input slot")
  }

  if (!all(var_inp %in% names(inp$meteo))) {
    stop("Variable not found in meteo data")
  }

  df <- inp$meteo |>
    tidyr::pivot_longer(cols = !dplyr::contains("Date")) |>
    dplyr::filter(name %in% var_inp) |>
    dplyr::left_join(key_naming[, c("name", "name_parse")]
                     , by = c("name" = "name"))

  plot_tile(df = df, lat = lat, use_hydro_year = use_hydro_year)

}


#' Plot a tile plot of a data frame
#' @param df A data frame with the following columns:
#' \itemize{
#' \item \code{Date}: Date of observation
#' \item \code{value}: Value of the variable
#' \item \code{name_parse}: Parsed name of the variable
#' }
#' @param use_hydro_year Logical. If \code{TRUE}, the hydrological year is used
#'
#' @noRd
plot_tile <- function(df, lat, use_hydro_year = TRUE) {
  if (use_hydro_year) {

    if (lat < 0) {
      # Southern hemisphere - hydrological year starts in July
      df <- df |>
        dplyr::mutate(
          year = lubridate::year(Date),
          month = lubridate::month(Date),
          doy = lubridate::yday(Date),
          hyd_year = dplyr::case_when(
            month < 7 ~ year - 1,
            .default = year
          ),
          hyd_doy = dplyr::case_when(
            month < 7 ~ doy + 183,
            month >= 7 ~ doy - 183,
            .default = 0
          ),
          hyd_year = as.factor(hyd_year)
        ) |>
        dplyr::mutate(month = factor(month, levels = c(7:12, 1:6),
                                     labels = c("Jul", "Aug", "Sep", "Oct",
                                                "Nov", "Dec", "Jan", "Feb",
                                                "Mar", "Apr", "May", "Jun")))
    } else if (lat > 0) {
      # Northern hemisphere - hydrological year starts in October
      df <- df |>
        dplyr::mutate(
          year = lubridate::year(Date),
          month = lubridate::month(Date),
          doy = lubridate::yday(Date),
          hyd_year = dplyr::case_when(
            month >= 10 ~ year + 1,
            .default = year
          ),
          hyd_doy = dplyr::case_when(
            month >= 10 ~ doy - 275,
            month < 10 ~ doy + 90,
            .default = 0
          ),
          hyd_year = as.factor(hyd_year)
        ) |>
        dplyr::mutate(month = factor(month, levels = c(10:12, 1:9),
                                     labels = c("Oct", "Nov", "Dec", "Jan",
                                                "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")))
    }
    df <- df |>
      dplyr::mutate(year = hyd_year, doy = hyd_doy)
    y_lab <- "Hydrological year"
  } else {
    # No hydrological year
    df <- df |>
      dplyr::mutate(year = as.factor(lubridate::year(Date)),
                    month = factor(lubridate::month(Date), levels = 1:12,
                                   labels = month.abb))
    y_lab <- "Year"
  }
  df2 <- df |>
    dplyr::group_by(year, month, name_parse) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  plist <- lapply(unique(df2$name_parse), \(n) {
    df2 |>
      dplyr::filter(name_parse == n) |>
      ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x = month, y = year,
                                      fill = value)) +
      ggplot2::facet_wrap(~name_parse, scales = "free_y",
                          labeller = ggplot2::label_parsed) +
      ggplot2::labs(x = "Month", y = y_lab) +
      ggplot2::scale_fill_viridis_c()

  })

  if (length(plist) == 1) {
    return(plist[[1]])
  } else {
    patchwork::wrap_plots(plist, ncol = 1)
  }
}
