#' Plot water balance components
#'
#' @description Plot water balance components for a given model including
#' evaporation volume, lake precipitation, lake inflow, and lake outflow.
#'
#' @inheritParams plot_var
#'
#' @return ggplot2 object
#'
#' @export
#'
#' @importFrom dplyr left_join mutate bind_rows
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_line labs theme_bw
#' @importFrom utils data
#'

plot_wbal <- function(aeme, model, cumulative = FALSE) {

  utils::data("key_naming", package = "AEME", envir = environment())

  vars <- c("LKE_evpvol", "LKE_pcpvol", "LKE_inflow", "LKE_outflow")
  if (missing(model)) {
    model <- list_models(aeme)
  }
  df <- lapply(vars, \(v) {
    get_var(aeme, model = model, var_sim = v, return_df = TRUE,
            cumulative = cumulative)
  }) |>
    dplyr::bind_rows()

  df <- df |>
    dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                     by = c("var_sim" = "name")) |>
    dplyr::mutate(
      name_text = factor(name_text, levels = c("Evaporation",
                                               "Precipitation" ,
                                               "Inflow",
                                               "Outflow"))
    )

  y_lab <- eval(parse(text = "Volume~(m^3)"))

  if (all(is.na(df$value))) {
    message("No data to plot. Returning empty plot")
    p <- ggplot2::ggplot() +
      ggplot2::theme_bw()
    return(p)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = Date, y = value, colour = Model)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~name_text, scales = "free_y", ncol = 1,
                        strip.position = "right") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = y_lab)

}
