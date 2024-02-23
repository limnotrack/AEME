#' @title Plot fluxes
#'
#' @description Plot heat fluxes from AEME simulations. This includes incoming
#' shortwave radiation, net longwave radiation, evaporative heat flux, and
#' sensible heat flux.
#'
#' @param aeme AEME data object
#' @param cumulative logical; if \code{TRUE}, plot cumulative fluxes. If
#' \code{FALSE}, plot instantaneous fluxes.
#'
#' @return ggplot2 object
#'
#' @export
#'
#' @importFrom dplyr left_join mutate
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_line labs theme_bw
#' @importFrom utils data
#'

plot_fluxes <- function(aeme, cumulative = FALSE) {

  utils::data("key_naming", package = "AEME", envir = environment())

  vars <- c("LKE_Qsw", "LKE_Qlw", "LKE_Qe", "LKE_Qh")
  df <- lapply(vars, \(v) {
    get_var(aeme, model = model, var_sim = v, return_df = TRUE,
            cumulative = cumulative)
  }) |>
    do.call(what = rbind, args = _)

  df <- df |>
    dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                     by = c("var_sim" = "name")) |>
    dplyr::mutate(
      name_text = factor(name_text, levels = c("Shortwave radiation",
                                               "Longwave radiation" ,
                                               "Evaporative heat flux",
                                               "Sensible heat flux" ))
    )

  y_lab <- eval(parse(text = "Flux~(W~m^-2)"))

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
