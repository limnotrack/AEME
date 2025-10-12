#' Plot variable summary
#'
#' @param data A data frame with the following columns:
#' @param ylim A numeric vector of length 2
#' @param xlim A numeric vector of length 2
#'
#' @return A ggplot object
#' @noRd
plot_var_summary <- function(data, ylim, xlim) {

  utils::data("key_naming", package = "AEME", envir = environment())

  data <- data |>
    tidyr::separate(var_sim, into = c("group", "var", "loc"), sep = "_") |>
    dplyr::mutate(var_sim = paste0(group, "_", var),
                  Model = toggle_models(Model, to = "display"),
                  loc = dplyr::case_when(
                    loc == "top" ~ "Surface",
                    loc == "btm" ~ "Bottom",
                    .default = loc)) |>
    dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                     by = c("var_sim" = "name")) |>
    dplyr::mutate(loc = factor(loc, levels = c("Surface", "Bottom")))

  ggplot2::ggplot() +
    ggplot2::geom_line(data = data, ggplot2::aes(x = Date, y = value,
                                                 color = loc)) +
    ggplot2::facet_wrap(~Model, ncol = 1) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Location")) +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim)
}
