#' Plot inflows and/or outflows
#'
#' @inheritParams build_aeme
#' @param flow character vector; "inflow", "outflow" or both. Default is both.
#' For outflow, var_sim must be "HYD_flow".
#' @param var_sim character; column name in the data frame. Default is
#' "HYD_flow".
#'
#' @importFrom ggplot2 ggplot aes geom_line labs facet_wrap label_parsed
#' @importFrom dplyr bind_rows mutate select left_join
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' plot_flows(aeme = aeme)
plot_flows <- function(aeme, flow = c("inflow", "outflow"),
                       var_sim = "HYD_flow") {
  # Check if aeme is a Aeme object
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be a Aeme object")
  }

  # Check flow arg
  if (!all(flow %in% c("inflow", "outflow"))) {
    stop("flow must be 'inflow', 'outflow' or both")
  }
  if ("outflow" %in% flow & var_sim != "HYD_flow") {
    stop("var_sim must be 'HYD_flow' for outflow")
  }

  # Load key_naming
  utils::data("key_naming", package = "AEME", envir = environment())

  inf <- list()
  outf <- list()
  if ("inflow" %in% flow) {
    lst <- inflows(aeme)
    df <- lapply(names(lst$data), \(n) {
      lst$data[[n]] |>
        dplyr::mutate(name = n)
    }) |>
      dplyr::bind_rows()
    inf <- df
    if (!var_sim %in% colnames(df)) {
      stop("var_sim must be a column in df")
    }
  }
  if ("outflow" %in% flow) {
    lst <- outflows(aeme)
    df <- lapply(names(lst$data), \(n) {
      lst$data[[n]] |>
        dplyr::mutate(name = n)
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(HYD_flow = .data[["outflow"]])
    outf <- df
  }


  df <- dplyr::bind_rows(inf, outf) |>
    dplyr::select(dplyr::all_of(c("Date", "name", var_sim))) |>
    dplyr::mutate(var_aeme = var_sim) |>
    dplyr::left_join(key_naming[, c("name", "name_parse")],
                     by = c("var_aeme" = "name"))
  # Plot
  df |>
    ggplot2::ggplot(ggplot2::aes(x = Date, y = .data[[var_sim]],
                                 color = .data[["name"]])) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "value", colour = "Stream") +
    ggplot2::facet_wrap(~name_parse, scales = "free", ncol = 1,
                        labeller = ggplot2::label_parsed)

}
