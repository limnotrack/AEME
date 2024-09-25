#' Plot observations
#'
#' @inheritParams plot_output
#' @param add_line logical, add line to the plot
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' plot_obs(aeme = aeme, var_sim = c("HYD_temp", "LKE_lvlwtr"))
plot_obs <- function(aeme, var_sim = "HYD_temp", add_line = FALSE) {
  # Check if aeme is a Aeme object
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be a Aeme object")
  }

  # Load key_naming
  utils::data("key_naming", package = "AEME", envir = environment())

  # Load observation slot
  obs <- observations(aeme)
  lake <- obs[["lake"]]
  level <- obs[["level"]]

  # remove LKE_l
  incl_lake_level <- "LKE_lvlwtr" %in% var_sim
  if (incl_lake_level) {
    var_sim <- var_sim[var_sim != "LKE_lvlwtr"]
  }

  if (length(var_sim) == 0 & !incl_lake_level) {
    stop("No lake level observations found!")
  }

  # Check if var_sim is in the lake observations
  if (!all(var_sim %in% lake$var_aeme)) {
    stop("var_sim must be in the lake observations")
  }



  df <- lake |>
    dplyr::filter(var_aeme %in% var_sim)
  if (incl_lake_level) {

    level <- level |>
      dplyr::mutate(depth_from = 0, depth_to = 0)

    df <- dplyr::bind_rows(df, level)
  }
  df <- df |>
    dplyr::mutate(depth_mid = (depth_from + depth_to) / 2)
  n_depths <- unique(df$depth_mid) |>
    length()

  if (n_depths > 15) {
    # Group depths into 10 categories
    df <- df |>
      dplyr::mutate(fdepth = cut(depth_mid, breaks = 15))
    levs <- levels(df$fdepth)
    levs <- gsub(",", "-", levs)
    levs <- gsub("\\(|]", "", levs)
    df <- df |>
      dplyr::mutate(fdepth = factor(fdepth, labels = levs))
  } else {
    df <- df |>
      dplyr::mutate(fdepth = factor(depth_mid))
    levs <- levels(df$fdepth)
  }
  df <- df |>
    dplyr::left_join(key_naming[, c("name", "name_parse")],
                     by = c("var_aeme" = "name"))


  ggplot2::ggplot(df, ggplot2::aes(x = Date, y = value, color = fdepth)) +
    ggplot2::geom_point() +
    {if (add_line) ggplot2::geom_line() else NULL} +
    ggplot2::facet_wrap(~name_parse, scales = "free", ncol = 1,
                        labeller = ggplot2::label_parsed) +
    ggplot2::labs(x = "Date", y = "Value", colour = "Depth (m)")


}
