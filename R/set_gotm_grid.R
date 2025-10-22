#' Set grid for GOTM model
#'
#' @param gotm list; GOTM model configuration
#' @param depth numeric; depth of the lake
#' @param method 0=equal by default with optional zooming, 1=prescribed relative
#' fractions, 2=prescribed layer thicknesses
#' @param thickness_factor numeric; factor to multiply the thickness of the
#' layers. Default is 1.
#' @param ddu numeric; Surface zooming [dimensionless; min=0.0; default=0.0]
#' @param ddl numeric; Bottom zooming [dimensionless; min=0.0; default=0.0]
#' @inheritParams build_aeme
#'
#' @return list; GOTM model configuration
#' @noRd
#'
#' @importFrom yaml read_yaml
#'

set_gotm_grid <- function(gotm, path_gotm = NULL, depth, method = 1,
                          aeme = NULL, path = NULL, thickness_factor = 1,
                          ddu = 0, ddl = 0) {

  if (!is.null(aeme)) {

    if (is.null(path)) stop("Please provide the path to the AEME directory")

    lake_dir <- get_lake_dir(aeme = aeme, path = path)
    lke <- lake(aeme)
    depth <- lke$depth
    path_gotm <- file.path(lake_dir, "gotm_wet")
    gotm <- yaml::read_yaml(file.path(path_gotm, "gotm.yaml"))
  }


  sub_layers <- get_model_layers(depth = depth,
                                 thickness_factor = thickness_factor)
  nlev <- nrow(sub_layers)

  # layer thicknesses [0=equal by default with optional zooming, , ; default=0]
  if (method == 0) {
    gotm$grid$method <- 0
    gotm$grid$ddu <- ddu
    gotm$grid$ddl <- ddl

    # 1=prescribed relative fractions
  } else if (method == 1) {
    # Create a fraction each layer occupies with smalller at the surface and
    # larger deeper
    frac <- sub_layers$h / sum(sub_layers$h)
    # f1 <- 0.2 / depth
    # sum(frac)
    # plot(frac)

    lyrs <- data.frame(lyr = c(nlev, frac))
    write.table(lyrs, file.path(path_gotm, "inputs", "lyrs.dat"),
                row.names = FALSE, col.names = FALSE)

    gotm$grid$nlev <- nlev
    gotm$grid$method <- 1
    gotm$grid$file <- "inputs/lyrs.dat"

    # 2=prescribed thicknesses
  } else if (method == 2) {
    lyr_thick <- depth / nlev
    lyrs <- data.frame(lyr = c(nlev, rep(lyr_thick, nlev)))
    write.table(lyrs, file.path(path_gotm, "inputs", "lyrs.dat"),
                row.names = FALSE, col.names = FALSE)

    gotm$grid$method <- 2
    gotm$grid$file <- "inputs/lyrs.dat"
  }

  if (!is.null(aeme)) {
    write_yaml(gotm, file.path(path_gotm, "gotm.yaml"))
  } else {
    return(gotm)
  }
}
