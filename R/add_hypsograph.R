#' Add hypsograph to Aeme object
#'
#' @inheritParams build_aeme
#' @param hypsograph data frame with columns "depth" and "area". Depth should be
#' monotonic decreasing and area should be monotonic increasing. If elevation is
#' not provided, it will be calculated as depth + lake elevation.
#' If NULL, a simple hypsograph will be generated using lake depth and area.
#' @param surf_elev Lake surface elevation (m). Required if hypsograph is NULL.
#' @param lake_depth Lake maximum depth (m). Required if hypsograph is NULL.
#' @param lake_area Lake surface area (m2). Required if hypsograph is NULL.
#' 
#' @importFrom dplyr arrange desc
#'
#' @returns Aeme object with hypsograph added
#' @export
#'
#' @examples
#' depth <- seq(-5, 0, by = 0.5)
#' area <- c(0, 10, 30, 50, 70, 90, 110, 130, 150, 170, 190)
#' hypsograph <- data.frame(depth = depth, area = area)

add_hypsograph <- function(aeme = NULL, hypsograph = NULL,
                           surf_elev = 0, lake_depth = NULL, lake_area = NULL,
                           ext_elev = 0) {
  
  if (!is.null(aeme)) {
    # Check if aeme is a Aeme object
    aeme <- check_aeme(aeme)
    
    lke <- lake(aeme)
    inp <- input(aeme)
    surf_elev <- lke$elevation
    lake_depth <- lke$depth
    lake_area <- lke$area
  }
  
  if (is.null(hypsograph)) {
    warning(paste(strwrap("Hypsograph is not present. This function will
                            generate a simple hypsograph using lake depth and
                            area."),
                  collapse = "\n"))
    if (any(c(is.null(surf_elev), is.null(lake_depth), is.null(lake_area)))) {
      stop(paste(strwrap("Lake elevation, depth and area are not present.
                           These are required to build the models"),
                 sep = "\n"))
    }
    hypsograph <- data.frame(elev = c(surf_elev, surf_elev - lake_depth),
                             area = c(lake_area, 0),
                             depth = c(0, -lake_depth))
  }
  
  # Add elevation column if not present
  if (!"elev" %in% colnames(hypsograph)) {
    message("Adding elevation column to hypsograph")
    hypsograph$elev <- hypsograph$depth + surf_elev
  }
  
  # Extend & arrange hypsograph
  utils::data("model_layer_structure", package = "AEME", envir = environment())
  # Generate a sequence of depths from 0 to the maximum depth
  if (is.null(surf_elev)) {
    surf_elev <- hypsograph |>
      dplyr::filter(depth == 0) |>
      dplyr::pull(elev)
  }
  depths <- model_layer_structure |>
    dplyr::filter(zi <= abs(min(hypsograph$depth))) |>
    dplyr::pull(zi)
  depths <- -depths
  if (!min(hypsograph$depth) %in% depths) {
    depths <- c(depths, min(hypsograph$depth))
  }
  areas <- approx(x = hypsograph$depth, y = hypsograph$area, xout = depths)$y
  if (any(hypsograph$depth > 0)) {
    ext_depths <- hypsograph |>
      dplyr::filter(depth > 0) |>
      dplyr::pull(depth)
    ext_areas <- hypsograph |>
      dplyr::filter(depth > 0) |>
      dplyr::pull(area)
    depths <- c(ext_depths, depths)
    areas <- c(ext_areas, areas)
  }
  hypsograph <- data.frame(elev = surf_elev + depths,
                           area = areas,
                           depth = depths)
  max_elev <- max(hypsograph$elev)
  
  if (ext_elev > 0 & max_elev < (surf_elev + ext_elev)) {
    hypsograph <- extrap_hyps(hypsograph = hypsograph, ext_elev = ext_elev)
  }
  hypsograph <- hypsograph |>
    dplyr::arrange(dplyr::desc(elev)) |>
    dplyr::mutate(elev = round(elev, 2),
                  depth = round(depth, 2),
                  area = round(area, 2)) |> 
    check_hypsograph()
  

  if (is.null(aeme)) {
    return(hypsograph)
  } else {
    # Add hypsograph to input
    inp$hypsograph <- hypsograph
    input(aeme) <- inp
    return(aeme)
  }
}
