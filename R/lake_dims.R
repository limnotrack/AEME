#' Calculate the length, width and area at a give height based on a  bathy table
#'
#' @param lake_shape
#'
#' @return
#' @noRd
#'
#' @importFrom sf st_transform st_centroid st_coordinates
#'
#' @examples
lake_dims <- function(lake_shape) {

  shp <- lake_shape|>
    sf::st_transform(2193)

  # find the vertical length
  centre <- sf::st_centroid(shp)|>
    sf::st_coordinates()

  # draw a vertical line
  top <- centre
  top[2] <- top[2] + 100000
  bot <- centre
  bot[2] <- bot[2] - 100000

  vert <- rbind(top, bot)
  star <- lapply(seq(0,160,20),
                FUN = rotate, vert = vert, centre = centre, shp = shp) |>
    unlist()

  bsn_len <- max(star)
  bsn_wid <- min(star)


  return(c(
    round(as.numeric(bsn_len), 1),
    round(as.numeric(bsn_wid), 1)
  ))
}

#' Function to rotate line
#'
#' @param theta
#'
#' @return
#' @noRd
#'
#' @importFrom sf st_linestring st_sfc st_intersection st_length st_buffer
#'
#' @examples
rotate <- function(theta, vert, centre, shp) {

  theta.rad <- theta * pi / 180 ## in radians

  new_x <- cos( theta.rad ) * ( vert[, 1] - centre[1] ) - sin( theta.rad ) *
    ( vert[, 2] - centre[2] ) + centre[1]
  new_y <- sin( theta.rad ) * ( vert[, 1] - centre[1] ) + cos( theta.rad ) *
    ( vert[, 2] - centre[2] ) + centre[2]
  line <- cbind(x = new_x, y = new_y)|>
    sf::st_linestring()|>
    sf::st_sfc(crs = 2193)

  line2 <- sf::st_intersection(line, sf::st_buffer(shp, 0))


  sf::st_length(line2)|> as.numeric()

}
