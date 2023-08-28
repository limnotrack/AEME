#' Wind u anv v vectors to direction and speed
#' @param u vector component (m/s)
#' @param v vector component (m/s)
#'
#' @noRd
uv2ds <- function (u, v) {
  direction <- atan2(u, v)
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]
  speed <- sqrt((u * u) + (v * v))
  res <- cbind(dir = direction, speed = speed)
  return(res)
}


#' Wind speed and direction to uv
#' @param d direction (degrees)
#' @param s speed (m/s)
#'
#' @noRd
ds2uv <- function (d, s) {
  d <- d %% 360
  r <- deg2rad(d)
  u <- sin(r) * s
  v <- cos(r) * s
  cbind(u = u, v = v)
}

#' Convert radians to degrees
#' @param rad radians
#'
#' @noRd
rad2deg <- function(rad) {
  (rad * 180) / (pi)
}

#' Convert degrees to radians
#' @param deg degrees
#'
#' @noRd
deg2rad <- function(deg) {
  (deg * pi) / (180)
}
