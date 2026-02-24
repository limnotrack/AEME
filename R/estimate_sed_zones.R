#' Estimate sediment zones based on hypsograph
#' 
#' @param hypsograph data frame with columns "depth" and "area". Depth should be
#' negative below the surface and positive above. Area should be the lake area at
#' each depth. Hypsograph should be ordered by depth (descending).
#'
#' @returns A numeric vector with the estimated heights of each sediment zone. The
#' length of the vector corresponds to the number of zones. The heights are cumulative
#' from the lake bottom (i.e., the first value is the height of the first zone 
#' from the bottom, the second value is the height of the second zone from the 
#' bottom, etc.). The last value should be equal to the maximum depth of the 
#' lake.
#' @export
#' @importFrom dplyr filter arrange desc mutate pull
estimate_sed_zones <- function(hypsograph) {
  sub_hyps <- hypsograph |> 
    dplyr::filter(depth <= 0) |>
    dplyr::arrange(dplyr::desc(depth)) |>
    dplyr::mutate(
      zone_heights = abs(min(depth) - depth)
    )
  
  n_zones <- estimate_n_zones_hyps(sub_hyps, max_zones = 4)
  
  zone_bp <- get_zone_breakpoints(sub_hyps, n_zones = n_zones)
  
  zone_heights <- sapply(zone_bp, \(d) {
    sub_hyps |> 
      dplyr::filter(depth <= d) |>
      # dplyr::summarise(zone_heights = max(zone_heights)) |>
      dplyr::pull(zone_heights) |> 
      max()
  })
  zone_heights[which.max(zone_heights)] <- max(hypsograph$elev) - min(hypsograph$elev) 
  return(zone_heights)
}

#' Estimate number of sediment zones based on hypsograph slope
#' @noRd
estimate_n_zones_hyps <- function(hyps, max_zones = 5, plot = TRUE) {
  
  stopifnot(all(c("depth", "area") %in% names(hyps)))
  
  hyps <- hyps[order(hyps$depth), ]
  
  # Compute slope |dA/dz|
  dA <- diff(hyps$area)
  dz <- diff(hyps$depth)
  slope <- abs(dA / dz)
  
  dat <- data.frame(
    depth = hyps$depth[-1],
    slope = slope
  )
  
  # Remove any infinite or NA values
  dat <- dat[is.finite(dat$slope), ]
  
  # ----- Determine optimal number of clusters -----
  wss <- sapply(1:max_zones, function(k) {
    kmeans(scale(dat$slope), centers = k, nstart = 20)$tot.withinss
  })
  
  # Elbow detection using second derivative
  d1 <- diff(wss)
  d2 <- diff(d1)
  opt_k <- which.min(d2) + 1
  opt_k <- max(2, opt_k)  # at least 2 zones
  return(opt_k)
}

# get_zone_breakpoints <- function(hyps, n_zones = 3) {
#   
#   stopifnot(all(c("depth", "area") %in% names(hyps)))
#   
#   hyps <- hyps[order(hyps$depth), ] |> 
#     dplyr::filter(depth <= 0)
#   
#   # Compute slope |dA/dz|
#   dA <- diff(hyps$area)
#   dz <- diff(hyps$depth)
#   slope <- abs(dA / dz)
#   
#   dat <- data.frame(
#     depth = hyps$depth[-1],
#     slope = slope
#   )
#   
# 
#   m <- lm(slope ~ depth, data = dat)
#   seg <- segmented::segmented(m, seg.Z = ~depth, npsi = (n_zones - 1))  
#   
#   # summary(seg)
#   # plot(seg)
#   
#   # Extract breakpoints
#   breakpoints <- seg$psi[, "Est."] |> sort()
#   return(breakpoints)
# }

#' Get breakpoints for sediment zones based on hypsograph slope
#' @noRd
get_zone_breakpoints <- function(hyps, n_zones = 3, min_points = 5) {
  
  # ---- Checks ----
  if (!all(c("depth", "area") %in% names(hyps))) {
    stop("hyps must contain 'depth' and 'area' columns")
  }
  
  if (n_zones < 2) {
    stop("n_zones must be >= 2")
  }
  
  # ---- Order and filter ----
  hyps <- hyps[order(hyps$depth), ]
  hyps <- hyps[hyps$depth <= 0, ]
  
  # Need enough data
  if (nrow(hyps) < n_zones * min_points) {
    stop("Not enough data for requested number of zones")
  }
  
  # ---- Compute slope |dA/dz| ----
  dA <- diff(hyps$area)
  dz <- diff(hyps$depth)
  slope <- abs(dA / dz)
  
  dat <- data.frame(
    depth = hyps$depth[-1],
    slope = slope
  )
  
  n <- nrow(dat)
  
  # Candidate breakpoint indices
  candidates <- seq(min_points, n - min_points)
  
  # Helper: compute RSS for a set of break indices
  compute_rss <- function(break_idx) {
    
    # Define segment boundaries
    idx <- c(0, break_idx, n)
    
    total_rss <- 0
    
    for (i in seq_len(length(idx) - 1)) {
      seg_rows <- (idx[i] + 1):idx[i + 1]
      seg_data <- dat[seg_rows, ]
      
      fit <- lm(slope ~ depth, data = seg_data)
      total_rss <- total_rss + sum(residuals(fit)^2)
    }
    
    return(total_rss)
  }
  
  # ---- Grid search ----
  best_rss <- Inf
  best_breaks <- NULL
  
  if (n_zones == 2) {
    
    for (b1 in candidates) {
      rss <- compute_rss(b1)
      if (rss < best_rss) {
        best_rss <- rss
        best_breaks <- b1
      }
    }
    
  } else {
    
    combos <- combn(candidates, n_zones - 1)
    
    for (i in seq_len(ncol(combos))) {
      
      b <- sort(combos[, i])
      
      # Ensure segments have minimum size
      segment_lengths <- diff(c(0, b, n))
      if (any(segment_lengths < min_points)) next
      
      rss <- compute_rss(b)
      
      if (rss < best_rss) {
        best_rss <- rss
        best_breaks <- b
      }
    }
  }
  
  # Convert indices to depths
  breakpoints <- dat$depth[best_breaks]
  
  return(sort(breakpoints))
}


