#' Estimate sediment zones based on hypsograph
#'
#' @param hypsograph data frame with columns "depth" and "area". Depth should be
#' negative below the surface and positive above. Area should be the lake area at
#' each depth. Hypsograph should be ordered by depth (descending).
#' @param n_zones integer or `NULL`; number of sediment zones to return. `NULL`
#' (default) lets the zone count be inferred from the hypsograph slope
#' (`estimate_n_zones_hyps()`). When given, exactly `n_zones` heights are
#' returned, split by cumulative benthic area (falling back to evenly spaced
#' heights if the hypsograph is too coarse for that many zones).
#'
#' @returns A numeric vector with the estimated heights of each sediment zone. The
#' length of the vector corresponds to the number of zones. The heights are cumulative
#' from the lake bottom (i.e., the first value is the height of the first zone
#' from the bottom, the second value is the height of the second zone from the
#' bottom, etc.). The last value should be equal to the maximum depth of the
#' lake.
#' @export
#' @importFrom dplyr filter arrange desc mutate pull
estimate_sed_zones <- function(hypsograph, n_zones = NULL) {
  sub_hyps <- hypsograph |>
    dplyr::filter(depth <= 0) |>
    dplyr::arrange(dplyr::desc(depth)) |>
    dplyr::mutate(
      zone_heights = abs(min(depth) - depth)
    )

  max_sed_height <- max(hypsograph$elev) - min(hypsograph$elev)

  # Caller-fixed zone count (e.g. from a sediment/n_zones model parameter):
  # skip the slope-based inference and split by cumulative benthic area.
  if (!is.null(n_zones)) {
    n_zones <- as.integer(n_zones)
    if (is.na(n_zones) || n_zones < 1)
      stop("n_zones must be a positive integer.")
    if (n_zones == 1) return(ceiling(max_sed_height))
    return(.sed_zone_heights_for_n(sub_hyps, n_zones, max_sed_height))
  }

  n_zones <- estimate_n_zones_hyps(sub_hyps, max_zones = 4)

  zone_bp <- get_zone_breakpoints(sub_hyps, n_zones = n_zones)

  zone_heights <- sapply(zone_bp, \(d) {
    sub_hyps |>
      dplyr::filter(depth <= d) |>
      dplyr::pull(zone_heights) |>
      max()
  })
  zone_heights[which.max(zone_heights)] <- ceiling(max_sed_height)
  return(zone_heights)
}

#' Split a hypsograph into `n_zones` sediment zones by cumulative benthic area
#'
#' Returns `n_zones` strictly ascending zone-top heights above the bed. Zone
#' breaks are placed where cumulative bed area (approximated from the planform
#' area profile) reaches evenly spaced fractions of the total; if that fails to
#' yield `n_zones` distinct ascending heights the range is split evenly.
#' @noRd
.sed_zone_heights_for_n <- function(sub_hyps, n_zones, max_sed_height) {
  ord <- order(sub_hyps$zone_heights)
  hh  <- sub_hyps$zone_heights[ord]
  aa  <- sub_hyps$area[ord]

  zh <- NULL
  if (length(hh) >= 2 && diff(range(aa)) > 0) {
    cum <- (aa - min(aa)) / (max(aa) - min(aa))      # 0 at bed -> 1 at surface
    zh <- stats::approx(cum, hh, xout = seq_len(n_zones) / n_zones,
                        rule = 2, ties = "ordered")$y
  }
  if (is.null(zh) || anyNA(zh) || length(unique(round(zh, 6))) != n_zones ||
      is.unsorted(zh, strictly = TRUE)) {
    zh <- seq(max_sed_height / n_zones, max_sed_height, length.out = n_zones)
  }
  zh[n_zones] <- ceiling(max_sed_height)
  zh
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


