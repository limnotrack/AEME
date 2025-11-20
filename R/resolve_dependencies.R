#' Resolve Variable Dependencies
#' 
#' This function resolves the dependencies of a set of simulation variables 
#' based on predefined derivation dependencies.
#' It ensures that all necessary variables are included and orders them such 
#' that hydrological variables (`HYD_*`) are processed first.
#' @param vars_sim A character vector of variable names to simulate.
#' @param deps A named list where each name corresponds to a variable and its 
#' value is a character vector of variables it depends on.
#' @returns A character vector of resolved variable names, including 
#' dependencies, ordered with hydrological variables first.
#' @noRd
resolve_dependencies <- function(vars_sim, deps = .deriv_deps) {
  
  resolved <- character(0)
  to_process <- vars_sim
  
  while (length(to_process) > 0) {
    v <- to_process[1]
    to_process <- to_process[-1]
    
    if (!v %in% resolved) {
      resolved <- c(resolved, v)
      new <- deps[[v]]
      if (length(new) > 0) {
        to_process <- unique(c(to_process, new))
      }
    }
  }
  
  # ---- enforce ordering: HYD_* comes first ----
  is_hyd <- grepl("^HYD_", resolved)
  resolved <- c(resolved[is_hyd], resolved[!is_hyd])
  
  return(resolved)
}
