#' Resolve GLM extra-variable names to their raw netCDF names
#'
#' Accepts any mix of [glm_extra_vars] AEME names (e.g. `"SED_temp"`), raw
#' GLM/AED netCDF names (e.g. `"sed_temp"`, `"SDF_Fsed_oxy_Z"`), the group
#' keywords `"sediment"`, `"diag"` and `"all"`, and the lake-wide summary
#' suffix `_lake` (see `zone_summary` in [read_glm_output()]).
#'
#' @param x character; names to resolve.
#' @return list with `raw` (character; raw netCDF names, `_lake` suffix
#'   preserved), `groups` (character; any group keywords in `x`) and `none`
#'   (logical; `x` was `"none"`).
#' @noRd
.resolve_glm_extra_vars <- function(x) {
  none <- identical(x, "none")
  groups <- intersect(x, c("sediment", "diag", "all"))
  nms <- setdiff(x, c(groups, "none"))
  data("glm_extra_vars", package = "AEME", envir = environment())
  reg <- glm_extra_vars
  raw <- vapply(nms, \(n) {
    suffix <- if (grepl("_lake$", n)) "_lake" else ""
    base <- sub("_lake$", "", n)
    idx <- match(base, reg$var_aeme)
    paste0(if (is.na(idx)) base else reg$glm_aed[idx], suffix)
  }, character(1), USE.NAMES = FALSE)
  list(raw = unique(raw), groups = groups, none = none)
}

#' Map raw GLM names back to registry AEME names, where one exists
#' @param x character; raw netCDF names (or `<raw>_lake`).
#' @return character; AEME names, `NA` where `x` is not in [glm_extra_vars].
#' @noRd
.glm_extra_aeme_name <- function(x) {
  data("glm_extra_vars", package = "AEME", envir = environment())
  reg <- glm_extra_vars
  base <- sub("_lake$", "", x)
  out <- reg$var_aeme[match(base, reg$glm_aed)]
  out[!is.na(out) & grepl("_lake$", x)] <-
    paste0(out[!is.na(out) & grepl("_lake$", x)], "_lake")
  out
}
