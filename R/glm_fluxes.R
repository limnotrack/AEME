#' Whole-lake C, N and P fluxes from the GLM-AED diagnostic output
#'
#' Integrates the diagnostic variables that libaed-water writes to the GLM
#' `output.nc` (process rates per m3, and fluxes across the sediment-water and
#' air-water interfaces per m2) over the lake, giving mmol per day for each
#' process. This is the basis of the budget returned by [diag_glm_budget()] and
#' drawn by [plot_glm_budget()]: it shows where each element comes from and
#' goes to, and so where a concentration that is out of balance is being made
#' or lost.
#'
#' Volumetric rates (`mmol/m3/d`) are multiplied by the layer volumes, sediment
#' zone fluxes (`*_Z`, `mmol/m2/d`) by `zarea` and surface fluxes by
#' `surface_area`. Each flux is positive in the direction `from` to `to` (see
#' the `from` and `to` columns), so the sediment-water exchange `sediment` to
#' `pom` is negative when particles are settling out.
#'
#' Only variables that GLM wrote are used. AED writes most fluxes only when
#' the module `diag_level` is high enough (`diag_level = 10` gives everything;
#' the `*_swi` terms need at least 2) and the `_Z` zone variables need
#' sediment zones, so a run with fewer diagnostics gives a less complete
#' picture. The variables that were looked for but not found are listed in the
#' `missing` attribute of the result.
#'
#' Denitrification is the sum of `NIT_denit` and, when the organic matter
#' module is coupled to nitrate (`simDenitrification`), `OGM_doc_denit`, which
#' holds nitrogen despite its carbon label.
#'
#' Settling to the sediment is worked out from the per-layer settling rates
#' (`OGM_pon_set`, `PHY_<group>_set_n`, ...) and the hypsograph:
#' `sum(rate * layer thickness * (A[k] - A[k - 1]))`, where the area change
#' across a layer is the sediment that particles settling through it can land
#' on. The sediment-water interface diagnostics (`OGM_pon_swi_Z`,
#' `PHY_phy_swi_*`) report the same thing but in the lake tested here were
#' 10 to 30 times larger than `vvel * concentration` allows (AED scales the
#' layer rate by a thickness that is not that of the layer), and the water
#' column inventory only closes with the hypsograph-based value. Use
#' `settling = "interface"` to see the interface values instead.
#'
#' @param aeme Aeme object that has been run with `glm_aed`. Not needed if
#'   `file` is given.
#' @param file character; path to the GLM `output.nc`. If `NULL` it is found
#'   from `aeme`.
#' @param element character; any of `"C"`, `"N"` and `"P"`.
#' @param path character; root directory used with `aeme`.
#' @param settling character; how settling to the sediment is calculated,
#'   `"hypsograph"` (default) or `"interface"`. See Details.
#'
#' @return a tibble with columns `Date`, `element`, `process`, `from`, `to`,
#'   `var` (the netCDF variable) and `value` (mmol/d, daily mean). The
#'   attribute `missing` lists the flux patterns for which GLM wrote no
#'   variable.
#' @export
#' @examples
#' \dontrun{
#' fx <- calc_glm_fluxes(aeme, element = "N")
#' dplyr::summarise(fx, mean = mean(value), .by = c(process, from, to))
#' }
calc_glm_fluxes <- function(aeme = NULL, file = NULL,
                            element = c("N", "P", "C"), path = NULL,
                            settling = c("hypsograph", "interface")) {
  element <- match.arg(element, several.ok = TRUE)
  settling <- match.arg(settling)
  file <- glm_output_nc(aeme = aeme, file = file, path = path)

  map <- glm_flux_map()
  map <- map[map$element %in% element & map$method %in% c("all", settling), ]

  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)
  time <- glm_nc_time(nc)
  nt <- length(time)

  get_mat <- function(v) {
    x <- ncdf4::ncvar_get(nc, v)
    x[!is.finite(x) | abs(x) > 1e30] <- NA_real_
    if (length(dim(x)) != 2) x <- matrix(x, ncol = nt)
    x
  }
  volume <- if ("V" %in% vars) get_mat("V") else NULL
  zone_area <- if ("zarea" %in% vars) get_mat("zarea") else NULL
  surf_area <- if ("surface_area" %in% vars) get_mat("surface_area") else NULL

  # exposed sediment area in each layer: the hypsograph change across it
  settle_area <- NULL
  if (all(c("H", "A") %in% vars)) {
    height <- get_mat("H")
    area <- get_mat("A")
    thick <- height
    thick[-1, ] <- height[-1, , drop = FALSE] -
      height[-nrow(height), , drop = FALSE]
    d_area <- area
    d_area[-1, ] <- area[-1, , drop = FALSE] -
      area[-nrow(area), , drop = FALSE]
    settle_area <- thick * d_area
  }

  # column sums that stay NA when a time step has no valid cells
  col_total <- function(x) {
    out <- colSums(x, na.rm = TRUE)
    out[colSums(!is.na(x)) == 0] <- NA_real_
    out
  }
  integrate_var <- function(v, kind) {
    x <- get_mat(v)
    switch(kind,
           vol = if (is.null(volume)) NULL else col_total(x * volume),
           zone = if (is.null(zone_area)) NULL else col_total(x * zone_area),
           settle = if (is.null(settle_area)) NULL else
             col_total(x * settle_area),
           surface = if (is.null(surf_area)) NULL else
             as.numeric(x[1, ]) * as.numeric(surf_area[1, ]))
  }

  res <- list()
  missing_patterns <- character()
  for (i in seq_len(nrow(map))) {
    hit <- grep(map$pattern[i], vars, value = TRUE)
    if (length(hit) == 0) {
      missing_patterns <- c(missing_patterns, map$pattern[i])
      next
    }
    for (v in hit) {
      val <- integrate_var(v, map$kind[i])
      if (is.null(val)) next
      res[[length(res) + 1]] <- tibble::tibble(
        datetime = time, element = map$element[i], process = map$process[i],
        from = map$from[i], to = map$to[i], var = v,
        value = val * map$sign[i])
    }
  }
  out <- dplyr::bind_rows(res)
  if (nrow(out) == 0) {
    cli::cli_abort(c("No flux diagnostics found in {.file {basename(file)}}.",
                     i = "Were the AED modules run with {.code diag_level} of
                          2 or more?"))
  }
  out <- out |>
    dplyr::mutate(Date = lubridate::as_date(datetime)) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE),
                     .by = c(Date, element, process, from, to, var)) |>
    dplyr::mutate(value = dplyr::if_else(is.nan(value), NA_real_, value)) |>
    dplyr::arrange(element, process, var, Date)
  attr(out, "missing") <- missing_patterns
  attr(out, "surface_area") <- if (is.null(surf_area)) NA_real_ else
    mean(surf_area, na.rm = TRUE)
  out
}

#' Path to the GLM-AED netCDF output
#'
#' @inheritParams calc_glm_fluxes
#' @return character; path to `output.nc`.
#' @noRd
glm_output_nc <- function(aeme = NULL, file = NULL, path = NULL) {
  if (is.null(file)) {
    if (is.null(aeme)) {
      cli::cli_abort("Either {.arg file} or {.arg aeme} must be provided.")
    }
    files <- get_model_outfile(aeme = aeme, model = "glm_aed", path = path,
                               all = TRUE)[["glm_aed"]]
    file <- unname(files[names(files) == "output"][1])
    if (is.na(file)) {
      cli::cli_abort("No GLM-AED {.file output.nc} found for this lake.")
    }
  }
  if (!file.exists(file)) {
    cli::cli_abort("File {.path {file}} does not exist.")
  }
  file
}

#' Time axis of a GLM netCDF file as POSIXct (UTC)
#'
#' @param nc ncdf4 object.
#' @noRd
glm_nc_time <- function(nc) {
  units <- ncdf4::ncatt_get(nc, "time", "units")$value
  by <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400)
  unit <- sub(" since.*$", "", units)
  origin <- as.POSIXct(sub("^.*since ", "", units), tz = "UTC")
  origin + ncdf4::ncvar_get(nc, "time") * by[[unit]]
}
