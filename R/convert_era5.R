#' Convert ERA5 to AEME or LER
#'
#' @description
#' Convert ERA5 netCDF to AEME or LakeEnsemblR formats.
#'
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param variable string with ERA5 variable names e.g. "2m_temperature", "total_precipitation"
#' @param year numeric; vector with years
#' @param site string of site name which was used when downloading the data.
#' @param user user ID linked with Copernicus account
#' @param era5_dataset string of which ERA5 dataset to use. Can be 'reanalysis-era5-single-levels' or 'reanalysis-era5-land'
#' @param path filepath to where the downloaded ERA5 ncdf files are stored.
#' @param format string; Either "AEME" or "LER". Default is "AEME".
#'
#' @importFrom stars read_ncdf st_extract
#' @importFrom sf st_as_sf
#'
#' @export
#'

convert_era5 <- function(lat,
                         lon,
                         variable = c("10m_u_component_of_wind",
                                      "10m_v_component_of_wind",
                                      "2m_dewpoint_temperature",
                                      "2m_temperature", "snowfall",
                                      "surface_pressure",
                                      "surface_solar_radiation_downwards",
                                      "surface_thermal_radiation_downwards",
                                      "total_precipitation"),
                         year = 2022,
                         site  = "test",
                         path = ".",
                         format = "AEME") {

  coords <- data.frame(lat = lat, lon = lon)
  coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
  timestep <- "hourly"

  out <- lapply(variable, \(v) {
    out2 <- lapply(year, \(y) {
      file <- file.path(path, paste0("era5", "_", v, "_", timestep, "_", y, "_", site, ".nc"))

      if (!file.exists(file)) {
        stop("Missing the file: ", file)
      }
      var <- era5_ref_table$nc[era5_ref_table$era5 == v]
      dat <- stars::read_ncdf(file, var = var)
      if(var %in% c("sf", "tp")) {
        met <- aggregate(dat, by = "day", FUN = max)
      } else if(var %in% c("ssrd", "strd")) {
        met <- aggregate(dat, by = "day", FUN = function(x) {max(x) / (24 * 60 * 60)})
      } else {
        met <- aggregate(dat, by = "day", FUN = mean)
      }

      df <- met |>
        stars::st_extract(coords_sf) |>
        as.data.frame()
      df <- df[, c("time", var)]
    })
    out2 <- do.call(rbind, out2)
  })

  met <- Reduce(merge, out)

  if ("t2m" %in% colnames(met) | "d2m" %in% colnames(met)) {
    sel_cols <- which(colnames(met) %in% c("t2m", "d2m"))
    met[, sel_cols] <- met[, sel_cols] - 273.15
  }
  if ("tp" %in% colnames(met) | "sf" %in% colnames(met)) {
    sel_cols <- which(colnames(met) %in% c("tp", "sf"))
    met[, sel_cols] <- met[,sel_cols] * 1000 # Convert to m
  }

  if (format == "AEME") {
    names(met) <- era5_ref_table$aeme[match(names(met), era5_ref_table$nc)]
  } else if (format == "LER") {
    names(met) <- era5_ref_table$ler[match(names(met), era5_ref_table$nc)]
  }

  return(met)
}
