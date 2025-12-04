#' Load a GLM nml file and convert to aeme object
#'
#' @param nml_file Path to GLM nml file
#'
#' @returns Aeme object
#' @export
#'

glm_to_aeme <- function(nml_file) {
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  # nml_file <- "inst/extdata/glm_aed/glm3.nml"
  base_dir <- dirname(nml_file)
  
  nml <- read_nml(nml_file)
  
  # Lake
  elevation <- max(nml$morphometry$H)
  lake <- list(
    name = nml$morphometry$lake_name,
    id = "glm001",
    latitude = nml$morphometry$latitude,
    longitude = nml$morphometry$longitude,
    elevation = elevation,
    depth = max(nml$morphometry$H) - min(nml$morphometry$H),
    area = max(nml$morphometry$A)
  )
  
  # Time
  time <- list(
    start = as.POSIXct(nml$time$start),
    stop = as.POSIXct(nml$time$stop)
  )
  
  # Inputs
  hypsograph <- data.frame(
    depth = (nml$morphometry$H),
    area = (nml$morphometry$A),
    elev = elevation - abs(nml$morphometry$H)
  ) |> 
    dplyr::arrange(dplyr::desc(depth))
  
  met_file <- nml$meteorology$meteo_fl
  met_filepath <- file.path(base_dir, met_file)
  # Check met file exists
  if (!file.exists(met_filepath)) {
    cli::cli_abort("Meteorological file {.file {met_filepath}} does not exist.")
  }
  met <- read.csv(met_filepath)
  met$time <- as.POSIXct(met$time)
  
  # Rename columns to match aeme expectations
  glm_aeme_names <- data.frame(
    glm = c("time", "ShortWave", "LongWave", "AirTemp", "RelHum", "WindSpeed", 
            "Rain", "Snow", "AirPres"),
    aeme = c("Date", "MET_radswd", "MET_radlwd", "MET_tmpair", "MET_humrel",
             "MET_wndspd", "MET_pprain", "MET_ppsnow", "MET_prsttn")
  )
  colnames(met) <- dplyr::recode(colnames(met), !!!setNames(glm_aeme_names$aeme, 
                                                            glm_aeme_names$glm))
  
  input = list(
    hypsograph = hypsograph,
    meteo = met,
    Kw = nml$light$Kw,
    init_depth = nml$init_profiles$lake_depth
  )
  
  aeme <- aeme_constructor(
    lake = lake,
    time = time,
    input = input
  )
  return(aeme)
}
