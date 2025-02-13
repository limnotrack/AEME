#' Convert aeme.yaml file to list
#'
#' @inheritParams build_aeme
#' @param file filepath; to aeme.yaml file
#'
#' @return aeme object
#' @export
#'
#' @importFrom yaml read_yaml
#' @importFrom sf st_read
#' @importFrom dplyr mutate
#' @importFrom withr local_locale local_timezone
#'

yaml_to_aeme <- function(path, file) {

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  yaml <- yaml::read_yaml(file.path(path, file))
  # if (!is.null(yaml$lake$shape)) {
  #   invisible(capture.output({
  #     yaml$lake$shape <- sf::st_read(file.path(path, yaml$lake$shape))
  #   }))
  # }
  # if (!is.null(yaml$catchment$shape)) {
  #   invisible(capture.output({
  #     yaml$catchment$shape <- sf::st_read(file.path(path, yaml$catchment$shape))
  #   }))
  # }
  if (!is.null(yaml$observations$lake)) {
    yaml$observations$lake <- read.csv(file.path(path,
                                                 yaml$observations$lake)) |>
      dplyr::mutate(Date = as.Date(datetime))
  }
  if (!is.null(yaml$observations$level)) {
    yaml$observations$level <- read.csv(file.path(path,
                                                  yaml$observations$level)) |>
      dplyr::mutate(Date = as.Date(Date))
  }
  if (!is.null(yaml$input$init_temp_profile)) {
    yaml$input$init_temp_profile <-
      read.csv(file.path(path, yaml$input$init_temp_profile))
  }
  if (!is.null(yaml$input$hypsograph)) {
    yaml$input$hypsograph <- read.csv(file.path(path, yaml$input$hypsograph))
  }
  if (!is.null(yaml$input$meteo)) {
    yaml$input$meteo <- read.csv(file.path(path, yaml$input$meteo)) |>
      dplyr::mutate(Date = as.Date(Date))
  }
  if (length(yaml$inflows$data) > 0) {
    yaml$inflows$data <- lapply(yaml$inflows$data, \(i) {
      read.csv(file.path(path, i)) |>
        dplyr::mutate(Date = as.Date(Date))
    })
  }
  if (length(yaml$outflows$data) > 0) {
    yaml$outflows$data <- lapply(yaml$outflows$data, \(i) {
      read.csv(file.path(path, i)) |>
        dplyr::mutate(Date = as.Date(Date))
    })
  }

  yaml$time$start <- as.POSIXct(yaml$time$start, format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")
  yaml$time$stop <- as.POSIXct(yaml$time$stop, format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC")

  aeme <- aeme_constructor(
    lake = yaml$lake,
    # catchment = yaml$catchment,
    time = yaml$time,
    configuration = yaml$configuration,
    observations = yaml$observations,
    input = yaml$input,
    inflows = yaml$inflows,
    outflows = yaml$outflows,
    water_balance = yaml$water_balance,
    output = yaml$output
  )

  return(aeme)
}
