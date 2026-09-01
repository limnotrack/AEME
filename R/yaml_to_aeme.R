#' Convert aeme.yaml file to list
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `yaml_to_aeme()` is soft-deprecated in favour of [aeme_constructor()] (build
#' an `Aeme` object from your own lake data, with full validation) or
#' [new_aeme()] (a quick placeholder object to populate incrementally). It
#' still works and will keep working, but new code should prefer those
#' instead of hand-editing a YAML file.
#'
#' @param file filepath; to aeme.yaml file. Can be used instead of `path` and `file` arguments.
#' @param path directory where aeme.yaml file is located. Can be used instead of `file` argument.
#'
#' @return aeme object
#' @seealso [aeme_constructor()], [new_aeme()]
#' @export
#'
#' @importFrom yaml read_yaml
#' @importFrom sf st_read
#' @importFrom dplyr mutate
#' @importFrom withr local_locale local_timezone
#' @importFrom cli cli_abort
#' @importFrom lifecycle deprecate_soft
#'
#' @examples
#' aeme_yaml <- system.file("extdata/lake/aeme.yaml", package = "AEME")
#' aeme <- yaml_to_aeme(file = aeme_yaml)
#' aeme


yaml_to_aeme <- function(path, file) {

  lifecycle::deprecate_soft(
    when = "0.4.0",
    what = "yaml_to_aeme()",
    details = "Use `aeme_constructor()` to build an Aeme object from your own
    lake data, or `new_aeme()` for a quick placeholder object to populate
    incrementally, instead of a yaml file."
  )

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  if (missing(path)) {
    path <- dirname(file)
    file <- basename(file)
  } else if (missing(file)) {
    file <- list.files(path, pattern = "*.yaml", full.names = FALSE)
    if (length(file) == 0) {
      cli::cli_abort("No yaml file found in {.file {path}}")
    } else if (length(file) > 1) {
      cli::cli_abort("Multiple .yaml files found in {.file {path}}. Please 
                     specify the file name.")
    }
  }
  path <- check_path(path = path, must_exist = TRUE)
  
  yaml <- yaml::read_yaml(file.path(path, file))
  
  # Set package version
  yaml$configuration$aeme_version <- as.character(utils::packageVersion("AEME"))
  
  
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
      dplyr::mutate(Date = as.Date(datetime)) |>
      normalise_lake_obs()
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
