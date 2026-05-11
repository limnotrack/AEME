#' S4 Class representing AEME data
#'
#' This class represents data related to a lake for running AEME. Items in bold
#' are required to run the models.
#' @title Aeme Class
#' @name Aeme
#' @aliases Aeme-class
#' @slot lake A list representing lake information. \itemize{
#'  \item \code{\bold{name}}: character; lake name.
#'  \item \code{\bold{id}}: character; lake identifier.
#'  \item \code{\bold{latitude}}: numeric; lake latitude.
#'  \item \code{\bold{longitude}}: numeric; lake longitude.
#'  \item \code{\bold{elevation}}: numeric; lake elevation.
#'  \item \code{\bold{depth}}: numeric; lake depth.
#'  \item \code{\bold{area}}: numeric; lake area.
#'  }
#' @slot time A list representing time information. \itemize{
#' \item \code{\bold{start}}: character; start date.
#' \item \code{\bold{stop}}: character; end date.
#' \item \code{\bold{timestep}}: numeric; time step.
#' \item \code{\bold{spin_up}}: list; spin up information for each model
#' }
#' @slot configuration A list representing each model's configuration. \itemize{
#' \item \code{model_controls}: dataframe; Model controls for simulation.
#' \item \code{dy_cd}: list; DYRESM-CAEDYM configuration.
#' \item \code{glm_aed}: list; GLM-AED configuration.
#' \item \code{gotm_wet}: list; GOTM-WET configuration.
#' }
#' @slot observations A list representing observation information. \itemize{
#' \item \code{lake}: dataframe; lake observations.
#' \item \code{level}: dataframe; lake level observations.
#' }
#' @slot input A list representing input information. \itemize{
#' \item \code{init_profile}: dataframe; initial temperature profile (if none
#' use NULL or leave empty; if empty/NULL, the observations file will be used).
#' \item \code{\bold{init_depth}}: numeric; initial height of lake surface relative to
#' the bottom (m).
#' \item \code{\bold{hypsograph}}: dataframe; hypsograph.
#' \item \code{\bold{meteo}}: dataframe; meteorological data.
#' \item \code{\bold{use_lw}}: logical; use longwave radiation.
#' \item \code{\bold{Kw}}: numeric; light extinction coefficient (m-1).
#' }
#' @slot inflows A list representing inflows information. \itemize{
#' \item \code{data}: named list of inflow dataframes.
#' \item \code{factor}: named list; inflow factors for each model.
#' }
#' @slot outflows A list representing outflows information. \itemize{
#' \item \code{data}: named list of outflow dataframes.
#' \item \code{factor}: named list; outflow factors for each model.
#' \item \code{lvl}: numeric; height of lake level outflow.
#' }
#' @slot water_balance A list representing water balance information. \itemize{
#' \item\code{\bold{method}}: integer; Method for calculating water balance.
#' 1 = none, 2 = outflows, 3 = inflows and outflows.
#' \item\code{\bold{use}}: character; Can be 'obs' or 'mod'. Use observations
#'  or modelled data for water balance.
#' \item{\code{data}}: list of dataframe for water balance.
#' }
#' @slot output A list representing output information. \itemize{
#' \item \code{dy_cd}: list; DYRESM-CAEDYM output.
#' \item \code{glm_aed}: list; GLM-AED output.
#' \item \code{gotm_wet}: list; GOTM-WET output.
#' }
#' @slot parameters A dataframe representing model parameters.
#' @export

setClass("Aeme",
         representation(
           lake = "list",
           # catchment = "list",
           time = "list",
           configuration = "list",
           observations = "list",
           input = "list",
           inflows = "list",
           outflows = "list",
           water_balance = "list",
           output = "list",
           parameters = "data.frame"
         )
)

# Validity checking ----
# Restored from commented-out state and expanded to cover all slots properly.
# methods::validObject() in setter methods will now actually enforce these rules.
# Errors: hard failures - wrong type or impossible value; object cannot be used.
# Warnings: suspicious values the user should know about but that don't block construction.
setValidity("Aeme", function(object) {
  errors <- character()
  
  # -- Slot-level type checks (hard errors) ------------------------------------
  if (!is.list(object@lake))
    errors <- c(errors, "@lake must be a list")
  
  if (!is.list(object@time))
    errors <- c(errors, "@time must be a list")
  
  if (!is.list(object@configuration))
    errors <- c(errors, "@configuration must be a list")
  
  if (!is.list(object@observations))
    errors <- c(errors, "@observations must be a list")
  
  if (!is.list(object@input))
    errors <- c(errors, "@input must be a list")
  
  if (!is.list(object@inflows))
    errors <- c(errors, "@inflows must be a list")
  
  if (!is.list(object@outflows))
    errors <- c(errors, "@outflows must be a list")
  
  if (!is.list(object@water_balance))
    errors <- c(errors, "@water_balance must be a list")
  
  if (!is.list(object@output))
    errors <- c(errors, "@output must be a list")
  
  if (!is.data.frame(object@parameters))
    errors <- c(errors, "@parameters must be a data.frame")
  
  # -- Lake sub-element checks -------------------------------------------------
  lke <- object@lake
  if (length(lke) > 0) {
    if (!is.null(lke$name) && !is.character(lke$name))
      errors <- c(errors, "@lake$name must be a character")
    if (!is.null(lke$latitude) && !is.numeric(lke$latitude))
      errors <- c(errors, "@lake$latitude must be numeric")
    if (!is.null(lke$longitude) && !is.numeric(lke$longitude))
      errors <- c(errors, "@lake$longitude must be numeric")
    if (!is.null(lke$elevation) && !is.numeric(lke$elevation))
      errors <- c(errors, "@lake$elevation must be numeric")
    if (!is.null(lke$depth) && !is.numeric(lke$depth))
      errors <- c(errors, "@lake$depth must be numeric")
    if (!is.null(lke$area) && !is.numeric(lke$area))
      errors <- c(errors, "@lake$area must be numeric")
    
    # Suspicious-but-valid lake values (warnings, not errors)
    if (!is.null(lke$latitude) && is.numeric(lke$latitude) &&
        (lke$latitude < -90 || lke$latitude > 90))
      cli::cli_warn(
        c("!" = "@lake$latitude {.val {lke$latitude}} is outside [-90, 90].",
          "i" = "Check that latitude and longitude have not been swapped."),
        class = "aeme_warn_latitude"
      )
    if (!is.null(lke$longitude) && is.numeric(lke$longitude) &&
        (lke$longitude < -180 || lke$longitude > 180))
      cli::cli_warn(
        c("!" = "@lake$longitude {.val {lke$longitude}} is outside [-180, 180].",
          "i" = "Check that latitude and longitude have not been swapped."),
        class = "aeme_warn_longitude"
      )
    if (!is.null(lke$depth) && is.numeric(lke$depth) && lke$depth <= 0)
      cli::cli_warn(
        c("!" = "@lake$depth {.val {lke$depth}} is <= 0.",
          "i" = "Depth should be a positive value in metres."),
        class = "aeme_warn_depth"
      )
    if (!is.null(lke$area) && is.numeric(lke$area) && lke$area <= 0)
      errors <- c(errors, "@lake$area must be positive (> 0)")
  }
  
  # -- water_balance sub-element checks ----------------------------------------
  wb <- object@water_balance
  if (!is.null(wb$method)) {
    if (!is.numeric(wb$method))
      errors <- c(errors, "@water_balance$method must be numeric")
    else if (wb$method < 1 || wb$method > 3)
      errors <- c(errors, "@water_balance$method must be 1, 2, or 3")
  }
  if (!is.null(wb$use) && !wb$use %in% c("obs", "mod"))
    errors <- c(errors, "@water_balance$use must be 'obs' or 'mod'")
  
  if (length(errors) == 0) TRUE else errors
})


#' Constructor function for Aeme class
#'
#' @param lake List representing lake information.
#' @param time List representing time information.
#' @param configuration List representing configuration information.
#' @param observations List representing observation information.
#' @param input List representing input information.
#' @param inflows List representing inflows information.
#' @param outflows List representing outflows information.
#' @param water_balance List representing water balance information.
#' @param output List representing output information.
#' @param parameters Dataframe containing model parameters.
#' @param print Logical; print messages. Default is TRUE.
#' @return An instance of the Aeme class.
#'
#' @importFrom sf st_area sf_use_s2
#' @importFrom units drop_units
#' @importFrom lubridate is.Date
#' @importFrom withr local_locale local_timezone
#' @importFrom cli cli_abort cli_warn cli_inform
#'
#' @export

aeme_constructor <- function(
    lake, time, configuration, observations,
    input, inflows, outflows, water_balance, output, parameters, print = TRUE
) {
  
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  # If missing arguments, create default list objects
  if (missing(lake) & missing(time) & missing(input)) {
    cli::cli_abort("Objects lake, time, and input must be provided.")
  }
  cfg_dflt <- list(
    model_controls = NULL,
    use_bgc = FALSE,
    dy_cd = NULL,
    glm_aed = NULL,
    gotm_wet = NULL
  )
  if (missing(configuration)) {
    configuration <- cfg_dflt
  } else {
    for (i in names(cfg_dflt)) {
      if (!i %in% names(configuration)) {
        configuration[[i]] <- cfg_dflt[[i]]
      }
    }
  }
  obs_dflt <- list(
    lake = NULL,
    level = NULL
  )
  if (missing(observations)) {
    observations <- obs_dflt
  } else {
    for (i in names(obs_dflt)) {
      if (!i %in% names(observations)) {
        observations[[i]] <- obs_dflt[[i]]
      }
    }
  }
  
  if (!is.null(observations[["level"]])) {
    # Add var_aeme column if missing
    if (!"var_aeme" %in% colnames(observations[["level"]])) {
      observations[["level"]][["var_aeme"]] <- "LKE_lvlwtr"
    }
  }
  
  
  if (missing(inflows)) {
    inflows <- list(
      data = NULL,
      factor = list(
        dy_cd = 1,
        glm_aed = 1,
        gotm_wet = 1
      )
    )
  }
  if (missing(outflows)) {
    outflows <- list(
      data = NULL,
      elevation = -1,
      factor = list(
        dy_cd = 1,
        glm_aed = 1,
        gotm_wet = 1
      )
    )
  }
  wbal_dflt <- list(
    use = "obs",
    method = 2,
    params = NULL,
    data = list(
      model = NULL,
      wbal = NULL
    )
  )
  if (missing(water_balance)) {
    water_balance <- wbal_dflt
  } else {
    for (i in names(wbal_dflt)) {
      if (!i %in% names(water_balance)) {
        water_balance[[i]] <- wbal_dflt[[i]]
      }
    }
  }
  if (missing(output)) {
    output <- list(
      n_members = 0,
      dy_cd = NULL,
      glm_aed = NULL,
      gotm_wet = NULL
    )
  }
  param_names <- param_colnames(incl_opt = FALSE)
  if (missing(parameters)) {
    parameters <- data.frame(matrix(nrow = 0, ncol = length(param_names)))
    colnames(parameters) <- param_names
  }
  
  # Validate top-level argument types - each checked individually so the user
  # sees exactly which argument is wrong, not just "something is wrong".
  list_args <- list(
    lake = lake, time = time, configuration = configuration,
    observations = observations, input = input, inflows = inflows,
    outflows = outflows, water_balance = water_balance, output = output
  )
  bad_lists <- names(Filter(Negate(is.list), list_args))
  if (length(bad_lists) > 0) {
    cli::cli_abort(
      c("The following arguments must be lists:",
        "x" = "{.arg {bad_lists}} {?is/are} not {.cls list}."),
      class = "aeme_error_bad_type"
    )
  }
  if (!is.data.frame(parameters)) {
    cli::cli_abort(
      c("{.arg parameters} must be a {.cls data.frame}.",
        "x" = "Got {.cls {class(parameters)}}."),
      class = "aeme_error_bad_type"
    )
  }
  
  # Lake type checking for specific elements
  if (!is.character(lake$name)) {
    cli::cli_abort(
      c("{.arg lake$name} must be a {.cls character}.",
        "x" = "Got {.cls {class(lake$name)}}."),
      class = "aeme_error_lake_name"
    )
  }
  if (any(grepl("[^[:alnum:]]", lake$name))) {
    cli::cli_abort(
      c("{.arg lake$name} {.val {lake$name}} contains non-alphanumeric characters.",
        "x" = "Only letters and numbers are allowed.",
        "i" = "Remove spaces, underscores, and punctuation from the lake name."),
      class = "aeme_error_lake_name"
    )
  }
  if (!is.character(lake$id)) {
    lake$id <- as.character(lake$id)
    if (is.na(lake$id)) {
      cli::cli_abort(
        c("{.arg lake$id} could not be coerced to {.cls character}.",
          "x" = "Got {.cls {class(lake$id)}}."),
        class = "aeme_error_lake_id"
      )
    }
    cli::cli_warn(
      c("!" = "{.arg lake$id} was not a {.cls character} and was coerced.",
        "i" = "Supply {.arg lake$id} as a character string to avoid this."),
      class = "aeme_warn_lake_id_coerced"
    )
  }
  if (any(grepl("[^[:alnum:]]", lake$id))) {
    cli::cli_abort(
      c("{.arg lake$id} {.val {lake$id}} contains non-alphanumeric characters.",
        "x" = "Only letters and numbers are allowed.",
        "i" = "Remove spaces, underscores, and punctuation from the lake id."),
      class = "aeme_error_lake_id"
    )
  }
  if (!is.numeric(lake$latitude)) {
    cli::cli_abort(
      c("{.arg lake$latitude} must be {.cls numeric}.",
        "x" = "Got {.cls {class(lake$latitude)}}."),
      class = "aeme_error_lake_coords"
    )
  }
  if (!is.numeric(lake$longitude)) {
    cli::cli_abort(
      c("{.arg lake$longitude} must be {.cls numeric}.",
        "x" = "Got {.cls {class(lake$longitude)}}."),
      class = "aeme_error_lake_coords"
    )
  }
  if (lake$latitude < -90 || lake$latitude > 90) {
    cli::cli_warn(
      c("!" = "{.arg lake$latitude} {.val {lake$latitude}} is outside [-90, 90].",
        "i" = "Check that latitude and longitude have not been swapped."),
      class = "aeme_warn_latitude"
    )
  }
  if (lake$longitude < -180 || lake$longitude > 180) {
    cli::cli_warn(
      c("!" = "{.arg lake$longitude} {.val {lake$longitude}} is outside [-180, 180].",
        "i" = "Check that latitude and longitude have not been swapped."),
      class = "aeme_warn_longitude"
    )
  }
  if (!is.numeric(lake$elevation)) {
    cli::cli_abort(
      c("{.arg lake$elevation} must be {.cls numeric}.",
        "x" = "Got {.cls {class(lake$elevation)}}."),
      class = "aeme_error_lake_elevation"
    )
  }
  if (!is.numeric(lake$depth)) {
    cli::cli_abort(
      c("{.arg lake$depth} must be {.cls numeric}.",
        "x" = "Got {.cls {class(lake$depth)}}."),
      class = "aeme_error_lake_depth"
    )
  }
  if (is.numeric(lake$depth) && lake$depth <= 0) {
    cli::cli_warn(
      c("!" = "{.arg lake$depth} {.val {lake$depth}} is <= 0.",
        "i" = "Depth should be a positive value in metres."),
      class = "aeme_warn_depth"
    )
  }
  if (is.null(lake$area)) {
    cli::cli_abort(
      c("{.arg lake$area} must be provided.",
        "i" = "Supply lake surface area in square metres."),
      class = "aeme_error_lake_area"
    )
  }
  if (!is.numeric(lake$area)) {
    cli::cli_abort(
      c("{.arg lake$area} must be {.cls numeric}.",
        "x" = "Got {.cls {class(lake$area)}}."),
      class = "aeme_error_lake_area"
    )
  }
  if (is.numeric(lake$area) && lake$area <= 0) {
    cli::cli_abort(
      c("{.arg lake$area} must be positive.",
        "x" = "Got {.val {lake$area}} m^2."),
      class = "aeme_error_lake_area"
    )
  }
  
  # Time type checking for specific elements
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  if (is.character(time$start)) {
    cli::cli_inform(
      c("i" = "{.arg time$start} is a {.cls character}; converting to {.cls POSIXct} (UTC)."),
      class = "aeme_inform_time_coerced"
    )
    time$start <- as.POSIXct(time$start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if (is.na(time$start))
      cli::cli_abort(
        c("{.arg time$start} could not be parsed as a date-time.",
          "i" = "Expected format: {.code YYYY-MM-DD HH:MM:SS}."),
        class = "aeme_error_time_start"
      )
  } else if (!is.POSIXct(time$start)) {
    cli::cli_abort(
      c("{.arg time$start} must be {.cls POSIXct} or a parseable {.cls character}.",
        "x" = "Got {.cls {class(time$start)}}."),
      class = "aeme_error_time_start"
    )
  }
  if (is.character(time$stop)) {
    cli::cli_inform(
      c("i" = "{.arg time$stop} is a {.cls character}; converting to {.cls POSIXct} (UTC)."),
      class = "aeme_inform_time_coerced"
    )
    time$stop <- as.POSIXct(time$stop, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if (is.na(time$stop))
      cli::cli_abort(
        c("{.arg time$stop} could not be parsed as a date-time.",
          "i" = "Expected format: {.code YYYY-MM-DD HH:MM:SS}."),
        class = "aeme_error_time_stop"
      )
  } else if (!is.POSIXct(time$stop)) {
    cli::cli_abort(
      c("{.arg time$stop} must be {.cls POSIXct} or a parseable {.cls character}.",
        "x" = "Got {.cls {class(time$stop)}}."),
      class = "aeme_error_time_stop"
    )
  }
  if (time$stop <= time$start) {
    cli::cli_abort(
      c("{.arg time$stop} must be after {.arg time$start}.",
        "x" = "start: {.val {as.character(time$start)}}",
        "x" = "stop:  {.val {as.character(time$stop)}}"),
      class = "aeme_error_time_order"
    )
  }
  if (is.null(time$time_step)) {
    cli::cli_inform(
      c("!" = "{.arg time$time_step} is missing.",
        "i" = "Defaulting to {.val {3600L}} seconds (1 hour)."),
      class = "aeme_inform_time_step_default"
    )
    time$time_step <- 3600
  }
  if (!is.numeric(time$time_step)) {
    cli::cli_abort(
      c("{.arg time$time_step} must be {.cls numeric}.",
        "x" = "Got {.cls {class(time$time_step)}}."),
      class = "aeme_error_time_step"
    )
  }
  if (!is.list(time$spin_up)) {
    if (is.null(time$spin_up)) {
      cli::cli_inform(
        c("!" = "{.arg time$spin_up} is missing.",
          "i" = "Defaulting to 2 days spin-up for all models."),
        class = "aeme_inform_spin_up_default"
      )
      time$spin_up <- list(dy_cd = 2, glm_aed = 2, gotm_wet = 2)
    } else {
      cli::cli_abort(
        c("{.arg time$spin_up} must be a {.cls list} of numeric values.",
          "x" = "Got {.cls {class(time$spin_up)}}."),
        class = "aeme_error_spin_up"
      )
    }
  } else if (all(!is.numeric(unlist(time$spin_up)))) {
    cli::cli_abort(
      c("All values in {.arg time$spin_up} must be {.cls numeric}.",
        "i" = "Expected a named list with entries for {.code dy_cd}, {.code glm_aed}, and {.code gotm_wet}."),
      class = "aeme_error_spin_up"
    )
  }
  
  # Configuration type checking for specific elements
  if (!is.null(configuration$model_controls) &&
      !is.data.frame(configuration$model_controls)) {
    cli::cli_abort(
      c("{.arg configuration$model_controls} must be a {.cls data.frame} or {.code NULL}.",
        "x" = "Got {.cls {class(configuration$model_controls)}}."),
      class = "aeme_error_configuration"
    )
  }
  if (!is.logical(configuration$use_bgc)) {
    cli::cli_abort(
      c("{.arg configuration$use_bgc} must be {.cls logical}.",
        "x" = "Got {.cls {class(configuration$use_bgc)}}."),
      class = "aeme_error_configuration"
    )
  }
  for (model_cfg in c("dy_cd", "glm_aed", "gotm_wet")) {
    val <- configuration[[model_cfg]]
    if (!is.null(val) && !is.list(val)) {
      cli::cli_abort(
        c("{.arg configuration${model_cfg}} must be a {.cls list} or {.code NULL}.",
          "x" = "Got {.cls {class(val)}}."),
        class = "aeme_error_configuration"
      )
    }
  }
  
  # Observations type checking for specific elements
  if (!is.null(observations$level) && !is.data.frame(observations$level)) {
    cli::cli_abort(
      c("{.arg observations$level} must be a {.cls data.frame} or {.code NULL}.",
        "x" = "Got {.cls {class(observations$level)}}."),
      class = "aeme_error_observations"
    )
  }
  obs_col_names <- get_obs_column_names()
  if (!is.null(observations$lake)) {
    if (!is.data.frame(observations$lake)) {
      cli::cli_abort(
        c("{.arg observations$lake} must be a {.cls data.frame} or {.code NULL}.",
          "x" = "Got {.cls {class(observations$lake)}}."),
        class = "aeme_error_observations"
      )
    }
    missing_cols <- setdiff(obs_col_names, colnames(observations$lake))
    if (length(missing_cols) > 0) {
      cli::cli_abort(
        c("{.arg observations$lake} is missing required columns.",
          "x" = "Missing: {.val {missing_cols}}.",
          "i" = "Required columns are: {.val {obs_col_names}}."),
        class = "aeme_error_observations_cols"
      )
    }
  }
  
  # Input type checking for specific elements
  if (!is.null(input$init_profile) && !is.data.frame(input$init_profile)) {
    cli::cli_abort(
      c("{.arg input$init_profile} must be a {.cls data.frame} or {.code NULL}.",
        "x" = "Got {.cls {class(input$init_profile)}}."),
      class = "aeme_error_input"
    )
  }
  if (!is.numeric(input$init_depth)) {
    cli::cli_abort(
      c("{.arg input$init_depth} must be {.cls numeric}.",
        "x" = "Got {.cls {class(input$init_depth)}}."),
      class = "aeme_error_input"
    )
  }
  if (!is.null(input$hypsograph) && !is.data.frame(input$hypsograph)) {
    cli::cli_abort(
      c("{.arg input$hypsograph} must be a {.cls data.frame} or {.code NULL}.",
        "x" = "Got {.cls {class(input$hypsograph)}}."),
      class = "aeme_error_input"
    )
  }
  if (!is.null(input$meteo)) {
    if (!is.data.frame(input$meteo)) {
      cli::cli_abort(
        c("{.arg input$meteo} must be a {.cls data.frame} or {.code NULL}.",
          "x" = "Got {.cls {class(input$meteo)}}."),
        class = "aeme_error_input"
      )
    } else if (!is.POSIXct(input$meteo$Date) &&
               !lubridate::is.Date(input$meteo$Date)) {
      cli::cli_warn(
        c("!" = "{.arg input$meteo$Date} is not {.cls POSIXct} or {.cls Date}.",
          "i" = "Coercing to {.cls Date}. Supply a proper date column to avoid this."),
        class = "aeme_warn_meteo_date_coerced"
      )
      input$meteo$Date <- as.Date(input$meteo$Date)
      if (any(is.na(input$meteo$Date))) {
        cli::cli_abort(
          c("NAs introduced when coercing {.arg input$meteo$Date} to {.cls Date}.",
            "x" = "{sum(is.na(input$meteo$Date))} row(s) could not be parsed.",
            "i" = "Supply dates in {.code YYYY-MM-DD} format or as {.cls POSIXct}."),
          class = "aeme_error_meteo_date"
        )
      }
    }
  }
  if (!is.logical(input$use_lw)) {
    if (is.null(input$use_lw)) {
      cli::cli_inform(
        c("!" = "{.arg input$use_lw} is missing.",
          "i" = "Defaulting to {.code TRUE} (use longwave radiation)."),
        class = "aeme_inform_use_lw_default"
      )
      input$use_lw <- TRUE
    } else {
      cli::cli_abort(
        c("{.arg input$use_lw} must be {.cls logical}.",
          "x" = "Got {.cls {class(input$use_lw)}}."),
        class = "aeme_error_input"
      )
    }
  }
  if (!is.numeric(input$Kw)) {
    cli::cli_abort(
      c("{.arg input$Kw} must be {.cls numeric}.",
        "x" = "Got {.cls {class(input$Kw)}}."),
      class = "aeme_error_input"
    )
  }
  if (is.numeric(input$Kw) && input$Kw <= 0) {
    cli::cli_warn(
      c("!" = "{.arg input$Kw} {.val {input$Kw}} is <= 0.",
        "i" = "The light extinction coefficient should be a positive value (m^-1)."),
      class = "aeme_warn_kw"
    )
  }
  
  # Inflows type checking for specific elements
  if (!is.null(inflows$data)) {
    if (!is.list(inflows$data) || !all(sapply(inflows$data, is.data.frame))) {
      cli::cli_abort(
        c("{.arg inflows$data} must be a named {.cls list} of {.cls data.frame}s or {.code NULL}.",
          "x" = "Got {.cls {class(inflows$data)}}."),
        class = "aeme_error_inflows"
      )
    }
  }
  if (!is.list(inflows$factor)) {
    cli::cli_abort(
      c("{.arg inflows$factor} must be a {.cls list}.",
        "x" = "Got {.cls {class(inflows$factor)}}."),
      class = "aeme_error_inflows"
    )
  } else if (!all(sapply(inflows$factor, is.numeric))) {
    cli::cli_abort(
      c("All values in {.arg inflows$factor} must be {.cls numeric}.",
        "i" = "Expected a named list with entries for {.code dy_cd}, {.code glm_aed}, and {.code gotm_wet}."),
      class = "aeme_error_inflows"
    )
  }
  
  # Outflows type checking for specific elements
  if (!is.null(outflows$data)) {
    if (!is.list(outflows$data) || !all(sapply(outflows$data, is.data.frame))) {
      cli::cli_abort(
        c("{.arg outflows$data} must be a named {.cls list} of {.cls data.frame}s or {.code NULL}.",
          "x" = "Got {.cls {class(outflows$data)}}."),
        class = "aeme_error_outflows"
      )
    }
  }
  if (!is.list(outflows$factor)) {
    cli::cli_abort(
      c("{.arg outflows$factor} must be a {.cls list}.",
        "x" = "Got {.cls {class(outflows$factor)}}."),
      class = "aeme_error_outflows"
    )
  } else if (!all(sapply(outflows$factor, is.numeric))) {
    cli::cli_abort(
      c("All values in {.arg outflows$factor} must be {.cls numeric}.",
        "i" = "Expected a named list with entries for {.code dy_cd}, {.code glm_aed}, and {.code gotm_wet}."),
      class = "aeme_error_outflows"
    )
  }
  
  # Water balance type checking for specific elements
  if (!is.null(water_balance[["data"]][["model"]]) &&
      !is.data.frame(water_balance[["data"]][["model"]])) {
    cli::cli_abort(
      c("{.arg water_balance$data$model} must be a {.cls data.frame} or {.code NULL}.",
        "x" = "Got {.cls {class(water_balance[[\"data\"]][[\"model\"]])}}."),
      class = "aeme_error_water_balance"
    )
  }
  if (!is.numeric(water_balance$method)) {
    cli::cli_abort(
      c("{.arg water_balance$method} must be {.cls numeric}.",
        "x" = "Got {.cls {class(water_balance$method)}}.",
        "i" = "Accepted values: {.val {1L}} (none), {.val {2L}} (outflows), {.val {3L}} (inflows & outflows)."),
      class = "aeme_error_water_balance_method"
    )
  } else if (water_balance$method < 1 || water_balance$method > 3) {
    cli::cli_abort(
      c("{.arg water_balance$method} {.val {water_balance$method}} is not a valid method.",
        "i" = "Accepted values: {.val {1L}} (none), {.val {2L}} (outflows), {.val {3L}} (inflows & outflows)."),
      class = "aeme_error_water_balance_method"
    )
  }
  if (!is.null(water_balance$use)) {
    if (!is.character(water_balance$use)) {
      cli::cli_abort(
        c("{.arg water_balance$use} must be {.cls character}.",
          "x" = "Got {.cls {class(water_balance$use)}}.",
          "i" = "Accepted values: {.val {'obs'}} or {.val {'mod'}}."),
        class = "aeme_error_water_balance_use"
      )
    }
    if (!water_balance$use %in% c("obs", "mod")) {
      cli::cli_abort(
        c("{.arg water_balance$use} {.val {water_balance$use}} is not a valid option.",
          "i" = "Accepted values: {.val {'obs'}} (observations) or {.val {'mod'}} (modelled)."),
        class = "aeme_error_water_balance_use"
      )
    }
  }
  
  # Output type checking for specific elements
  if (!is.list(output)) {
    cli::cli_abort(
      c("{.arg output} must be a {.cls list}.",
        "x" = "Got {.cls {class(output)}}."),
      class = "aeme_error_output"
    )
  }
  
  # Parameters type checking for specific elements
  if (!is.data.frame(parameters)) {
    cli::cli_abort(
      c("{.arg parameters} must be a {.cls data.frame}.",
        "x" = "Got {.cls {class(parameters)}}."),
      class = "aeme_error_parameters"
    )
  }
  missing_params <- setdiff(param_names, names(parameters))
  if (length(missing_params) > 0) {
    cli::cli_abort(
      c("{.arg parameters} is missing required columns.",
        "x" = "Missing: {.val {missing_params}}."),
      class = "aeme_error_parameters_cols"
    )
  }
  
  
  new("Aeme",
      lake = lake,
      # catchment = catchment,
      time = time,
      configuration = configuration,
      observations = observations,
      input = input,
      inflows = inflows,
      outflows = outflows,
      water_balance = water_balance,
      output = output,
      parameters = parameters
  )
}

# Accessor (getter) functions ----
# Each generic is defined once, then a method is registered for "Aeme".
# Roxygen @rdname groups the generic and method docs into a single help page,
# avoiding duplication and keeping the exported API clean.

#' Access the lake slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of lake characteristics.
#' @rdname lake
#' @export
setGeneric("lake", function(aeme) standardGeneric("lake"))

#' @rdname lake
#' @export
setMethod("lake", "Aeme", function(aeme) aeme@lake)

#' Access the time slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of time characteristics.
#' @rdname time
#' @export
setGeneric("time", function(aeme) standardGeneric("time"))

#' @rdname time
#' @export
setMethod("time", "Aeme", function(aeme) aeme@time)

#' Access the configuration slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of configuration characteristics.
#' @rdname configuration
#' @export
setGeneric("configuration", function(aeme) standardGeneric("configuration"))

#' @rdname configuration
#' @export
setMethod("configuration", "Aeme", function(aeme) aeme@configuration)

#' Access the observations slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of observations characteristics.
#' @rdname observations
#' @export
setGeneric("observations", function(aeme) standardGeneric("observations"))

#' @rdname observations
#' @export
setMethod("observations", "Aeme", function(aeme) aeme@observations)

#' Access the input slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of input characteristics.
#' @rdname input
#' @export
setGeneric("input", function(aeme) standardGeneric("input"))

#' @rdname input
#' @export
setMethod("input", "Aeme", function(aeme) aeme@input)

#' Access the inflows slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of inflows characteristics.
#' @rdname inflows
#' @export
setGeneric("inflows", function(aeme) standardGeneric("inflows"))

#' @rdname inflows
#' @export
setMethod("inflows", "Aeme", function(aeme) aeme@inflows)

#' Access the outflows slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of outflows characteristics.
#' @rdname outflows
#' @export
setGeneric("outflows", function(aeme) standardGeneric("outflows"))

#' @rdname outflows
#' @export
setMethod("outflows", "Aeme", function(aeme) aeme@outflows)

#' Access the water_balance slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of water_balance characteristics.
#' @rdname water_balance
#' @export
setGeneric("water_balance", function(aeme) standardGeneric("water_balance"))

#' @rdname water_balance
#' @export
setMethod("water_balance", "Aeme", function(aeme) aeme@water_balance)

#' Access the output slot of an Aeme object
#' @param aeme An Aeme object.
#' @return List of output characteristics.
#' @rdname output
#' @export
setGeneric("output", function(aeme) standardGeneric("output"))

#' @rdname output
#' @export
setMethod("output", "Aeme", function(aeme) aeme@output)

#' Access the parameters slot of an Aeme object
#' @param aeme An Aeme object.
#' @return Dataframe of parameters.
#' @rdname parameters
#' @export
setGeneric("parameters", function(aeme) standardGeneric("parameters"))

#' @rdname parameters
#' @export
setMethod("parameters", "Aeme", function(aeme) aeme@parameters)


# Setter (replacement) functions ----
# Each setter validates its input before assigning, then calls methods::validObject()
# so that setValidity() rules are enforced. This means validation is never
# bypassed via direct @<- assignment by users.

#' Set the lake slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New lake list to assign.
#' @return A modified Aeme object with updated lake slot.
#' @rdname lake-set
#' @export
setGeneric("lake<-", function(aeme, value) standardGeneric("lake<-"))

#' @rdname lake-set
#' @export
setReplaceMethod("lake", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_lake_set"
    )
  aeme@lake <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg lake}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_lake_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the time slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New time list to assign.
#' @return A modified Aeme object with updated time slot.
#' @rdname time-set
#' @importFrom stats time
#' @export
setGeneric("time<-", function(aeme, value) standardGeneric("time<-"),
           package = "AEME")
#' @rdname time-set
#' @export
#' @importFrom methods validObject
setReplaceMethod("time", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_time_set"
    )
  aeme@time <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg time}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_time_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the configuration slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New configuration list to assign.
#' @return A modified Aeme object with updated configuration slot.
#' @rdname configuration-set
#' @export
setGeneric("configuration<-", function(aeme, value)
  standardGeneric("configuration<-"))

#' @rdname configuration-set
#' @export
setReplaceMethod("configuration", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_configuration_set"
    )
  aeme@configuration <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg configuration}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_configuration_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the observations slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New observations list to assign.
#' @return A modified Aeme object with updated observations slot.
#' @rdname observations-set
#' @export
setGeneric("observations<-", function(aeme, value)
  standardGeneric("observations<-"))

#' @rdname observations-set
#' @export
setReplaceMethod("observations", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_observations_set"
    )
  aeme@observations <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg observations}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_observations_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the input slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New input list to assign.
#' @return A modified Aeme object with updated input slot.
#' @rdname input-set
#' @export
setGeneric("input<-", function(aeme, value) standardGeneric("input<-"))

#' @rdname input-set
#' @export
setReplaceMethod("input", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_input_set"
    )
  aeme@input <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg input}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_input_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the inflows slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New inflows list to assign.
#' @return A modified Aeme object with updated inflows slot.
#' @rdname inflows-set
#' @export
setGeneric("inflows<-", function(aeme, value) standardGeneric("inflows<-"))

#' @rdname inflows-set
#' @export
setReplaceMethod("inflows", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_inflows_set"
    )
  aeme@inflows <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg inflows}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_inflows_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the outflows slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New outflows list to assign.
#' @return A modified Aeme object with updated outflows slot.
#' @rdname outflows-set
#' @export
setGeneric("outflows<-", function(aeme, value) standardGeneric("outflows<-"))

#' @rdname outflows-set
#' @export
setReplaceMethod("outflows", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_outflows_set"
    )
  aeme@outflows <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg outflows}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_outflows_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the water_balance slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New water_balance list to assign.
#' @return A modified Aeme object with updated water_balance slot.
#' @rdname water_balance-set
#' @export
setGeneric("water_balance<-", function(aeme, value) standardGeneric("water_balance<-"))

#' @rdname water_balance-set
#' @export
setReplaceMethod("water_balance", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}."),
      class = "aeme_error_water_balance_set"
    )
  if (!is.null(value$method) &&
      (!is.numeric(value$method) || value$method < 1 || value$method > 3))
    cli::cli_abort(
      c("{.arg value$method} must be 1, 2, or 3.",
        "x" = "Got {.val {value$method}}.",
        "i" = "1 = none, 2 = outflows, 3 = inflows & outflows."),
      class = "aeme_error_water_balance_method"
    )
  if (!is.null(value$use) && !value$use %in% c("obs", "mod"))
    cli::cli_abort(
      c("{.arg value$use} must be {.val {'obs'}} or {.val {'mod'}}.",
        "x" = "Got {.val {value$use}}."),
      class = "aeme_error_water_balance_use"
    )
  aeme@water_balance <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg water_balance}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_water_balance_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the output slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New output list to assign.
#' @return A modified Aeme object with updated output slot.
#' @rdname output-set
#' @export
setGeneric("output<-", function(aeme, value) standardGeneric("output<-"))

#' @rdname output-set
#' @export
setReplaceMethod("output", "Aeme", function(aeme, value) {
  if (!is.list(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls list}.",
        "x" = "Got {.cls {class(value)}}." ),
      class = "aeme_error_output_set"
    )
  aeme@output <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg output}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_output_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Set the parameters slot of an Aeme object
#' @param aeme An Aeme object.
#' @param value New parameters data.frame to assign.
#' @return A modified Aeme object with updated parameters slot.
#' @rdname parameters-set
#' @export
setGeneric("parameters<-", function(aeme, value) standardGeneric("parameters<-"))

#' @rdname parameters-set
#' @export
setReplaceMethod("parameters", "Aeme", function(aeme, value) {
  if (!is.data.frame(value))
    cli::cli_abort(
      c("{.arg value} must be a {.cls data.frame}.",
        "x" = "Got {.cls {class(value)}}."),
      class = "aeme_error_parameters_set"
    )
  aeme@parameters <- value
  tryCatch(
    methods::validObject(aeme),
    error = function(e) {
      cli::cli_abort(
        c("Invalid value for {.arg parameters}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_parameters_set",
        call = NULL
      )
    }
  )
  aeme
})

#' Show an Aeme object in the console
#'
#' This method prints the Aeme output in a readable format to the console.
#'
#' @title Print Aeme object to the console
#' @param object An Aeme object.
#' @return prints the Aeme object to the console.
#' @export
setMethod("show", "Aeme", function(object) {
  lke <- lake(object)
  aeme_time <- time(object)
  config <- configuration(object)
  obs <- observations(object)
  inp <- input(object)
  inf <- inflows(object)
  outf <- outflows(object)
  wbal <- water_balance(object)
  outp <- output(object)
  params <- parameters(object)
  
  n_dyresm <- as.vector(matrix(0, nrow = 1, ncol = outp$n_members))
  n_glm <- as.vector(matrix(0, nrow = 1, ncol = outp$n_members))
  n_gotm <- as.vector(matrix(0, nrow = 1, ncol = outp$n_members))
  if (outp$n_members > 0) {
    ens_names <- names(outp)[grepl("ens", names(outp))]
    for (i in 1:length(ens_names)) {
      n_dyresm[i] <- ifelse(!is.null(outp[[ens_names[i]]][["dy_cd"]]), 1, 0)
      n_glm[i] <- ifelse(!is.null(outp[[ens_names[i]]][["glm_aed"]]), 1, 0)
      n_gotm[i] <- ifelse(!is.null(outp[[ens_names[i]]][["gotm_wet"]]), 1, 0)
    }
  }
  
  cat(
    "\t\t\t   AEME ",
    paste0(
      "\n-------------------------------------------------------------------\n",
      "  Lake\n",
      lke$name, " (ID: ", lke$id), "); Lat: ",
    round(lke$latitude, 2), "; Lon: ", round(lke$longitude,
                                             2),
    "; Elev: ", round(lke$elevation, 2), "m; Depth: ",
    round(lke$depth, 2), "m;\nArea: ", round(lke$area, 2),
    " m2",
    "\n-------------------------------------------------------------------\n",
    "  Time\n",
    "Start: ", as.character(aeme_time$start),
    "; Stop: ", as.character(aeme_time$stop),
    "; Time step: ", as.character(aeme_time$time_step),
    "\n\tSpin up (days): GLM: ", aeme_time$spin_up$glm_aed, "; GOTM: ",
    aeme_time$spin_up$gotm_wet, "; DYRESM: ",
    aeme_time$spin_up$dy_cd,
    "\n-------------------------------------------------------------------\n",
    "  Configuration\n",
    "    Model controls: ", ifelse(is.null(config[["model_controls"]]),
                                   "Absent ", "Present"), "\n",
    "    Use biogeochemical model: ", ifelse(config[["use_bgc"]],
                                             "Yes ", "No"), "\n",
    "          Physical   |   Biogeochemical",
    "\nDY-CD    : ", ifelse(is.null(config[["dy_cd"]][["hydrodynamic"]]),
                            "Absent ", "Present"), "    |   ",
    ifelse(is.null(config[["dy_cd"]][["bgc"]]), "Absent ",
           "Present"),
    "\nGLM-AED  : ", ifelse(is.null(config[["glm_aed"]][["hydrodynamic"]]),
                            "Absent ", "Present"), "    |   ",
    ifelse(is.null(config[["glm_aed"]][["bgc"]]), "Absent ",
           "Present"),
    "\nGOTM-WET : ", ifelse(is.null(config[["gotm_wet"]][["hydrodynamic"]]),
                            "Absent ", "Present"), "    |   ",
    ifelse(is.null(config[["gotm_wet"]][["bgc"]]),
           "Absent ", "Present"),
    "\n-------------------------------------------------------------------\n",
    "  Observations\n",
    "Lake: ", ifelse(is.data.frame(obs$lake), "Present",
                     "Absent"),
    "; Level: ", ifelse(is.data.frame(obs$level), "Present",
                        "Absent"),
    "\n-------------------------------------------------------------------\n",
    "  Input\n",
    "Inital profile: ", ifelse(is.data.frame(inp$init_profile),
                               "Present", "Absent"),
    "; Inital depth: ", paste0(inp$init_depth, "m"),
    "; Hypsograph: ", ifelse(is.data.frame(inp$hypsograph),
                             "Present", "Absent"),
    ifelse(is.data.frame(inp$hypsograph),
           paste0(" (n=", nrow(inp$hypsograph), ")"), ""),
    ";\nMeteo: ", ifelse(is.data.frame(inp$meteo),
                         "Present", "Absent"),
    "; Use longwave: ", inp$use_lw,
    "; Kw: ", inp$Kw,
    "\n-------------------------------------------------------------------\n",
    "  Inflows\n",
    "Data: ", ifelse(length(inf$data) > 0,
                     "Present", "Absent"),
    "; Scaling factors: DY-CD: ", round(inf$factor$dy_cd, 2),
    "; GLM-AED: ", round(inf$factor$glm_aed, 2),
    "; GOTM-WET: ", round(inf$factor$gotm_wet, 2),
    "\n-------------------------------------------------------------------\n",
    "  Outflows\n",
    "Data: ", ifelse(length(outf$data) > 0,
                     "Present", "Absent"),
    "; Scaling factors: DY-CD: ", round(outf$factor$dy_cd, 2),
    "; GLM-AED: ", round(outf$factor$glm_aed, 2),
    "; GOTM-WET: ", round(outf$factor$gotm_wet, 2),
    "\n-------------------------------------------------------------------\n",
    "  Water balance\n",
    "Method: ", wbal$method, "; Use: ", wbal$use,"; Modelled: ",
    ifelse(!is.null(wbal[["data"]][["model"]]),
           "Present", "Absent"), "; Water balance: ",
    ifelse(is.data.frame(wbal[["data"]][["wbal"]]),
           "Present", "Absent"),
    "\n-------------------------------------------------------------------\n",
    "  Parameters: ", "\n",
    "Number of parameters: ", nrow(params),
    "\n-------------------------------------------------------------------\n",
    "  Output: ", "\n",
    "\nDY-CD:    ", paste(n_dyresm, collapse = " "),
    "\nGLM-AED:  ", paste(n_glm, collapse = " "),
    "\nGOTM-WET: ", paste(n_gotm, collapse = " "),
    sep = ""
  )
  return(invisible(object))
})

#' Summarise an Aeme object
#'
#' This method summarises the Aeme output.
#'
#' @title Summarise an Aeme object
#' @param object An Aeme object.
#' @return Aeme object.
#' @export
setMethod("summary", "Aeme", function(object) {
  aeme_summ <- summarise_aeme(object)
  return(aeme_summ)
})

#' Plot an Aeme object
#'
#' This method plots the Aeme object.
#'
#' @title Plot method for Aeme objects
#' @param x An Aeme object.
#' @param y An Aeme slot name (character). Defaults to \code{"output"}.
#' @param ... Additional arguments (currently unused).
#' @param add Logical; add to current plot? (currently unused)
#'
#' @importFrom sf st_transform st_geometry
#' @importFrom ggplot2 ggplot aes geom_sf geom_point geom_line labs ggtitle
#' @importFrom ggplot2 theme_bw facet_wrap
#' @importFrom patchwork wrap_plots
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join bind_rows filter contains
#' @importFrom rlang .data
#' @importFrom cli cli_abort
#' @importFrom utils data
#' @importFrom methods slot slotNames 
#'
#' @return A ggplot object, or prints to the active graphics device.
#' @export
setMethod("plot", "Aeme", function(x, y, ..., add = FALSE) {
  
  if (missing(y)) {
    y <- "output"
  }
  
  valid_slots <- methods::slotNames(x)
  if (!y %in% valid_slots) {
    cli::cli_abort(
      c("{.val {y}} is not a valid slot name.",
        "x" = "Valid slots are: {.val {valid_slots}}."),
      class = "aeme_error_plot_slot"
    )
  }
  
  obj <- methods::slot(x, y)
  
  if (is.list(obj) && all(sapply(obj, is.null))) {
    cli::cli_abort(
      c("The {.val {y}} slot contains no data.",
        "i" = "Run the appropriate setup or model step to populate this slot."),
      class = "aeme_error_plot_empty"
    )
  }
  
  if (y == "lake") {
    p <- ggplot2::ggplot()
    pnt <- data.frame(lat = obj$latitude, lon = obj$longitude) |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    p <- p + ggplot2::geom_sf(data = pnt) +
      ggplot2::labs(x = "Longitude", y = "Latitude",
                    title = paste0(obj$name, " (", obj$id,")"),
                    subtitle = paste0("Elevation: ", obj$elevation,
                                      "m; Depth: ", obj$depth, "m"))
    return(p)
  }
  
  if (y == "input") {
    
    # Load Rdata
    data("key_naming", package = "AEME", envir = environment())
    
    inp <- input(x)
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_line(data = inp$hypsograph, ggplot2::aes(x = area, y = elev)) +
      ggplot2::geom_point(data = inp$hypsograph, ggplot2::aes(x = area, y = elev)) +
      ggplot2::labs(x = "Area (m2)", y = "Elevation (m)") +
      ggplot2::ggtitle("Hypsograph") +
      ggplot2::theme_bw()
    
    p2 <- inp$meteo |>
      tidyr::pivot_longer(cols = !dplyr::contains("Date")) |>
      dplyr::left_join(key_naming[, c("name", "name_parse")], by = c("name" = "name")) |>
      dplyr::filter(!is.na(name_parse)) |>
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = Date, y = value)) +
      ggplot2::facet_wrap(~name_parse, scales = "free_y", labeller = ggplot2::label_parsed) +
      ggplot2::theme_bw()
    
    g <- p1 + p2 + patchwork::plot_layout(nrow = 1, widths = c(1, 4))
    return(g)
  }
  
  if (y == "observations") {
    
    if (!is.null(obj$lake) & !is.null(obj$level)) {
      p1 <- obj$lake |>
        dplyr::bind_rows(obj$level) |>
        dplyr::left_join(key_naming[, c("name", "name_parse")],
                         by = c("var_aeme" = "name")) |>
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = Date, y = value)) +
        ggplot2::labs(x = "Date", y = "Value") +
        ggplot2::facet_wrap(~name_parse, scales = "free_y",
                            labeller = ggplot2::label_parsed) +
        ggplot2::theme_bw()
    } else if (!is.null(obj$lake)) {
      p1 <- obj$lake |>
        dplyr::left_join(key_naming[, c("name", "name_parse")],
                         by = c("var_aeme" = "name")) |>
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = Date, y = value)) +
        ggplot2::labs(x = "Date", y = "Value") +
        ggplot2::facet_wrap(~name_parse, scales = "free_y",
                            labeller = ggplot2::label_parsed) +
        ggplot2::theme_bw()
    } else if (!is.null(obj$level)) {
      p1 <- obj$level |>
        dplyr::filter(var_aeme == "LKE_lvlwtr") |>
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = Date, y = value)) +
        ggplot2::labs(x = "Date", y = "Elevation (m)") +
        ggplot2::ggtitle("Lake level") +
        ggplot2::theme_bw()
    }
    return(p1)
  }
  
  if (y == "inflows" | y == "outflows") {
    
    df <- lapply(seq_along(obj$data), function(i) {
      cbind(obj$data[[i]], flow_name = names(obj$data)[i])
    }) |>
      dplyr::bind_rows() |>
      dplyr::select(-dplyr::any_of("model"))
    p1 <- df |>
      tidyr::pivot_longer(cols = -c("Date", "flow_name"), names_to = "var_aeme", values_to = "value") |>
      dplyr::left_join(key_naming[, c("name", "name_parse")], by = c("var_aeme" = "name")) |>
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = Date, y = value)) +
      ggplot2::facet_wrap(~name_parse, scales = "free_y",
                          labeller = ggplot2::label_parsed) +
      ggplot2::labs(x = "Date", y = "Value") +
      ggplot2::theme_bw()
    return(p1)
  }
  
  if (y == "water_balance") {
    obs <- observations(x)
    if (!is.null(obj$data$wbal)) {
      wbal <- obj$data$wbal
      level <- obs$level |>
        dplyr::filter(var_aeme == "LKE_lvlwtr" & Date %in% wbal$Date)
      p1 <- ggplot2::ggplot() +
        ggplot2::geom_line(data = wbal, ggplot2::aes(x = Date, y = value)) +
        ggplot2::labs(x = "Date", y = "Elevation (m)")
      if (nrow(level)) {
        p1 <- p1 +
          ggplot2::geom_point(data = level, ggplot2::aes(x = Date, y = value,
                                                         colour = "Obs"))
      }
      
      p2 <- wbal |>
        dplyr::select(Date, dplyr::contains("outflow")) |>
        tidyr::pivot_longer(cols = dplyr::contains("outflow"),
                            names_to = "Model", values_to = "value",
                            names_transform = list(Model = \(x)
                                                   gsub("outflow_", "", x))) |>
        ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(x = Date, y = value, col = Model)) +
        ggplot2::labs(x = "Date", y = "Outflow (m3/day)")
      
      p3 <- wbal |>
        dplyr::select(Date, dplyr::contains("evap_m3")) |>
        tidyr::pivot_longer(cols = dplyr::contains("evap_m3"),
                            names_to = "Model", values_to = "value",
                            names_transform = list(Model = \(x)
                                                   gsub("_evap_m3", "", x))) |>
        ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(x = Date, y = value, col = Model)) +
        ggplot2::labs(x = "Date", y = "Evaporation (m3/day)")
      
      g <- patchwork::wrap_plots(p1, p2, p3, ncol = 1, guides = "collect")
      return(g)
    }
  }
  
  if (y == "output") {
    ens_n <- 1
    model <- list_models(x)
    p1 <- plot_output(aeme = x, model = model, ens_n = ens_n)
    return(p1)
  }
})

#' Get names of an Aeme object
#'
#' Returns the names of all slots in the Aeme object.
#'
#' @title Names method for Aeme objects
#' @param x An Aeme object.
#' @return Character vector of slot names.
#' @export
setMethod("names", "Aeme", function(x) {
  methods::slotNames(x)
})

#' Get column names for the observational data frame
#'
#' @return Character vector of required column names for observational data.
#' @export
get_obs_column_names <- function() {
  c("Date", "var_aeme", "depth_from", "depth_to", "value")
}
