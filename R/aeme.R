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

# setValidity("Aeme", function(object) {
#   if (!is.list(lake)) {
#     "@lake must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(catchment)) {
#     "@catchment must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(time)) {
#     "@time must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(observations)) {
#     "@observations must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(input)) {
#     "@input must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(inflows)) {
#     "@inflows must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(outflows)) {
#     "@outflows must be a list"
#   } else {
#     TRUE
#   }
#   if (!is.list(output)) {
#     "@output must be a list"
#   } else {
#     TRUE
#   }
# })

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
    stop("Objects lake, time, and input must be provided.")
  }
  cfg_dflt <- list(
    model_controls = NULL,
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
      lvl = -1,
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
  param_names <- get_param_names()
  if (missing(parameters)) {
    parameters <- data.frame(matrix(nrow = 0, ncol = length(param_names)))
    colnames(parameters) <- param_names
  }

  # Validate the types before creating the object
  if (!is.list(lake) || !is.list(time) ||
      !is.list(configuration) || !is.list(observations) || !is.list(input) ||
      !is.list(inflows) || !is.list(outflows) || !is.list(water_balance) ||
      !is.list(output) || !is.data.frame(parameters)) {
    stop("All inputs must be lists or data frames.")
  }

  # Lake type checking for specific elements
  if (!is.character(lake$name)) {
    stop("Lake name must be a character.")
  }
  if (any(grepl("[^[:alnum:]]", lake$name))) {
    stop(strwrap(paste0("Lake name '", lake$name, "' contains non-alphanumeric
                          characters. Please remove these characters from the
                          lake name.")))
  }
  if (!is.character(lake$id)) {
    lake$id <- as.character(lake$id)
    if (is.na(lake$id)) {
      stop("Lake id must be a character.")
    }
  }
  if (any(grepl("[^[:alnum:]]", lake$id))) {
    stop(strwrap(paste0("Lake id '", lake$id, "' contains non-alphanumeric
                          characters. Please remove these characters from the
                          lake name.")))
  }
  if (!is.numeric(lake$latitude)) {
    stop("Lake latitude must be numeric.")
  }
  if (!is.numeric(lake$longitude)) {
    stop("Lake longitude must be numeric.")
  }
  if (!is.numeric(lake$elevation)) {
    stop("Lake elevation must be numeric.")
  }
  # if (!is(lake$shape, "sf") & !is.null(lake$shape)) {
  #   stop("Lake shape must be an 'sf' object or NULL.")
  # }
  if (!is.numeric(lake$depth)) {
    stop("Lake depth must be numeric.")
  }
  if (is.null(lake$area)) {
    stop("Lake area must be provided.")
  }
  # if (is(lake$shape, "sf") & is.null(lake$area)) {
  #   if (print) {
  #     message("Calculating lake area from lake shape.")
  #   }
  #   suppressMessages(sf::sf_use_s2(FALSE))
  #   lake$area <- sf::st_area(lake$shape) |>
  #     units::drop_units()
  #   if (print) {
  #     message(paste0("   ", round(lake$area, 2), " m2"))
  #   }
  #   suppressMessages(sf::sf_use_s2(TRUE))
  # }
  # if (!is.null(lake$shape) & !is.null(lake$area)) {
  #   suppressMessages(sf::sf_use_s2(FALSE))
  #   shape_area <- sf::st_area(lake$shape) |>
  #     units::drop_units() |>
  #     round(2)
  #   suppressMessages(sf::sf_use_s2(TRUE))
  #   if (abs(shape_area - lake$area) > 10) {
  #     warning(paste(strwrap(paste0("Lake area [", lake$area,
  #                                  " m2] is different to the area calculated from the lake
  #                    shape [", shape_area, " m2].")), collapse = "\n"))
  #   }
  # }
  if (!is.numeric(lake$area)) {
    stop(paste(strwrap("Lake area must be numeric."), collpse = "\n"))
  }

  # Catchment type checking for specific elements
  # if (!is.character(catchment$name)) {
  #   stop("Catchment name must be a character.")
  # }
  # if (!is(catchment$shape, "sf") & !is.null(catchment$shape)) {
  #   stop("Catchment shape must be an 'sf' object or NULL.")
  # }
  # if (!is.numeric(catchment$area) & !is.null(catchment$area)) {
  #   stop(paste(strwrap("Catchment area must be numeric or NULL. If NULL,
  #                      catchment shape needs to be provided."), collpse = "\n"))
  # }
  # if (is(catchment$shape, "sf") & is.null(catchment$area)) {
  #   message("Calculating catchment area from catchment shape:")
  #   suppressMessages(sf::sf_use_s2(FALSE))
  #   catchment$area <- sf::st_area(catchment$shape) |>
  #     units::drop_units()
  #   message(paste0("   ", round(catchment$area, 2), " m2"))
  #   suppressMessages(sf::sf_use_s2(TRUE))
  # }
  # if (!is(catchment$shape, "sf") & is.null(catchment$area)) {
  #   stop("Catchment area must be provided if no catchment shape is present.")
  # }

  # Time type checking for specific elements
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  if (is.character(time$start)) {
    time$start <- as.POSIXct(time$start, format = "%Y-%m-%d %H:%M:%S",
                             tz = "UTC")
  } else if(!is.POSIXct(time$start)) {
    stop("Time start must be POSIXct.")
  }
  if (is.character(time$stop)) {
    time$stop <- as.POSIXct(time$stop, format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")
  } else if(!is.POSIXct(time$stop)) {
    stop("Time stop must be POSIXct.")
  }
  if (time$stop <= time$start) {
    stop("Time stop must be greater than time start.")
  }
  if (is.null(time$time_step)) {
    if (print) {
      message(strwrap("Time step missing.\nSetting time step to 3600 seconds."))
    }
    time$time_step <- 3600
  }
  if (!is.numeric(time$time_step)) {
    stop("Time step must be numeric.")
  }
  if (!is.list(time$spin_up)) {
    if (is.null(time$spin_up)) {
      if (print) {
        message(strwrap("Spin up for models missing.\nSetting spin up to 2 for
                      all models."))
      }
      time$spin_up <- list(
        dy_cd = 2,
        glm_aed = 2,
        gotm_wet = 2
      )
    } else {
      stop("Time spin-up must be a list.")
    }
  } else if (all(!is.numeric(unlist(time$spin_up)))) {
    stop("Time spin-up for models must be numeric.")
  }

  # Configuration type checking for specific elements
  if (!is.null(configuration$model_controls)) {
    if (!is.data.frame(configuration$model_controls)) {
      stop("Configuration model_controls must be a dataframe or NULL.")
    }
  }
  if (!is.null(configuration$dy_cd)) {
    if (!is.list(configuration$dy_cd)) {
      stop("Configuration dy_cd must be a list or NULL.")
    }
  }
  if (!is.null(configuration$glm_aed)) {
    if (!is.list(configuration$glm_aed)) {
      stop("Configuration glm_aed must be a list or NULL.")
    }
  }
  if (!is.null(configuration$gotm_wet)) {
    if (!is.list(configuration$gotm_wet)) {
      stop("Configuration gotm_wet must be a list or NULL.")
    }
  }

  # Observations type checking for specific elements
  if (!is.null(observations$level)) {
    if (!is.data.frame(observations$level)) {
      stop("Observations level must be a dataframe or NULL.")
    }
  }
  obs_col_names <- get_obs_column_names()
  # if (is.null(observations$level)) {
  #   df <- matrix(NA, nrow = 1, ncol = length(obs_col_names)) |>
  #     as.data.frame()
  #   names(df) <- obs_col_names
  #   observations$level <- df |>
  #     dplyr::filter(!is.na(value))
  # }
  if (!is.null(observations$lake)) {
    if (!is.data.frame(observations$lake)) {
      stop("Observations lake must be a dataframe or NULL.")
    }
    obs_col_names <- get_obs_column_names()
    if (!all(obs_col_names %in% colnames(observations$lake))) {
      stop(paste("Observations lake must contain the following columns:",
                 paste(obs_col_names, collapse = ", ")))
    }
  }
  # if (is.null(observations$lake)) {
  #   df <- matrix(NA, nrow = 1, ncol = length(obs_col_names)) |>
  #     as.data.frame()
  #   names(df) <- obs_col_names
  #   observations$lake <- df |>
  #     dplyr::filter(!is.na(value))
  # }

  # Input type checking for specific elements
  if (!is.null(input$init_profile)) {
    if (!is.data.frame(input$init_profile)) {
      stop("Input inital temperature profile must be a dataframe or NULL.")
    }
  }
  if (!is.numeric(input$init_depth)) {
    stop("Lake inital depth must be numeric.")
  }
  if (!is.null(input$hypsograph)) {
    if (!is.data.frame(input$hypsograph)) {
      stop("Input hypsograph must be a dataframe or NULL.")
    }
  }
  if (!is.null(input$meteo)) {
    if (!is.data.frame(input$meteo)) {
      stop("Input meteo must be a dataframe or NULL.")
    } else {
      if (!is.POSIXct(input$meteo$Date) &
          !lubridate::is.Date(input$meteo$Date)) {
        if (print) {
          message(strwrap("Input meteo datetime is not in POSIXct/Date format.
                        Converting to 'Date' format."))
        }
        input$meteo$Date <- as.Date(input$meteo$Date)
        if (any(is.na(input$meteo$Date))) {
          stop(strwrap("NA's introduced when coercing to Date object. Input
                       meteo datetime is preferred in POSIXct/Date format."))
        }
      }
    }
  }
  if (!is.logical(input$use_lw)) {
    if (is.null(input$use_lw)) {
      if (print) {
        message(strwrap("Use longwave missing.\nSetting use longwave to TRUE."))
      }
      input$use_lw <- TRUE
    } else {
      stop("Input use longwave must be logical.")
    }
  }
  if (!is.numeric(input$Kw)) {
    stop("Input Kw must be numeric.")
  }

  # Inflows type checking for specific elements
  if (!is.null(inflows$data)) {
    if (!is.list(inflows$data)) {
      if (!all(sapply(inflows$data, is.data.frame))) {
        stop("Inflows data must be a list of dataframes or NULL.")
      }
    }
  }
  if (!is.list(inflows$factor)) {
    stop("Inflows factor must be a list.")
  } else if (all(!is.numeric(unlist(inflows$factor)))) {
    stop("Inflows factor for models must be numeric.")
  }

  # Outflows type checking for specific elements
  if (!is.null(outflows$data)) {
    if (!is.list(outflows$data)) {
      if (!all(sapply(outflows$data, is.data.frame))) {
        stop("Outflows data must be a list of dataframes or NULL.")
      }
    }
  }
  if (!is.list(outflows$factor)) {
    stop("Outflows factor must be a list.")
  } else if (all(!is.numeric(unlist(outflows$factor)))) {
    stop("Outflows factor for models must be numeric.")
  }

  # Water balance type checking for specific elements
  if (!is.list(water_balance)) {
    stop("Water balance must be a list of lists or NULL.")
  }
  if (!is.null(water_balance[["data"]][["model"]])) {
    if (!is.data.frame(water_balance[["data"]][["model"]])) {
      stop("Modelled water level must be a dataframe or NULL.")
    }
  }
  if (!is.numeric(water_balance$method)) {
    stop("Water balance method must be numeric. Accepted values are 1-3")
  } else if (water_balance$method < 1 | water_balance$method > 3) {
    stop(strwrap("Water balance method selected not available. Accepted values
                 are 1 (none), 2 (outflows) or 3 (inflows & outflows"))
  }
  if (!is.null(water_balance$use)) {
    if (!is.character(water_balance$use)) {
      stop("Water balance use must be a character of 'obs' or 'mod'.")
    }
    if (!water_balance$use %in% c("obs", "mod")) {
      stop(strwrap("Water balance use must be one of 'obs',
                   'mod'."))
    }
  }

  # Output type checking for specific elements
  if (!is.list(output)) {
    stop("Output must be a list of lists or NULL.")
  }

  # Parameters type checking for specific elements
  if (!is.data.frame(parameters)) {
    stop("Parameters must be a dataframe.")
  }
  if (!all(param_names %in% names(parameters))) {
    stop(strwrap(paste0("Parameters must have the following columns: ",
                        paste(param_names, collapse = ", "), ".")))
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

# Accessor functions ----

#' @title Access lake slot
#' @param aeme An Aeme object.
#' @return list of lake characteristics
#' @export
setGeneric("lake", function(aeme) standardGeneric("lake"))

#' @title Access lake slot
#' @param aeme An Aeme object.
#' @return list of lake characteristics
#' @export
setMethod("lake", "Aeme", function(aeme) aeme@lake)

#' @title Access time slot
#' @param aeme An Aeme object.
#' @return list of time characteristics
#' @export
setGeneric("time", function(aeme) standardGeneric("time"))

#' @title Access time slot
#' @param aeme An Aeme object.
#' @return list of time characteristics
#' @export
setMethod("time", "Aeme", function(aeme) aeme@time)

#' @title Access configuration slot
#' @param aeme An Aeme object.
#' @return list of configuration characteristics
#' @export
setGeneric("configuration", function(aeme) standardGeneric("configuration"))

#' @title Access configuration slot
#' @param aeme An Aeme object.
#' @return list of configuration characteristics
#' @export
setMethod("configuration", "Aeme", function(aeme) aeme@configuration)

#' @title Access observations slot
#' @param aeme An Aeme object.
#' @return list of observations characteristics
#' @export
setGeneric("observations", function(aeme) standardGeneric("observations"))

#' @title Access observations slot
#' @param aeme An Aeme object.
#' @return list of observations characteristics
#' @export
setMethod("observations", "Aeme", function(aeme) aeme@observations)

#' @title Access input slot
#' @param aeme An Aeme object.
#' @return list of input characteristics
#' @export
setGeneric("input", function(aeme) standardGeneric("input"))

#' @title Access input slot
#' @param aeme An Aeme object.
#' @return list of input characteristics
#' @export
setMethod("input", "Aeme", function(aeme) aeme@input)

#' @title Access inflows slot
#' @param aeme An Aeme object.
#' @return list of inflows characteristics
#' @export
setGeneric("inflows", function(aeme) standardGeneric("inflows"))

#' @title Access inflows slot
#' @param aeme An Aeme object.
#' @return list of inflows characteristics
#' @export
setMethod("inflows", "Aeme", function(aeme) aeme@inflows)

#' @title Access outflows slot
#' @param aeme An Aeme object.
#' @return list of outflows characteristics
#' @export
setGeneric("outflows", function(aeme) standardGeneric("outflows"))

#' @title Access outflows slot
#' @param aeme An Aeme object.
#' @return list of outflows characteristics
#' @export
setMethod("outflows", "Aeme", function(aeme) aeme@outflows)

#' @title Access water_balance slot
#' @param aeme An Aeme object.
#' @return list of water_balance characteristics
#' @export
setGeneric("water_balance", function(aeme) standardGeneric("water_balance"))

#' @title Access water_balance slot
#' @param aeme An Aeme object.
#' @return list of water_balance characteristics
#' @export
setMethod("water_balance", "Aeme", function(aeme) aeme@water_balance)


#' @title Access output slot
#' @param aeme An Aeme object.
#' @return list of output characteristics
#' @export
setGeneric("output", function(aeme) standardGeneric("output"))
#' @title Access output slot
#' @param aeme An Aeme object.
#' @return list of output characteristics
#' @export
setMethod("output", "Aeme", function(aeme) aeme@output)

#' @title Access parameters slot
#' @param aeme An Aeme object.
#' @return dataframe of parameters
#' @export
setGeneric("parameters", function(aeme) standardGeneric("parameters"))
#' @title Access parameters slot
#' @param aeme An Aeme object.
#' @return dataframe of parameters
#' @export
setMethod("parameters", "Aeme", function(aeme) aeme@parameters)


# Setter functions ----

#' Update the lake slot of an Aeme object
#'
#' @title Set lake in Aeme object
#' @param aeme An Aeme object.
#' @param value New lake data to be assigned.
#' @return A modified Aeme object with updated lake slot.
#' @export
setGeneric("lake<-", function(aeme, value) standardGeneric("lake<-"))

#' Update the lake slot of an Aeme object
#'
#' This method updates the "lake" slot of An Aeme object with new data.
#'
#' @title Set lake in Aeme object
#' @param aeme An Aeme object.
#' @param value New lake data to be assigned.
#' @return A modified Aeme object with updated lake slot.
#' @export
setMethod("lake<-", "Aeme", function(aeme, value) {
  aeme@lake <- value
  validObject(aeme)
  aeme
})

#' Update the time slot of an Aeme object
#'
#' @title Set time in Aeme object
#' @param aeme An Aeme object.
#' @param value New time data to be assigned.
#' @return A modified Aeme object with updated time slot.
#' @export
setGeneric("time<-", function(aeme, value) standardGeneric("time<-"))

#' Update the time slot of an Aeme object
#'
#' This method updates the "time" slot of An Aeme object with new data.
#'
#' @title Set time in Aeme object
#' @param aeme An Aeme object.
#' @param value New time data to be assigned.
#' @return A modified Aeme object with updated time slot.
#' @export
setMethod("time<-", "Aeme", function(aeme, value) {
  aeme@time <- value
  validObject(aeme)
  aeme
})

#' Update the configuration slot of an Aeme object
#'
#' @title Set configuration in Aeme object
#' @param aeme An Aeme object.
#' @param value New configuration data to be assigned.
#' @return A modified Aeme object with updated configuration slot.
#' @export
setGeneric("configuration<-", function(aeme, value)
  standardGeneric("configuration<-"))

#' Update the configuration slot of an aeme object
#'
#' This method updates the "configuration" slot of An Aeme object with new data.
#'
#' @title Set configuration in Aeme object
#' @param aeme An Aeme object.
#' @param value New configuration data to be assigned.
#' @return A modified Aeme object with updated configuration slot.
#' @export
setMethod("configuration<-", "Aeme", function(aeme, value) {
  aeme@configuration <- value
  validObject(aeme)
  aeme
})

#' Update the observations slot of an Aeme object
#'
#' @title Set observations in Aeme object
#' @param aeme An Aeme object.
#' @param value New observations data to be assigned.
#' @return A modified Aeme object with updated observations slot.
#' @export
setGeneric("observations<-", function(aeme, value)
  standardGeneric("observations<-"))

#' Update the observations slot of an Aeme object
#'
#' This method updates the "observations" slot of An Aeme object with new data.
#'
#' @title Set observations in Aeme object
#' @param aeme An Aeme object.
#' @param value New observations data to be assigned.
#' @return A modified Aeme object with updated observations slot.
#' @export
setMethod("observations<-", "Aeme", function(aeme, value) {
  aeme@observations <- value
  validObject(aeme)
  aeme
})

#' Update the input slot of an Aeme object
#'
#' @title Set input in Aeme object
#' @param aeme An Aeme object.
#' @param value New input data to be assigned.
#' @return A modified Aeme object with updated input slot.
#' @export
setGeneric("input<-", function(aeme, value) standardGeneric("input<-"))

#' Update the input slot of an Aeme object
#'
#' This method updates the "input" slot of An Aeme object with new data.
#'
#' @title Set input in Aeme object
#' @param aeme An Aeme object.
#' @param value New input data to be assigned.
#' @return A modified Aeme object with updated input slot.
#' @export
setMethod("input<-", "Aeme", function(aeme, value) {
  aeme@input <- value
  validObject(aeme)
  aeme
})

#' Update the inflows slot of an Aeme object
#'
#' @title Set inflows in Aeme object
#' @param aeme An Aeme object.
#' @param value New inflows data to be assigned.
#' @return A modified Aeme object with updated inflows slot.
#' @export
setGeneric("inflows<-", function(aeme, value) standardGeneric("inflows<-"))

#' Update the inflows slot of an Aeme object
#'
#' This method updates the "inflows" slot of An Aeme object with new data.
#'
#' @title Set inflows in Aeme object
#' @param aeme An Aeme object.
#' @param value New inflows data to be assigned.
#' @return A modified Aeme object with updated inflows slot.
#' @export
setMethod("inflows<-", "Aeme", function(aeme, value) {
  aeme@inflows <- value
  validObject(aeme)
  aeme
})

#' Update the outflows slot of an Aeme object
#'
#' @title Set outflows in Aeme object
#' @param aeme An Aeme object.
#' @param value New outflows data to be assigned.
#' @return A modified Aeme object with updated outflows slot.
#' @export
setGeneric("outflows<-", function(aeme, value) standardGeneric("outflows<-"))

#' Update the outflows slot of an Aeme object
#'
#' This method updates the "outflows" slot of An Aeme object with new data.
#'
#' @title Set outflows in Aeme object
#' @param aeme An Aeme object.
#' @param value New outflows data to be assigned.
#' @return A modified Aeme object with updated outflows slot.
#' @export
setMethod("outflows<-", "Aeme", function(aeme, value) {
  aeme@outflows <- value
  validObject(aeme)
  aeme
})

#' Update the water_balance slot of an Aeme object
#'
#' @title Set water_balance in Aeme object
#' @param aeme An Aeme object.
#' @param value New water_balance data to be assigned.
#' @return A modified Aeme object with updated water_balance slot.
#' @export
setGeneric("water_balance<-", function(aeme, value) standardGeneric("water_balance<-"))

#' Update the water_balance slot of an Aeme object
#'
#' This method updates the "water_balance" slot of An Aeme object with new data.
#'
#' @title Set water_balance in Aeme object
#' @param aeme An Aeme object.
#' @param value New water_balance data to be assigned.
#' @return A modified Aeme object with updated water_balance slot.
#' @export
setMethod("water_balance<-", "Aeme", function(aeme, value) {
  aeme@water_balance <- value
  validObject(aeme)
  aeme
})

#' Update the output slot of an Aeme object
#'
#' @title Set output in Aeme object
#' @param aeme An Aeme object.
#' @param value New output data to be assigned.
#' @return A modified Aeme object with updated output slot.
#' @export
setGeneric("output<-", function(aeme, value) standardGeneric("output<-"))

#' Update the output slot of an Aeme object
#'
#' This method updates the "output" slot of an Aeme object with new data.
#'
#' @title Set output in Aeme object
#' @param aeme An Aeme object.
#' @param value New output data to be assigned.
#' @return A modified Aeme object with updated output slot.
#' @export
setMethod("output<-", "Aeme", function(aeme, value) {
  aeme@output <- value
  validObject(aeme)
  aeme
})

#' Update the parameters slot of an Aeme object
#'
#' @title Set parameters in Aeme object
#' @param aeme An Aeme object.
#' @param value New parameters data to be assigned.
#' @return A modified Aeme object with updated parameters slot.
#' @export
setGeneric("parameters<-", function(aeme, value) standardGeneric("parameters<-"))

#' Update the parameters slot of an Aeme object
#'
#' This method updates the "parameters" slot of An Aeme object with new data.
#'
#' @title Set parameters in Aeme object
#' @param aeme An Aeme object.
#' @param value New parameters data to be assigned.
#' @return A modified Aeme object with updated parameters slot.
#' @export
setMethod("parameters<-", "Aeme", function(aeme, value) {
  aeme@parameters <- value
  validObject(aeme)
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
  # catchm <- catchment(object)
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
    # "  Catchment \n",
    # "Name: ", ifelse(is.null(catchm$name), NA, catchm$name),
    # "; Area: ", ifelse(is.null(catchm$area), NA, catchm$area), " m2;
    # Shape file: ", ifelse(is(catchm$shape, "sf"), "Present", "Absent"),
    # "\n-------------------------------------------------------------------\n",
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
    "          Physical   |   Biogeochemical",
    "\nDY-CD    : ", ifelse(is.null(config[["dy_cd"]][["hydrodynamic"]]),
                            "Absent ", "Present"), "    |   ",
    ifelse(is.null(config[["dy_cd"]][["ecosystem"]]), "Absent ",
           "Present"),
    "\nGLM-AED  : ", ifelse(is.null(config[["glm_aed"]][["hydrodynamic"]]),
                            "Absent ", "Present"), "    |   ",
    ifelse(is.null(config[["glm_aed"]][["ecosystem"]]), "Absent ",
           "Present"),
    "\nGOTM-WET : ", ifelse(is.null(config[["gotm_wet"]][["hydrodynamic"]]),
                            "Absent ", "Present"), "    |   ",
    ifelse(is.null(config[["gotm_wet"]][["ecosystem"]]),
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
    "Data: ", ifelse(is.list(inf$data),
                     "Present", "Absent"),
    "; Scaling factors: DY-CD: ", round(inf$factor$dy_cd, 2),
    "; GLM-AED: ", round(inf$factor$glm_aed, 2),
    "; GOTM-WET: ", round(inf$factor$gotm_wet, 2),
    "\n-------------------------------------------------------------------\n",
    "  Outflows\n",
    "Data: ", ifelse(is.list(outf$data),
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
    "Number of ensembles: ", outp$n_members,
    "\nDY-CD:    ", paste(n_dyresm, collapse = " "),
    "\nGLM-AED:  ", paste(n_glm, collapse = " "),
    "\nGOTM-WET: ", paste(n_gotm, collapse = " "),
    sep = ""
  )
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

  # lke <- lake(aeme_summ)
  # aeme_time <- time(aeme_summ)
  # cfg <- configuration(aeme_summ)
  # obs <- observations(aeme_summ)
  # inp <- input(aeme_summ)
  # inf <- inflows(aeme_summ)
  # outf <- outflows(aeme_summ)
  # wb <- water_balance(aeme_summ)
  # outp <- output(aeme_summ)
  # param <- parameters(aeme_summ)

  # Create an instance of the aemeSummary class
  # summary_object <- new("AemeSummary",
  #                       aeme = aeme_summ
  #                       )

  return(aeme_summ)

})

#' Plot an Aeme object
#'
#' This method plots the Aeme object.
#'
#' @title Update summary Method
#' @param x An Aeme object.
#' @param y An Aeme slot (optional). Defaults to "output".
#' @param ... additional arguments affecting the plot produced.
#' @param add logical; add to current plot?
#'
#' @importFrom sf st_transform st_geometry
#' @importFrom ggplot2 ggplot aes geom_sf geom_point geom_line labs ggtitle
#' theme_bw facet_wrap
#' @importFrom patchwork wrap_plots
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join bind_rows filter contains
#'
#' @return prints the Aeme object to the console.
#' @export
setMethod("plot", "Aeme", function(x, y, ..., add = FALSE) {

  if (missing(y)) {
    y <- "output"
  }

  if (!(y %in% slotNames(x))) {
    stop("'", y, "' is not a named slot in x. Options are:\n'",
         paste(slotNames(x), collapse = "', '"), "'.")
  }

  obj <- eval(parse(text = paste0(y, '(x)')))

  if (all(sapply(obj, is.null))) {
    stop("No data in '", y, "' slot in x.")
  }

  if (y == "lake") {
    p <- ggplot2::ggplot()
    # if (is(obj$shape, "sf")) {
    #   lake_shp <- obj$shape |>
    #     sf::st_transform(4326) |>
    #     sf::st_geometry()
    #   p <- p + ggplot2::geom_sf(data = lake_shp, fill = "lightblue")
    # }
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
    utils::data("key_naming", package = "AEME", envir = environment())

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

    if (!is.null(obj$lake) & !!is.null(obj$level)) {
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
    } else if (!is.null(obj$lake) ) {
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
      dplyr::bind_rows()
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


    if (!is.null(obj$data)) {
      par(mfrow = c(length(obj$data), 1))
      xlims <- range(do.call(c, lapply(obj$data, "[", "Date")), na.rm = TRUE) |>
        as.Date()
      for (i in seq_along(obj$data)) {
        if (y == "inflows") {
          vars <- names(obj$data[[i]])[-1]
          if (length(vars) > 5) {
            nc <- 2
            nr <- ceiling(length(vars) / nc)
          } else {
            nc <- 1
            nr <- length(vars)
          }
          par(mfrow = c(nr, nc),
              oma = c(5,4,0,0) + 0.1,
              mar = c(0,0,1,1) + 0.1)
          x <- obj$data[[i]]$Date
          for (v in seq_along(vars)) {
            sub <- obj$data[[i]][, c("Date", vars[v])]
            # x <- sub$Date
            base::plot(sub$Date, sub[[vars[v]]], axes = FALSE, type = "l")
            title(main = parse(text = rename_modelvars(vars[v])))
            axis(side = 2, labels = TRUE)
            box(which = "plot", bty = "l")
            if ((nc == 1 & v == nr) |
                (nc == 2 & v %in% c(length(vars), length(vars)-1))) {
              labels <- TRUE
            } else {
              labels <- FALSE
            }
            axis.Date(side = 1, at = seq(min(x), max(x), by = "6 months"),
                      x = x, labels = labels, format = "%m/%Y")
          }

        } else if (y == "outflows") {
          vars <- names(obj$data[[i]])[-1]
          x <- obj$data[[i]]$Date
          for (v in seq_along(vars)) {
            sub <- obj$data[[i]][, c("Date", vars[v])]
            # x <- sub$Date
            if (v == 1) {
              base::plot(sub$Date, sub[[vars[v]]], axes = FALSE, type = "l",
                         xlim = xlims)
              title(main = names(obj$data)[i])
              axis(side = 2, labels = TRUE)
              box(which = "plot", bty = "l")
              labels <- ifelse(i == length(obj$data), TRUE, FALSE)
              axis.Date(side = 1, at = seq(min(x), max(x), by = "6 months"),
                        x = x, labels = labels, format = "%m/%Y")
            } else {
              graphics::lines(sub$Date, sub[[vars[v]]], col = v)
            }
          }
        }
      }
    }
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

      par(mfrow = c(3, 1))
      ylim <- range(c(wbal$lvlwtr, level$value), na.rm = TRUE)
      plot(wbal$Date, wbal$lvlwtr, type = "l", axes = FALSE,
           ylab = "Water level (m)", ylim = ylim,
           xlab = "", main = "Water Level")
      axis(side = 2, labels = TRUE)
      axis.Date(side = 1, at = seq(min(wbal$Date), max(wbal$Date),
                                   by = "6 months"),
                x = wbal$Date, labels = FALSE, format = "%m/%Y")
      if (nrow(level)) {
        points(level$Date, level$value, pch = 16, col = "red")
      }

      ylim <- range(wbal[, c("outflow_gotm_wet", "outflow_glm_aed")],
                    na.rm = TRUE)
      plot(wbal$Date, wbal$outflow_gotm_wet, type = "l",
           ylab = "Discharge (m3)", ylim = ylim, axes = FALSE,
           xlab = "", main = "Estimated outflow")
      lines(wbal$Date, wbal$outflow_glm_aed, col = "red")
      axis(side = 2, labels = TRUE)
      axis.Date(side = 1, at = seq(min(wbal$Date), max(wbal$Date),
                                   by = "6 months"),
                x = wbal$Date, labels = FALSE, format = "%m/%Y")


      ylim <- range(wbal[, c("gotm_wet_evap_m3", "glm_aed_evap_m3")],
                    na.rm = TRUE)
      plot(wbal$Date, wbal$gotm_wet_evap_m3, type = "l",
           ylab = "Discharge (m3)", ylim = ylim, axes = FALSE,
           xlab = "Date", main = "Estimated evaporation")
      abline(h = 0)
      lines(wbal$Date, wbal$glm_aed_evap_m3, col = "red")
      axis(side = 2, labels = TRUE)
      axis.Date(side = 1, at = seq(min(wbal$Date), max(wbal$Date),
                                   by = "6 months"),
                x = wbal$Date, labels = TRUE, format = "%m/%Y")
    }
  }

  if (y == "output") {
    # inp <- input(x)
    ens_n <- 1
    ens_lab <- paste0("ens_", sprintf("%03d", ens_n))
    outp <- output(x)
    model <- names(outp[[ens_lab]])
    p1 <- plot_output(aeme = x, model = model, ens_n = ens_n)
    return(p1)

    obs <- observations(x)

    # z <- t(as.matrix(outp[[m]][["HYD_temp"]]))
    # Date = outp[[m]][["Date"]]
    # filled.contour(x = Date, z = z, color.palette = hcl.colors,
    #                key.title = title(main = "Temp\n(\u00B0C)"))

    mod <- lapply(names(outp), \(m) {
      if (!is.null(outp[[m]])) {
        depth <- outp[[m]][["LKE_lvlwtr"]]
        # lyr <- outp[[m]][["LAYERS"]]
        temp <- outp[[m]][["HYD_temp"]]
        df <- data.frame(Date = outp[[m]][["Date"]],
                         # depth = unlist(lyr),
                         Ts = unlist(temp[nrow(temp), ]),
                         Tb = unlist(temp[1, ]),
                         lvl = depth,
                         model = m)
        df
      }
    }) |>
      dplyr::bind_rows()

    if (!is.null(obs$lake)) {
      obs_temp <- obs$lake |>
        dplyr::filter(var == "HYD_temp" & Date %in% mod$Date) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(Ts = value[which.min(depth_from)],
                         Tb = value[which.max(depth_from)])
    }
    if (!is.null(obs$level)) {
      obs_lvl <- obs$level |>
        dplyr::filter(Date %in% mod$Date & var == "LKE_lvlwtr") |>
        dplyr::mutate(lvl_adj = value - min(inp$hypsograph$elev))
    }

    #
    par(mfrow = c(2, 1))
    ylim <- range(mod$Ts, mod$Tb, na.rm = TRUE)
    base::plot(mod$Date, mod$Ts, type = "n", ylab = "Temperature (\u00B0C)",
               xlab = "Time", main = "Water temperature", ylim = ylim)
    for (i in seq_along(outp)) {
      sub <- mod[mod$model == names(outp)[i], ]
      lines(sub$Date, sub$Ts, col = i)
      lines(sub$Date, sub$Tb, col = i, lty = 2)
    }
    if (!is.null(obs$lake)) {
      points(obs_temp$Date, obs_temp$Ts, pch = 1, cex = 0.75)
      points(obs_temp$Date, obs_temp$Tb, pch = 4, cex = 0.75)
    }
    legend("topright", legend = c("Surface", "Bottom", "Obs (surf)",
                                  "Obs (bott)"), lty = c(1:2, NA, NA),
           pch = c(NA, NA, 1, 4), col = "black", bg = "transparent")
    # legend("bottomright", legend = c(), pch = c(1, 4),
    #        col = "black", bg = "transparent")

    # Water level
    base::plot(mod$Date, mod$lvl, type = "n", ylab = "Water level (m)",
               xlab = "Time", main = "Water level")
    for (i in seq_along(outp)) {
      sub <- mod[mod$model == names(outp)[i], ]
      lines(sub$Date, sub$lvl, col = i)
    }
    if (!is.null(obs$level)) {
      points(obs_lvl$Date, obs_lvl$lvl_adj, pch = 20, cex = 0.5)
    }
    legend("bottomright", legend = c("DY-CD", "GLM-AED", "GOTM-WET", "Obs"),
           lty = c(rep(1, 3), NA), pch = c(rep(NA, 3), 20),
           col = c(1:3, 1), bg = "transparent")
  }
})

#' Get names of an Aeme object
#'
#' This method prints the names of the slots in the Aeme object.
#'
#' @title Update names Method
#' @param x An Aeme object.
#' @return vector of names of the slots in the Aeme object to the console.
#' @export
setMethod("names", "Aeme", function(x) {
  slotNames(x)
})

#' Get parameters data frame column names
#' @noRd

get_param_names <- function() {
  param_names <- c("model", "file", "name", "value", "min", "max", "module",
                   "group", "par", "logical", "logical_val", "char", "char_val")
  return(param_names)
}

#' Get column names for the observational data frame
#' @export
#'
#' @return vector of column names for the observational data frame
#'

get_obs_column_names <- function() {
  obs_column_names <- c("Date", "var_aeme", "depth_from", "depth_to", "value")
  return(obs_column_names)
}
