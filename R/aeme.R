#' S4 Class representing AEME data
#'
#' This class represents data related to a lake for running AEME.
#' @title aeme Class
#' @name aeme
#' @aliases aeme-class
#' @slot lake A list representing lake information.
#' @slot time A list representing time information.
#' @slot configuration A list representing each model's configuration.
#' @slot observations A list representing observation information.
#' @slot input A list representing input information.
#' @slot inflows A list representing inflows information.
#' @slot outflows A list representing outflows information.
#' @slot water_balance A list representing water balance information.
#' @slot output A list representing output information.
#' @importFrom methods new
#' @export

setClass("aeme",
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
           output = "list"
         )
)

# setValidity("aeme", function(object) {
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

#' Constructor function for aeme class
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
#' @return An instance of the aeme class.
#'
#' @importFrom sf st_area sf_use_s2
#' @importFrom units drop_units
#'
#' @export

aeme_constructor <- function(
    lake, time, configuration, observations,
    input, inflows, outflows, water_balance, output
) {

  # If missing arguments, create default list objects
  if (missing(lake) & missing(time) & missing(input)) {
    stop("Objects lake, time, and input must be provided.")
  }
  if (missing(configuration)) {
    configuration <- list(
      dy_cd = NULL,
      glm_aed = NULL,
      gotm_wet = NULL
    )
  }
  if (missing(observations)) {
    observations <- list(
      lake = NULL,
      level = NULL
    )
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
  if (missing(water_balance)) {
    water_balance <- list(
      use = "obs",
      data = list(
        model = NULL,
        wbal = NULL
      )
    )
  }
  if (missing(output)) {
    output <- list(
      dy_cd = NULL,
      glm_aed = NULL,
      gotm_wet = NULL
    )
  }

  # Validate the types before creating the object
  if (!is.list(lake) || !is.list(time) ||
      !is.list(configuration) || !is.list(observations) || !is.list(input) ||
      !is.list(inflows) || !is.list(outflows) || !is.list(water_balance) ||
      !is.list(output)) {
    stop("All inputs must be lists.")
  }

  # Lake type checking for specific elements
  if (!is.character(lake$name)) {
    stop("Lake name must be a character.")
  }
  if (!is.numeric(lake$id)) {
    stop("Lake id must be numeric.")
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
  if (!is(lake$shape, "sf") & !is.null(lake$shape)) {
    stop("Lake shape must be an 'sf' object or NULL.")
  }
  if (!is.numeric(lake$depth)) {
    stop("Lake depth must be numeric.")
  }
  if (is.null(lake$shape) & is.null(lake$area)) {
    stop("Lake shape or area must be provided.")
  }
  if (is(lake$shape, "sf") & is.null(lake$area)) {
    message("Calculating lake area from lake shape.")
    suppressMessages(sf::sf_use_s2(FALSE))
    lake$area <- sf::st_area(lake$shape) |>
      units::drop_units()
    message(paste0("   ", round(lake$area, 2), " m2"))
    suppressMessages(sf::sf_use_s2(TRUE))
  }
  if (!is.null(lake$shape) & !is.null(lake$area)) {
    suppressMessages(sf::sf_use_s2(FALSE))
    shape_area <- sf::st_area(lake$shape) |>
      units::drop_units() |>
      round(2)
    suppressMessages(sf::sf_use_s2(TRUE))
    if (abs(shape_area - lake$area) > 10) {
      warning(paste(strwrap(paste0("Lake area [", lake$area,
                     " m2] is different to the area calculated from the lake
                     shape [", shape_area, " m2].")), collapse = "\n"))
    }
  }
  if (!is.numeric(lake$area) & !is.null(lake$area)) {
    stop(paste(strwrap("Lake area must be numeric or NULL. If NULL,
                       lake shape needs to be provided."), collpse = "\n"))
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

  if (!is.numeric(time$time_step)) {
    stop("Time step must be numeric.")
  }
  if (!is.list(time$spin_up)) {
    if (is.null(time$spin_up)) {
      message(strwrap("Spin up for models missing.\nSetting spin up to 0 for
                      all models."))
      time$spin_up <- list(
        dy_cd = 0,
        glm_aed = 0,
        gotm_wet = 0
      )
    } else {
      stop("Time spin-up must be a list.")
    }
  } else if (all(!is.numeric(unlist(time$spin_up)))) {
    stop("Time spin-up for models must be numeric.")
  }

  # Configuration type checking for specific elements
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
  if (!is.null(observations$lake)) {
    if (!is.data.frame(observations$lake)) {
      stop("Observations lake must be a dataframe or NULL.")
    }
  }

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
        message(strwrap("Input meteo datetime is not in POSIXct/Date format.
                        Converting to 'Date' format."))
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
      message(strwrap("Use longwave missing.\nSetting use longwave to TRUE."))
      input$use_lw <- TRUE
    } else {
      stop("Input use longwave must be boolean.")
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


  new("aeme",
      lake = lake,
      # catchment = catchment,
      time = time,
      configuration = configuration,
      observations = observations,
      input = input,
      inflows = inflows,
      outflows = outflows,
      water_balance = water_balance,
      output = output
  )
}

# Accessor functions ----

#' @title Access lake slot
#' @param aeme A aeme object.
#' @return list of lake characteristics
#' @export
setGeneric("lake", function(aeme) standardGeneric("lake"))

#' @title Access lake slot
#' @param aeme A aeme object.
#' @return list of lake characteristics
#' @export
setMethod("lake", "aeme", function(aeme) aeme@lake)

#' @title Access time slot
#' @param aeme A aeme object.
#' @return list of time characteristics
#' @export
setGeneric("time", function(aeme) standardGeneric("time"))

#' @title Access time slot
#' @param aeme A aeme object.
#' @return list of time characteristics
#' @export
setMethod("time", "aeme", function(aeme) aeme@time)

#' @title Access configuration slot
#' @param aeme A aeme object.
#' @return list of configuration characteristics
#' @export
setGeneric("configuration", function(aeme) standardGeneric("configuration"))

#' @title Access configuration slot
#' @param aeme A aeme object.
#' @return list of configuration characteristics
#' @export
setMethod("configuration", "aeme", function(aeme) aeme@configuration)

#' @title Access observations slot
#' @param aeme A aeme object.
#' @return list of observations characteristics
#' @export
setGeneric("observations", function(aeme) standardGeneric("observations"))

#' @title Access observations slot
#' @param aeme A aeme object.
#' @return list of observations characteristics
#' @export
setMethod("observations", "aeme", function(aeme) aeme@observations)

#' @title Access input slot
#' @param aeme A aeme object.
#' @return list of input characteristics
#' @export
setGeneric("input", function(aeme) standardGeneric("input"))

#' @title Access input slot
#' @param aeme A aeme object.
#' @return list of input characteristics
#' @export
setMethod("input", "aeme", function(aeme) aeme@input)

#' @title Access inflows slot
#' @param aeme A aeme object.
#' @return list of inflows characteristics
#' @export
setGeneric("inflows", function(aeme) standardGeneric("inflows"))

#' @title Access inflows slot
#' @param aeme A aeme object.
#' @return list of inflows characteristics
#' @export
setMethod("inflows", "aeme", function(aeme) aeme@inflows)

#' @title Access outflows slot
#' @param aeme A aeme object.
#' @return list of outflows characteristics
#' @export
setGeneric("outflows", function(aeme) standardGeneric("outflows"))

#' @title Access outflows slot
#' @param aeme A aeme object.
#' @return list of outflows characteristics
#' @export
setMethod("outflows", "aeme", function(aeme) aeme@outflows)

#' @title Access water_balance slot
#' @param aeme A aeme object.
#' @return list of water_balance characteristics
#' @export
setGeneric("water_balance", function(aeme) standardGeneric("water_balance"))

#' @title Access water_balance slot
#' @param aeme A aeme object.
#' @return list of water_balance characteristics
#' @export
setMethod("water_balance", "aeme", function(aeme) aeme@water_balance)


#' @title Access output slot
#' @param aeme A aeme object.
#' @return list of output characteristics
#' @export
setGeneric("output", function(aeme) standardGeneric("output"))
#' @title Access output slot
#' @param aeme A aeme object.
#' @return list of output characteristics
#' @export
setMethod("output", "aeme", function(aeme) aeme@output)

# Setter functions ----

#' Update the lake slot of an aeme object
#'
#' @title Set lake in aeme object
#' @param aeme A aeme object.
#' @param value New lake data to be assigned.
#' @return A modified aeme object with updated lake slot.
#' @export
setGeneric("lake<-", function(aeme, value) standardGeneric("lake<-"))

#' Update the lake slot of an aeme object
#'
#' This method updates the "lake" slot of a aeme object with new data.
#'
#' @title Set lake in aeme object
#' @param aeme An aeme object.
#' @param value New lake data to be assigned.
#' @return A modified aeme object with updated lake slot.
#' @export
setMethod("lake<-", "aeme", function(aeme, value) {
  aeme@lake <- value
  validObject(aeme)
  aeme
})

#' Update the time slot of an aeme object
#'
#' @title Set time in aeme object
#' @param aeme A aeme object.
#' @param value New time data to be assigned.
#' @return A modified aeme object with updated time slot.
#' @export
setGeneric("time<-", function(aeme, value) standardGeneric("time<-"))

#' Update the time slot of an aeme object
#'
#' This method updates the "time" slot of a aeme object with new data.
#'
#' @title Set time in aeme object
#' @param aeme An aeme object.
#' @param value New time data to be assigned.
#' @return A modified aeme object with updated time slot.
#' @export
setMethod("time<-", "aeme", function(aeme, value) {
  aeme@time <- value
  validObject(aeme)
  aeme
})

#' Update the configuration slot of an aeme object
#'
#' @title Set configuration in aeme object
#' @param aeme A aeme object.
#' @param value New configuration data to be assigned.
#' @return A modified aeme object with updated configuration slot.
#' @export
setGeneric("configuration<-", function(aeme, value)
  standardGeneric("configuration<-"))

#' Update the configuration slot of an aeme object
#'
#' This method updates the "configuration" slot of a aeme object with new data.
#'
#' @title Set configuration in aeme object
#' @param aeme An aeme object.
#' @param value New configuration data to be assigned.
#' @return A modified aeme object with updated configuration slot.
#' @export
setMethod("configuration<-", "aeme", function(aeme, value) {
  aeme@configuration <- value
  validObject(aeme)
  aeme
})

#' Update the observations slot of an aeme object
#'
#' @title Set observations in aeme object
#' @param aeme A aeme object.
#' @param value New observations data to be assigned.
#' @return A modified aeme object with updated observations slot.
#' @export
setGeneric("observations<-", function(aeme, value)
  standardGeneric("observations<-"))

#' Update the observations slot of an aeme object
#'
#' This method updates the "observations" slot of a aeme object with new data.
#'
#' @title Set observations in aeme object
#' @param aeme An aeme object.
#' @param value New observations data to be assigned.
#' @return A modified aeme object with updated observations slot.
#' @export
setMethod("observations<-", "aeme", function(aeme, value) {
  aeme@observations <- value
  validObject(aeme)
  aeme
})

#' Update the input slot of an aeme object
#'
#' @title Set input in aeme object
#' @param aeme A aeme object.
#' @param value New input data to be assigned.
#' @return A modified aeme object with updated input slot.
#' @export
setGeneric("input<-", function(aeme, value) standardGeneric("input<-"))

#' Update the input slot of an aeme object
#'
#' This method updates the "input" slot of a aeme object with new data.
#'
#' @title Set input in aeme object
#' @param aeme An aeme object.
#' @param value New input data to be assigned.
#' @return A modified aeme object with updated input slot.
#' @export
setMethod("input<-", "aeme", function(aeme, value) {
  aeme@input <- value
  validObject(aeme)
  aeme
})

#' Update the inflows slot of an aeme object
#'
#' @title Set inflows in aeme object
#' @param aeme A aeme object.
#' @param value New inflows data to be assigned.
#' @return A modified aeme object with updated inflows slot.
#' @export
setGeneric("inflows<-", function(aeme, value) standardGeneric("inflows<-"))

#' Update the inflows slot of an aeme object
#'
#' This method updates the "inflows" slot of a aeme object with new data.
#'
#' @title Set inflows in aeme object
#' @param aeme An aeme object.
#' @param value New inflows data to be assigned.
#' @return A modified aeme object with updated inflows slot.
#' @export
setMethod("inflows<-", "aeme", function(aeme, value) {
  aeme@inflows <- value
  validObject(aeme)
  aeme
})

#' Update the outflows slot of an aeme object
#'
#' @title Set outflows in aeme object
#' @param aeme A aeme object.
#' @param value New outflows data to be assigned.
#' @return A modified aeme object with updated outflows slot.
#' @export
setGeneric("outflows<-", function(aeme, value) standardGeneric("outflows<-"))

#' Update the outflows slot of an aeme object
#'
#' This method updates the "outflows" slot of a aeme object with new data.
#'
#' @title Set outflows in aeme object
#' @param aeme An aeme object.
#' @param value New outflows data to be assigned.
#' @return A modified aeme object with updated outflows slot.
#' @export
setMethod("outflows<-", "aeme", function(aeme, value) {
  aeme@outflows <- value
  validObject(aeme)
  aeme
})

#' Update the water_balance slot of an aeme object
#'
#' @title Set water_balance in aeme object
#' @param aeme A aeme object.
#' @param value New water_balance data to be assigned.
#' @return A modified aeme object with updated water_balance slot.
#' @export
setGeneric("water_balance<-", function(aeme, value) standardGeneric("water_balance<-"))

#' Update the water_balance slot of an aeme object
#'
#' This method updates the "water_balance" slot of a aeme object with new data.
#'
#' @title Set water_balance in aeme object
#' @param aeme An aeme object.
#' @param value New water_balance data to be assigned.
#' @return A modified aeme object with updated water_balance slot.
#' @export
setMethod("water_balance<-", "aeme", function(aeme, value) {
  aeme@water_balance <- value
  validObject(aeme)
  aeme
})

#' Update the output slot of an aeme object
#'
#' @title Set output in aeme object
#' @param aeme A aeme object.
#' @param value New output data to be assigned.
#' @return A modified aeme object with updated output slot.
#' @export
setGeneric("output<-", function(aeme, value) standardGeneric("output<-"))

#' Update the output slot of an aeme object
#'
#' This method updates the "output" slot of a aeme object with new data.
#'
#' @title Set output in aeme object
#' @param aeme An aeme object.
#' @param value New output data to be assigned.
#' @return A modified aeme object with updated output slot.
#' @export
setMethod("output<-", "aeme", function(aeme, value) {
  aeme@output <- value
  validObject(aeme)
  aeme
})

#' Show an aeme object in the console
#'
#' This method prints the aeme output in a readable format to the console.
#'
#' @title Print aeme object to the console
#' @param object An aeme object.
#' @return prints the aeme object to the console.
#' @export
setMethod("show", "aeme", function(object) {
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
      " m2; Shape file: ", ifelse(is(lke$shape, "sf"), "Present",
                                 "Absent"),
      "\n-------------------------------------------------------------------\n",
      # "  Catchment \n",
      # "Name: ", ifelse(is.null(catchm$name), NA, catchm$name),
      # "; Area: ", ifelse(is.null(catchm$area), NA, catchm$area), " m2;
      # Shape file: ", ifelse(is(catchm$shape, "sf"), "Present", "Absent"),
      # "\n-------------------------------------------------------------------\n",
      "  Time\n",
      "Start: ", as.character(aeme_time$start),
      " Stop: ", as.character(aeme_time$stop),
      " Time step: ", as.character(aeme_time$time_step),
      "\n-------------------------------------------------------------------\n",
      "  Configuration\n",
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
      "  Outflows\n",
      "Data: ", ifelse(is.list(outf$data),
                       "Present", "Absent"),
      "; Scaling factors: DY-CD: ", round(outf$factor$dy_cd, 2),
      "; GLM-AED: ", round(outf$factor$glm_aed, 2),
      "; GOTM-WET: ", round(outf$factor$gotm_wet, 2),
      "\n-------------------------------------------------------------------\n",
      "  Water balance\n",
      "Use: ", wbal$use,"; Modelled: ", ifelse(!is.null(wbal[["data"]][["model"]]),
                       "Present", "Absent"), "; Water balance: ",
      ifelse(is.data.frame(wbal[["data"]][["wbal"]]),
                       "Present", "Absent"),
      "\n-------------------------------------------------------------------\n",
      "  Output: ", "\n",
      "DY-CD: ", ifelse(is.null(outp$dy_cd), "Absent", "Present"),
      "\nGLM-AED: ", ifelse(is.null(outp$glm_aed), "Absent",
                            "Present"),
      "\nGOTM-WET: ", ifelse(is.null(outp$gotm_wet), "Absent",
                             "Present"),
      sep = ""
  )
})

#' Summarise an aeme object
#'
#' This method summarises the aeme output.
#'
#' @title Summarise an aeme object
#' @param object An aeme object.
#' @param ... additional arguments affecting the summary produced.
#' @return prints the aeme object to the console.
#' @export
setMethod("summary", "aeme", function(object, ...) {

  obs <- observations(object)
  inp <- input(object)
  inf <- inflows(object)
  outf <- outflows(object)
  outp <- output(object)

  cat("Lake observations:\n")
  print(summary(obs$lake))
  cat("-------------------------------------------------------------------\n")
  cat("Lake level:\n")
  print(summary(obs$level))
  cat("-------------------------------------------------------------------\n")
  cat("Meteorology:\n")
  print(summary(inp$meteo))
  cat("-------------------------------------------------------------------\n")
  cat("Inflows:\n")
  for (i in seq_along(inf$data)) {
    cat("  ", names(inf$data)[i], "\n")
    print(summary(inf$data[[i]]))
  }
  cat("-------------------------------------------------------------------\n")
  cat("Outflows:\n")
  for (i in seq_along(outf$data)) {
    cat("  ", names(outf$data)[i], "\n")
    print(summary(outf$data[[i]]))
  }
  cat("-------------------------------------------------------------------\n")
  cat("Outputs:\n")
  for (m in seq_along(outp)) {
    cat("  ", toupper(gsub("_", "-", names(outp)[m])), "\n")
    if (!is.null(outp[[m]])) {

      temp <- outp[[m]][["HYD_temp"]]
      lst <- lapply(names(outp[[m]]), \(v) {
        if (length(dim(outp[[m]][[v]])) == 1) {
          rep(outp[[m]][[v]], each = nrow(temp))
        } else if (length(dim(outp[[m]][[v]])) == 2) {
          unlist(outp[[m]][[v]])
        }
      })
      names(lst) <- names(outp[[m]])
      df <- do.call(cbind, lst)
      print(summary(df))
    } else {
      print(summary(outp[[m]]))
    }
  }
  cat("-------------------------------------------------------------------\n")
})

#' Plot an aeme object
#'
#' This method plots the aeme object.
#'
#' @title Update summary Method
#' @param x An aeme object.
#' @param y An aeme slot (optional). Defaults to "output".
#' @param ... additional arguments affecting the plot produced.
#' @param add logical; add to current plot?
#'
#' @importFrom sf st_transform st_geometry
#'
#' @return prints the aeme object to the console.
#' @export
setMethod("plot", "aeme", function(x, y, ..., add = FALSE) {

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
    if (is(obj$shape, "sf")) {
      obj$shape |>
        sf::st_transform(4326) |>
        sf::st_geometry() |>
        base::plot(axes = TRUE, col = "cyan", add = add)
      add <- TRUE
    }
    data.frame(lat = obj$latitude, lon = obj$longitude) |>
      sf::st_as_sf(coords = c("lon", "lat")) |>
      base::plot(pch = 16, add = add, axes = TRUE)
    title(main = paste0(obj$name, "_", obj$id),
          sub = paste0("Elevation: ", obj$elevation, "m; Depth: ",
                       obj$depth, "m"))
  }

  # if (y == "catchment") {
  #   if (is(obj$shape, "sf")) {
  #     obj$shape |>
  #       sf::st_transform(4326) |>
  #       sf::st_geometry() |>
  #       base::plot(axes = TRUE, col = "#F2F2F2", add = add)
  #     add <- TRUE
  #   }
  #   if (is(obj$stream, "sf")) {
  #     obj$stream |>
  #       sf::st_geometry() |>
  #       base::plot(axes = TRUE, col = "blue", add = add)
  #     add <- TRUE
  #   }
  # }

  if (y == "observations") {

    if (!is.null(obj$lake)) {
      vars <- unique(obj$lake$var)
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
      x <- obj$lake$Date
      xlim <- range(obj$lake$Date)

      for (i in seq_along(vars)) {
        sub <- obj$lake[obj$lake$var == vars[i], ]
        # x <- sub$Date
        base::plot(sub$Date, sub$value, axes = FALSE, xlim = xlim, type = "n")
        deps <- unique(sub$depth_from)
        for (d in seq_along(deps)) {
          sub2 <- sub[sub$depth_from == deps[d], ]
          points(sub2$Date, sub2$value, pch = d)
        }

        title(main = parse(text = rename_modelvars(vars[i])))
        axis(side = 2, labels = TRUE)
        box(which = "plot", bty = "l")
        if ((nc == 1 & i == nr) |
            (nc == 2 & i %in% c(length(vars), length(vars)-1))) {
          labels <- TRUE
        } else {
          labels <- FALSE
        }
        axis.Date(side = 1, at = seq(min(x), max(x), by = "6 months"),
                  x = x, labels = labels, format = "%m/%Y")
      }
    }

    if (!is.null(obj$level)) {
      par(mfrow = c(1, 1))
      base::plot(obj$level$Date, obj$level$lvlwtr, ylab = "Elevation (m)",
           xlab = "Date", main = "Lake level")
    }
  }

  if (y == "inflows" | y == "outflows") {
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
      par(mfrow = c(3, 1))
      ylim <- range(c(wbal$lvlwtr, obs[["level"]][["lvlwtr"]]), na.rm = TRUE)
      plot(wbal$Date, wbal$lvlwtr, type = "l", axes = FALSE,
                 ylab = "Water level (m)", ylim = ylim,
                 xlab = "", main = "Water Level")
      axis(side = 2, labels = TRUE)
      axis.Date(side = 1, at = seq(min(wbal$Date), max(wbal$Date),
                                   by = "6 months"),
                x = wbal$Date, labels = FALSE, format = "%m/%Y")
      if (!is.null(obs$level)) {
        points(obs$level$Date, obs$level$lvlwtr, pch = 16, col = "red")
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
    inp <- input(x)
    outp <- output(x)
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
      do.call(rbind, args = _)

    if (!is.null(obs$lake)) {
      obs_temp <- obs$lake |>
        dplyr::filter(var == "HYD_temp" & Date %in% mod$Date) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(Ts = value[which.min(depth_from)],
                         Tb = value[which.max(depth_from)])
    }
    if (!is.null(obs$level)) {
      obs_lvl <- obs$level |>
        dplyr::filter(Date %in% mod$Date) |>
        dplyr::mutate(lvl_adj = lvlwtr - min(inp$hypsograph$elev))
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

#' Get names of an aeme object
#'
#' This method prints the names of the slots in the aeme object.
#'
#' @title Update names Method
#' @param x An aeme object.
#' @return vector of names of the slots in the aeme object to the console.
#' @export
setMethod("names", "aeme", function(x) {
  slotNames(x)
})

