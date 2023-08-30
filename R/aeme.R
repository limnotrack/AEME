#' S4 Class representing AEME data
#'
#' This class represents data related to a lake for running AEME.
#' @title aeme Class
#' @name aeme
#' @aliases aeme-class
#' @slot lake A list representing lake information.
#' @slot catchment A list representing catchment information.
#' @slot time A list representing time information.
#' @slot configuration A list representing each model's configuration.
#' @slot observations A list representing observation information.
#' @slot input A list representing input information.
#' @slot inflows A list representing inflows information.
#' @slot outflows A list representing outflows information.
#' @slot output A list representing output information.
#' @importFrom methods new
#' @export

setClass("aeme",
         representation(
           lake = "list",
           catchment = "list",
           time = "list",
           configuration = "list",
           observations = "list",
           input = "list",
           inflows = "list",
           outflows = "list",
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
#' @param catchment List representing catchment information.
#' @param time List representing time information.
#' @param configuration List representing configuration information.
#' @param observations List representing observation information.
#' @param input List representing input information.
#' @param inflows List representing inflows information.
#' @param outflows List representing outflows information.
#' @param output List representing output information.
#' @return An instance of the aeme class.
#'
#' @importFrom sf st_area sf_use_s2
#' @importFrom units drop_units
#'
#' @export

aeme_constructor <- function(
    lake, catchment, time, configuration, observations,
    input, inflows, outflows, output
) {
  # Validate the types before creating the object
  if (!is.list(lake) || !is.list(catchment) || !is.list(time) ||
      !is.list(configuration) || !is.list(observations) || !is.list(input) ||
      !is.list(inflows) || !is.list(outflows) || !is.list(output)) {
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
  if (!is.numeric(lake$init_depth)) {
    stop("Lake inital depth must be numeric.")
  }

  # Catchment type checking for specific elements
  if (!is.character(catchment$name)) {
    stop("Catchment name must be a character.")
  }
  if (!is(catchment$shape, "sf") & !is.null(catchment$shape)) {
    stop("Catchment shape must be an 'sf' object or NULL.")
  }
  if (!is.numeric(catchment$area) & !is.null(catchment$area)) {
    stop(paste(strwrap("Catchment area must be numeric or NULL. If NULL,
                       catchment shape needs to be provided."), collpse = "\n"))
  }
  if (is(catchment$shape, "sf") & is.null(catchment$area)) {
    message("Calculating catchment area from catchment shape:")
    suppressMessages(sf::sf_use_s2(FALSE))
    catchment$area <- sf::st_area(catchment$shape) |>
      units::drop_units()
    message(paste0("   ", round(catchment$area, 2), " m2"))
    suppressMessages(sf::sf_use_s2(TRUE))
  }
  if (!is(catchment$shape, "sf") & is.null(catchment$area)) {
    stop("Catchment area must be provided if no catchment shape is present.")
  }

  # Time type checking for specific elements
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  if (is.character(time$start)) {
    time$start <- as.POSIXct(yaml$time$start, format = "%Y-%m-%d %H:%M:%S",
                             tz = "UTC")
  } else if(!is.POSIXct(time$start)) {
    stop("Time start must be POSIXct.")
  }
  if (is.character(time$stop)) {
    time$stop <- as.POSIXct(yaml$time$stop, format = "%Y-%m-%d %H:%M:%S",
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
    stop("Time spin-up must be a list.")
  } else if (all(!is.numeric(unlist(time$spin_up)))) {
    stop("Time spin-up for models must be numeric.")
  }

  # Configuration type checking for specific elements
  if (!is.list(configuration$physical)) {
    stop("Configuration physical must be a list.")
  } else if (!all(sapply(configuration$physical, \(x) is.list(x) |
                        is.null(x)))) {
    stop("Configuration physical for models must be either a list or NULL.")
  }
  if (!is.list(configuration$bgc)) {
    stop("Configuration bgc must be a list.")
  } else if (!all(sapply(configuration$bgc, \(x) is.list(x) |
                         is.null(x)))) {
    stop("Configuration bgc for models must be either a list or NULL.")
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
  if (!is.null(input$hypsograph)) {
    if (!is.data.frame(input$hypsograph)) {
      stop("Input hypsograph must be a dataframe or NULL.")
    }
  }
  if (!is.null(input$meteo)) {
    if (!is.data.frame(input$meteo)) {
      stop("Input meteo must be a dataframe or NULL.")
    }
  }
  if (!is.logical(input$use_lw)) {
    stop("Input use longwave must be boolean.")
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

  # Output type checking for specific elements
  if (!is.list(output)) {
    stop("Output must be a list of lists or NULL.")
  }


  new("aeme",
      lake = lake,
      catchment = catchment,
      time = time,
      observations = observations,
      input = input,
      inflows = inflows,
      outflows = outflows,
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

#' @title Access catchment slot
#' @param aeme A aeme object.
#' @return list of catchment characteristics
#' @export
setGeneric("catchment", function(aeme) standardGeneric("catchment"))

#' @title Access catchment slot
#' @param aeme A aeme object.
#' @return list of catchment characteristics
#' @export
setMethod("catchment", "aeme", function(aeme) aeme@catchment)

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

#' Update the catchment slot of an aeme object
#'
#' @title Set catchment in aeme object
#' @param aeme A aeme object.
#' @param value New catchment data to be assigned.
#' @return A modified aeme object with updated catchment slot.
#' @export
setGeneric("catchment<-", function(aeme, value) standardGeneric("catchment<-"))

#' Update the catchment slot of an aeme object
#'
#' This method updates the "catchment" slot of a aeme object with new data.
#'
#' @title Set catchment in aeme object
#' @param aeme An aeme object.
#' @param value New catchment data to be assigned.
#' @return A modified aeme object with updated catchment slot.
#' @export
setMethod("catchment<-", "aeme", function(aeme, value) {
  aeme@catchment <- value
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
  catchm <- catchment(object)
  aeme_time <- time(object)
  config <- configuration(object)
  obs <- observations(object)
  inp <- input(object)
  inf <- inflows(object)
  outf <- outflows(object)
  outp <- output(object)

  cat(
      "\t\tAEME ",
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
      "  Catchment \n",
      "Name: ", ifelse(is.null(catchm$name), NA, catchm$name),
      "; Area: ", ifelse(is.null(catchm$area), NA, catchm$area), " m2;
      Shape file: ", ifelse(is(catchm$shape, "sf"), "Present", "Absent"),
      "\n-------------------------------------------------------------------\n",
      "  Time\n",
      "Start: ", as.character(aeme_time$start),
      " Stop: ", as.character(aeme_time$stop),
      " Time step: ", as.character(aeme_time$time_step),
      "\n-------------------------------------------------------------------\n",
      "  Configuration\n",
      "          Physical  |   Biogeochemical",
      "\nDY-CD    : ", ifelse(is.null(config[["physical"]][["dy_cd"]]),
                              "Absent ", "Present"), "    |   ",
      ifelse(is.null(config[["bgc"]][["dy_cd"]]), "Absent ",
             "Present"),
      "\nGLM-AED  : ", ifelse(is.null(config[["physical"]][["glm_aed"]]),
                              "Absent ", "Present"), "    |   ",
      ifelse(is.null(config[["bgc"]][["glm_aed"]]), "Absent ",
             "Present"),
      "\nGOTM-WET : ", ifelse(is.null(config[["physical"]][["gotm_wet"]]),
                             "Absent ", "Present"), "    |   ",
      ifelse(is.null(config[["bgc"]][["gotm_wet"]]),
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
      "; Hypsograph: ", ifelse(is.data.frame(inp$hypsograph),
                                 "Present", "Absent"),
      "; Meteo: ", ifelse(is.data.frame(inp$meteo),
                               "Present", "Absent"),
      ";\nUse longwave: ", inp$use_lw,
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
# summary(aeme)

#' Plot an aeme object
#'
#' This method plots the aeme object.
#'
#' @title Update summary Method
#' @param x An aeme object.
#' @param y An aeme slot (optional).
#' @param ... additional arguments affecting the plot produced.
#' @return prints the aeme object to the console.
#' @export
setMethod("plot", "aeme", function(x, y, ...) {
  inp <- input(x)
  outp <- output(x)
  obs <- observations(x)

  # z <- t(as.matrix(outp[[m]][["HYD_temp"]]))
  # Date = outp[[m]][["Date"]]
  # filled.contour(x = Date, z = z, color.palette = hcl.colors,
  #                key.title = title(main = "Temp\n(\u00B0C)"))

  mod <- lapply(names(outp), \(m) {
    if (!is.null(outp[[m]])) {
      depth <- outp[[m]][["DEPTH"]]
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
  ylim <- range(mod$Ts, mod$Tb)
  plot(mod$Date, mod$Ts, type = "n", ylab = "Temperature (\u00B0C)",
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
  legend("bottomright", legend = c(), pch = c(1, 4),
         col = "black", bg = "transparent")

  # Water level
  plot(mod$Date, mod$lvl, type = "n", ylab = "Water level (m)",
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
})

