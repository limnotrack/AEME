#' S4 Class representing AEME data
#'
#' This class represents data related to a lake for running AEME.
#' @title aeme Class
#' @name aeme
#' @aliases aeme-class
#' @slot lake A list representing lake information.
#' @slot catchment A list representing catchment information.
#' @slot time A list representing time information.
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
#' @param observations List representing observation information.
#' @param input List representing input information.
#' @param inflows List representing inflows information.
#' @param outflows List representing outflows information.
#' @param output List representing output information.
#' @return An instance of the aeme class.
#' @export

aeme_constructor <- function(
    lake, catchment, time, observations,
    input, inflows, outflows, output
) {
  # Validate the types before creating the object
  if (!is.list(lake) || !is.list(catchment) || !is.list(time) ||
      !is.list(observations) || !is.list(input) || !is.list(inflows) ||
      !is.list(outflows) || !is.list(output)) {
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
  if (!is.numeric(lake$depth)) {
    stop("Lake depth must be numeric.")
  }
  if (!is.numeric(lake$area)) {
    stop("Lake area must be numeric.")
  }
  if (!is.numeric(lake$init_depth)) {
    stop("Lake inital depth must be numeric.")
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
setMethod("catchment", "aeme", function(aeme) aeme@lake)

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

# Setter functions
setGeneric("lake<-", function(aeme, value) standardGeneric("lake<-"))
setMethod("lake<-", "aeme", function(aeme, value) {
  aeme@lake <- value
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
  cat(
      "\t\tAEME ",
      paste0(
        "\n-------------------------------------------------------------------\n",
        "  Lake\n",
        object@lake$name, " (ID: ", object@lake$id), "); Lat: ",
      round(object@lake$latitude, 2), "; Lon: ", round(object@lake$longitude,
                                                       2),
      "; Elev: ", round(object@lake$elevation, 2), "m; Depth: ",
      round(object@lake$depth, 2), "m;\nArea: ", round(object@lake$area, 2),
      "m2; Shape file: ", ifelse(is(object@lake$shape, "sf"), "Present",
                                 "Absent"),
      "\n-------------------------------------------------------------------\n",
      "  Catchment \n",
      "To be determined...",

      "\n-------------------------------------------------------------------\n",
      "  Time\n",
      "Start: ", as.character(object@time$start),
      " Stop: ", as.character(object@time$stop),
      " Time step: ", as.character(object@time$time_step),
      "\n-------------------------------------------------------------------\n",
      "  Observations\n",
      "Lake: ", ifelse(is.data.frame(object@observations$lake), "Present",
                       "Absent"),
      "; Level: ", ifelse(is.data.frame(object@observations$level), "Present",
                          "Absent"),
      "\n-------------------------------------------------------------------\n",
      "  Input\n",
      "Inital profile: ", ifelse(is.data.frame(object@input$init_profile),
                                 "Present", "Absent"),
      "; Hypsograph: ", ifelse(is.data.frame(object@input$hypsograph),
                                 "Present", "Absent"),
      "; Meteo: ", ifelse(is.data.frame(object@input$meteo),
                               "Present", "Absent"),
      ";\nUse longwave: ", object@input$use_lw,
      "; Kw: ", object@input$Kw,
      "\n-------------------------------------------------------------------\n",
      "  Inflows\n",
      "Data: ", ifelse(is.list(object@inflows$data),
                                 "Present", "Absent"),
      "; Scaling factors: DY-CD: ", round(object@inflows$factor$dy_cd, 2),
      "; GLM-AED: ", round(object@inflows$factor$glm_aed, 2),
      "; GOTM-WET: ", round(object@inflows$factor$gotm_wet, 2),
      "\n-------------------------------------------------------------------\n",
      "  Outflows\n",
      "Data: ", ifelse(is.list(object@outflows$data),
                       "Present", "Absent"),
      "; Scaling factors: DY-CD: ", round(object@outflows$factor$dy_cd, 2),
      "; GLM-AED: ", round(object@outflows$factor$glm_aed, 2),
      "; GOTM-WET: ", round(object@outflows$factor$gotm_wet, 2),
      "\n-------------------------------------------------------------------\n",
      "  Output: ", "\n",
      "DY-CD: ", ifelse(is.null(object@output$dy_cd), "Absent", "Present"),
      "\nGLM-AED: ", ifelse(is.null(object@output$glm_aed), "Absent",
                            "Present"),
      "\nGOTM-WET: ", ifelse(is.null(object@output$gotm_wet), "Absent",
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
  cat("Lake observations:\n")
  print(summary(object@observations$lake))
  cat("-------------------------------------------------------------------\n")
  cat("Lake level:\n")
  print(summary(object@observations$level))
  cat("-------------------------------------------------------------------\n")
  cat("Meteorology:\n")
  print(summary(object@input$meteo))
  cat("-------------------------------------------------------------------\n")
  cat("Inflows:\n")
  for (i in seq_along(object@inflows$data)) {
    cat("  ", names(object@inflows$data)[i], "\n")
    print(summary(object@inflows$data[[i]]))
  }
  cat("-------------------------------------------------------------------\n")
  cat("Outflows:\n")
  for (i in seq_along(object@outflows$data)) {
    cat("  ", names(object@outflows$data)[i], "\n")
    print(summary(object@outflows$data[[i]]))
  }
  cat("-------------------------------------------------------------------\n")
  cat("Outputs:\n")
  for (i in seq_along(object@output)) {
    cat("  ", toupper(gsub("_", "-", names(object@output)[i])), "\n")
    if (!is.null(object@output[[i]])) {

      depth <- object@output[[i]][["DEPTH"]]
      lyr <- object@output[[i]][["LAYERS"]]
      temp <- object@output[[i]][["HYD_temp"]]
      df <- data.frame(Date = rep(object@output[[i]][["Date"]],
                                  each = nrow(temp)),
                       depth = unlist(lyr), temp = unlist(temp),
                       lvl = rep(depth, each = nrow(temp)))
      print(summary(df))
    } else {
      print(summary(object@output[[i]]))
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
  out <- output(x)

  mod <- lapply(names(out), \(m) {
    if (!is.null(out[[m]])) {
      depth <- out[[m]][["DEPTH"]]
      # lyr <- out[[m]][["LAYERS"]]
      temp <- out[[m]][["HYD_temp"]]
      df <- data.frame(Date = out[[m]][["Date"]],
                       # depth = unlist(lyr),
                       Ts = unlist(temp[nrow(temp), ]),
                       Tb = unlist(temp[1, ]),
                       lvl = depth,
                       model = m)
      df
    }
  }) |>
    do.call(rbind, args = _)

  #
  par(mfrow = c(2, 1))
  ylim <- range(mod$Ts, mod$Tb)
  plot(mod$Date, mod$Ts, type = "n", ylab = "Temperature (\u00B0C)",
       xlab = "Time", main = "Water temperature", ylim = ylim)
  for (i in seq_along(out)) {
    sub <- mod[mod$model == names(out)[i], ]
    lines(sub$Date, sub$Ts, col = i)
    lines(sub$Date, sub$Tb, col = i, lty = 2)
  }
  legend("topright", legend = c("Surface", "Bottom"), lty = 1:2, col = "black")

  # Water level
  plot(mod$Date, mod$lvl, type = "n", ylab = "Water level (m)",
       xlab = "Time", main = "Water level")
  for (i in seq_along(out)) {
    sub <- mod[mod$model == names(out)[i], ]
    lines(sub$Date, sub$lvl, col = i)
  }
  legend("bottomright", legend = c("DY-CD", "GLM-AED", "GOTM-WET"), lty = 1, col = 1:3)
})

