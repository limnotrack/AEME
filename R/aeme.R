#' S4 Class representing AEME data
#'
#' This class represents data related to a lake.
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

aeme <- function(
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

# Convert the parsed YAML content to an instance of the aeme class
# aeme_data <- aeme(
#   lake = yaml$lake,
#   catchment = yaml$catchment,
#   time = yaml$time,
#   observations = yaml$observations,
#   input = yaml$input,
#   inflows = yaml$inflows,
#   outflows = yaml$outflows,
#   output = yaml$output
# )
#
# # Print the S4 object
# print(aeme_data)
