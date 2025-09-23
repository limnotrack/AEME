#' Check and convert time input to POSIXct
#'
#' @param x character, Date, or POSIXt object representing time
#' @param tz character; timezone, default is "UTC"
#'
#' @returns POSIXct object
#' @noRd

check_time_format <- function(x, tz = "UTC") {
  # If it's already Date or POSIXt, convert to POSIXct
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  } else if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  } else if (is.character(x)) {
    # Acceptable formats
    formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")
    parsed <- NA
    for (fmt in formats) {
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      if (!is.na(parsed)) return(parsed)
    }
    stop("Input must be in 'YYYY-mm-dd' or 'YYYY-mm-dd HH:MM' or 'YYYY-mm-dd HH:MM:SS' format, or a Date/POSIXt object")
  } else {
    stop("Input must be a character string, Date, or POSIXt object")
  }
}

#' Check hypsograph data frame
#'
#' @param hypsograph data frame with columns "depth", "area" and "elev". Depth should be
#' monotonic decreasing and area should be monotonic increasing.
#' If NULL, the function will check for hypsograph in the input slot of the Aeme object.
#' @inheritParams build_aeme
#' @returns Invisibly returns the hypsograph data frame if aeme is NULL, otherwise returns the Aeme object.
#' @noRd
#' @importFrom dplyr arrange desc

check_hypsograph <- function(hypsograph, aeme = NULL) {
  if (!is.null(aeme)) {
    # Check hypsograph is in the input slot
    inp <- input(aeme)
    hypsograph <- inp$hypsograph
    if (is.null(hypsograph)) {
      stop("Hypsograph not found in input. Please add a hypsograph data frame using the 'input' slot of the Aeme object.")
    }
  }
  
  # Check hypsograph is a data frame
  if (!is.data.frame(hypsograph)) {
    stop("hypsograph must be a data frame")
  }
  # Check hypsograph has required columns: depth and area
  required_cols <- c("depth", "area", "elev")
  if (!all(required_cols %in% colnames(hypsograph))) {
    stop(paste("hypsograph must contain the following columns:", 
               paste(required_cols, collapse = ", ")))
  }
  
  # Check depth and area are monotonic
  hypsograph <- hypsograph |> 
    dplyr::arrange(dplyr::desc(depth))
  if (any(diff(hypsograph$depth) >= 0)) {
    stop("depth must be monotonic decreasing")
  }
  if (any(diff(hypsograph$area) >= 0)) {
    stop("area must be monotonic increasing")
  }
  if (is.null(aeme)) {
    invisible(hypsograph)
  } else {
    invisible(aeme)
  }
}

#' Check if object is a valid Aeme object
#' @param aeme object to check
#' @returns Invisibly returns the Aeme object if valid, otherwise throws an 
#' error.
#' @noRd
check_aeme <- function(aeme) {
  # Check if aeme is a Aeme object
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be a Aeme object")
  }
  required_slots <- c("lake", "time", "input", "inflows", "outflows", 
                      "water_balance", "parameters")
  if (!all(required_slots %in% slotNames(aeme))) {
    stop(paste("aeme must contain the following slots:",
               paste(required_slots, collapse = ", ")))
  }
  invisible(aeme)
}

#' Check meteorological data frame
#' @param met data frame 
#' @returns Invisibly returns the met data frame if valid, otherwise throws an 
#' error.
#' @noRd
check_met <- function(met) {
  # Check met is a data frame
  if (!is.data.frame(met)) {
    stop("met must be a data frame")
  }
  # Check met has required columns
  required_cols <- c("Date", "MET_radswd", "MET_tmpair", "MET_pprain")
  if (!all(required_cols %in% colnames(met))) {
    stop(paste("met must contain the following columns:", 
               paste(required_cols, collapse = ", ")))
  }
  
  wind1 <- "MET_wnspd"
  wind2 <- c("MET_wnduvu", "MET_wnduvv")
  if (!wind1 %in% colnames(met) & !all(wind2 %in% colnames(met))) {
    stop("met must contain either MET_wnspd or both MET_wnduvu and MET_wnduvv columns")
  }
  
  # Check met has no missing values in required columns
  if (any(is.na(met[, required_cols]))) {
    stop("met contains NA in required columns")
  }
  
  # Check Date column is Date
  if (!inherits(met$Date, "Date")) {
    stop("met$Date must be a Date object")
  }
  
  invisible(met)
}
