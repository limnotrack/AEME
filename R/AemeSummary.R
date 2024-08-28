#' S4 Class representing summarised AEME data
#'
#' This class represents data summarising an Aeme object.
#' @title Aeme Summary Class
#' @name AemeSummary
#' @aliases AemeSummary-class
#' @slot aeme An Aeme object which has been summarised
#' @export

setClass("AemeSummary",
         representation(
           aeme = "Aeme"
         )
)
