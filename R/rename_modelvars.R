#' Rename variables among the models and controlled vocabulary
#'
#' @param input string; vector of names
#' @param type_output string; type of output
#' @param type_input string; type of input
#' @param verbose boolean; print changes to console
#'
#' @return vector of changed names
#' @export
#'
#' @examples
rename_modelvars <- function(input,
                             type_output = "name_parse",
                             type_input = "name",
                             verbose = F) {

  # Load Rdata
  data("key_naming", package = "AEME", envir = environment())

  this.key <- data.frame(key_naming[key_naming[, type_input] %in% input,])

  names.new <- this.key[match(input,this.key[,type_input]), type_output]

  if (sum(is.na(names.new)) > 0) {
    stop("incomplete match for names.. check input")
  }

  if (verbose) {
    cat(paste(input,"          matched to        ",names.new, collapse = "\n"))
  }

  return(names.new)
}



