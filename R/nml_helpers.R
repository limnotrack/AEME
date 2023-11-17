#' appends nml list with new values
#'
#' @param textLine string; line from GLM nml file
#' @param lineNum numeric; line number in file
#' @param blckName string; for block name
#' @return a logical vector
#' @keywords internal
#' @source glmtools package: https://github.com/GLEON/glmtools
#' @noRd

buildVal	<-	function(textLine, lineNum, blckName){
  #-----function appends nml list with new values-----
  # remove all text after comment string
  textLine	<-	strsplit(textLine,'!')[[1]][1]

  if (!any(grep("=", textLine))){
    stop(c("no hanging lines allowed in .nml, used ",textLine,'.\nSee line number:',lineNum,' in "&',blckName,'" section.'))
  }
  params	<-	strsplit(textLine,"=") # break text at "="
  parNm	  <-	params[[1]][1]
  parVl	  <-	params[[1]][2]
  # figure out what parval is...if string, remove quotes and keep as string
  # ***for logical text, use "indentical" so that 0!= FALSE
  # can be: string, number, comma-sep-numbers, or logical

  # special case for date:
  if (is.na(parVl)){
    stop('Empty values after "', textLine, '" on line ', lineNum,
         '. \nPerhaps the values are on the next line?', call. = FALSE)
  }
  if (nchar(parVl>17) & substr(parVl,14,14)==':' & substr(parVl,17,17)==':'){
    parVl<-paste(c(substr(parVl,1,11),' ',substr(parVl,12,nchar(parVl))),collapse='')
  }
  if (any(grep("'",parVl))){

    parVl	<-	gsub("'","",parVl)
  }else if (any(grep("\"",parVl))){
    parVl  <-	gsub("\"","",parVl)
  }else if (isTRUE(grepl(".true.",parVl) || grepl(".false.",parVl))){
    logicals <- unlist(strsplit(parVl,","))
    parVl <- from.glm_boolean(logicals)
  }else if (any(grep(",",parVl))){	# comma-sep-nums
    parVl	<-	c(as.numeric(unlist(strsplit(parVl,","))))
  }else {	# test for number
    parVl	<-	as.numeric(parVl)
  }
  lineVal	<-	list(parVl)
  names(lineVal)	<-	parNm
  return(lineVal)
}

#' go from glm2.nml logical vectors to R logicals
#'
#' @param values a vector of strings containing either .false. or .true.
#' @return a logical vector
#' @keywords internal
#' @noRd
from.glm_boolean <- function(values){

  logicals <- sapply(values, FUN = function(x){
    if (!isTRUE(grepl(".true.", x) || grepl(".false.", x))){
      stop(x, ' is not a .true. or .false.; conversion to TRUE or FALSE failed.',
           call. = FALSE)
    }
    return(ifelse(isTRUE(grepl(".true.", x)), TRUE, FALSE))
  })
  return(as.logical(logicals))
}

#' Convert R logical to GLM logical
#'
#' @param values vector of values
#'
#' @return GLM logical equivalent
#' @noRd
to.glm_boolean <- function(values){
  val.logical <- values
  values[val.logical] <- '.true.'
  values[!val.logical] <- '.false.'
  return(values)
}

#' Find block in nml object
#'
#' @param nml nml object
#' @param argName name of argument in GLM nml file
#'
#' @return nml block
#' @noRd
findBlck	<-	function(nml,argName){

  # test for argName being a string
  if (!is.character(argName)){stop(c("parameter name must be a string"))}
  fau <- " "
  fault.string <- rep(fau,1000) # names fault matrix, only returned when empty match
  blockNames	<-	names(nml)
  blckI	<-	c()
  for (i in seq_len(length(blockNames))){
    if (any(argName %in% names(nml[[i]]))){
      blckI	<- c(blckI,i)
    } else {
      one.i <- which(fault.string==fau)[1]
      fault.string[one.i:(one.i+length(names(nml[[i]]))-1)]=names(nml[[i]])
    }

  }
  fault.string <- fault.string[!fault.string==fau] # is empty if found
  # test to see if a block match was made
  if (is.null(blckI)){stop(c("parameter name ",argName," not found in nml. Possible names:",paste(fault.string,collapse=', ')))}
  return(blckI)
}

#' Set a list of arguments in nml object
#'
#' @inheritParams set_nml
#'
#' @return updated nml object
#' @noRd
setnmlList <- function(glm_nml, arg_list){
  if (!is.list(arg_list)){stop("arg_list must be a list")}

  if (any(nchar(names(arg_list)) == 0) | length(names(arg_list)) == 0){
    stop('arg_list must be a named list')
  }

  arg_names  <-	names(arg_list)

  for (i in seq_len(length(arg_names))){
    glm_nml <- set_nml(glm_nml, arg_name = arg_names[i],
                       arg_val = arg_list[[i]])
  }

  return(glm_nml)
}

#' Check if file is a GLM nml file
#'
#' @param nml_file filepath; to nml file
#'
#' @return TRUE or FALSE
#' @noRd
#' @importFrom utils tail
is_nml_file <- function(nml_file){

  is_nml <- FALSE
  fl_ext <- tail(strsplit(nml_file, "\\.")[[1]],1)

  if (fl_ext == 'nml'){
    is_nml <- TRUE
  }
  return(is_nml)
}

#' What about the file is ASCII
#'
#' @param file filepath
#'
#' @return vector of strings
#' @noRd
#' @importFrom utils capture.output
#' @importFrom tools showNonASCIIfile
what_ascii <- function(file){
  response <- utils::capture.output(tools::showNonASCIIfile(file))
  return(response)
}

#' Check if file is ASCII
#'
#' @param file filepath
#'
#' @return TRUE or FALSE
#' @noRd
ascii_only <- function(file){
  response <- what_ascii(file)


  if (length(response) > 0){
    return(FALSE)
  } else {
    return(TRUE)
  }

}

#' Get nml block
#'
#' @inheritParams read_nml
#' @param arg_name string; argument name
#' @param warn logical; print warning
#'
#' @return string of argument name
#' @noRd
get_block <- function(glm_nml, arg_name, warn = TRUE){
  arg_split = strsplit(arg_name,'::')[[1]]
  if (length(arg_split) > 1){
    blck = arg_split[1]
    arg_name = get_arg_name(arg_name)
  } else{
    blck	<-	findBlck(glm_nml,arg_name)
  }
  if (length(blck) > 1){
    if (warn)
      warning(arg_name, " found in ", paste(names(glm_nml[blck]),
                                            collapse=' & '),
              ", returning the first. Try ",names(glm_nml[blck])[1],"::",
              arg_name, " for explicit match")
    blck = blck[1]
  }

  return(blck)
}

#' Get nml argument
#'
#' @param arg_name
#'
#' @return string of argument name
#' @noRd
get_arg_name <- function(arg_name){
  arg_split = strsplit(arg_name,'::')[[1]]

  if (length(arg_split) > 1){
    blck = arg_split[1]
    arg_name = arg_split[2]
  }
  return(arg_name)
}

#' Convert to nml class
#'
#' @param list_obj
#'
#' @return object of nml class
#' @noRd
.nml <- function(list_obj){
  nml <- list_obj
  class(nml) <- "nml"
  invisible(nml)
}

#' @export
print.nml <- function(x, ...){
  glm_nml <- x
  for (i in seq_len(length(names(glm_nml)))){ # these are the blocks
    blckNm  <-	names(glm_nml)[i]
    cat("&")
    cat(blckNm)
    cat('\n')
    blckList	<-	glm_nml[[i]]
    for (j in seq_len(length(names(blckList)))){
      cat('   ')
      cat(names(blckList)[j])
      cat(' = ')
      if (length(blckList[[j]])>1){
        if (is.logical(blckList[[j]])){
          charText <- to.glm_boolean(blckList[[j]])
        } else {
          charText <- c(blckList[[j]])
        }
        writer	<-	paste(charText,collapse=', ')
      } else if (is.character(blckList[[j]])) {
        charText <- strsplit(blckList[[j]],',')
        writer <- paste(c("'",paste(c(charText[[1]]),collapse="','"),"'"),collapse='')
      } else if (is.logical(blckList[[j]])){
        writer <- to.glm_boolean(blckList[[j]])
      } else {
        writer <- blckList[[j]]
      }
      cat(writer)
      cat('\n')
    }
    cat('/\n')
  }
}
#' @export
summary.nml <- function(object,...){
  print(object,...)
}
