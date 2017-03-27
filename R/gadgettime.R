#' Create a gadgettime object
#'
#' Create a time file object, from fresh or an existing file.
#'
#' @param file_name	The name of the time file
#' @param path		The path to the gadget directory to read from
#' @param missingOkay	If \code{TRUE}, return an empty time file object if file does not exist.
#' @return A list of time components representing file
#' @examples
#' path <- './cod-model'
#' gadgettime('Modelfiles/time', path, missingOkay = TRUE)  # Read 'Modelfiles/time' time file, creating it if it doesn't exist
#' @export
gadgettime <- function(file_name, path, missingOkay = FALSE) {
  gf <- read.gadget.file(path, file_name, file_type = "time", missingOkay = missingOkay)
  class(gf) <- c("gadgettime", class(gf))
  
  return(gf)
}    




#' Update gadget time components in a time file
#'
#' Replace and/or append new time comonents to an existing file
#'
#' @param gf		The gadgetfile object to update
#' @param component	Either a replacement \code{gadget_time_component} (from MFDB or rgadget), or a component type name
#' @param ...		If a component type was provided above, the extra options to supply to \code{gadget_time_component}
#'
#' @examples
#' path <- './model'
#' gadgettime('time', path, missingOkay = TRUE) %>%
#'    gadget_update() %>% 
#'    write.gadget.file(path)
#' @export
gadget_update.gadgettime <- function(gf, ...) {
  args <- list(...)
  names(args) <- tolower(names(args))
  tmp <- 
    intersect(c("firstyear",   "firststep",   "lastyear",    "laststep",    "notimesteps"),
              names(args))
  if(length(tmp)>0){
    args <- args[tmp]
  } else {
    stop('Unknown values',paste(names(args),collapse=' - '),
         'Valid options are: c("firstyear",   "firststep",   "lastyear",    "laststep",    "notimesteps")')
  }
  gf[[1]][[names(args)]] <- args
  return(gf)
}

