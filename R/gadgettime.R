#' Create a gadgettime object
#'
#' Create a time file object, from fresh or an existing file.
#'
#' @param file_name	The name of the time file
#' @param path		The path to the gadget directory to read from
#' @param missingOkay	If \code{TRUE}, return an empty time file object if file does not exist.
#' @return A list of time components representing file
#' @examples
#' \dontrun{
#' path <- './cod-model'
#' # Read 'Modelfiles/time' time file, creating it if it doesn't exist
#' gadgettime('Modelfiles/time', path, missingOkay = TRUE)  
#' }
#' @export
gadgettime <- function(file_name, path, missingOkay = FALSE) {
  gf <- read.gadget.file(path, file_name, file_type = "time", missingOkay = missingOkay)
  if (length(gf) == 0) gf[[1]] <- list()
  class(gf) <- c("gadgettime", class(gf))
  
  return(gf)
}    

  


#' Update gadget time components in a time file
#'
#' Replace and/or append new time comonents to an existing file
#'
#' @param gf		The gadgetfile object to update
#' @param ...		Valid options for a time file; firstyear, firststep, lastyear, laststep, notimesteps. See gadget user guide
#'
#' @examples
#' \dontrun{
#' library(magrittr)  # import %>% function
#' path <- './model'
#' gadgettime('time', path, missingOkay = TRUE) %>%
#'    gadget_update(firstyear = 1990, lastyear = 2000) %>% 
#'    write.gadget.file(path)
#'    }
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
  gf[[1]][names(args)] <- args
  return(gf)
}

