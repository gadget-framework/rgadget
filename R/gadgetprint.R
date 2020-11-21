#' Create a gadgetprint object
#'
#' Create a print file object, from fresh or an existing file.
#'
#' @param file_name	The name of the print file
#' @param path		The path to the gadget directory to read from
#' @param missingOkay	If \code{TRUE}, return an empty print file object if file does not exist.
#' @return A list of print components representing file
#' @examples
#' \dontrun{
#' path <- './cod-model'
#' # Read 'print' print file, creating it if it doesn't exist
#' gadgetprintfile('print', path, missingOkay = TRUE)  
#' }
#' @export
gadgetprintfile <- function(file_name, path, missingOkay = FALSE) {
  gf <- read.gadget.file(path, file_name, file_type = "print",
                         missingOkay = missingOkay)
  class(gf) <- c("gadgetprint", class(gf))
  
  return(gf)
} 

#' Update gadget print components in a print file
#'
#' Replace and/or append new print comonents to an existing file
#'
#' @param gf		The gadgetfile object to update
#' @param component	Either a replacement \code{gadget_print_component} (from MFDB or rgadget), or a component type name
#' @param ...		If a component type was provided above, the extra options to supply to \code{gadget_print_component}
#'
#' @export
gadget_update.gadgetprint <- function(gf, component, ...) {
  if (!("gadget_print_component" %in% class(component))) {
    # Assume arguments are function call for gadget_print_component
    component <- gadgetprintcomponent(component, ...)
  }
  
  # print components always have some kind of preamble, to space out
  if (is.null(attr(component, "preamble"))) {
    attr(component, "preamble") <- ""
  }
  
  # Replace component with matching name/type, or append
  gf <- gadget_component_replace(gf, component, function(comp) {
    if (length(comp) == 0) "" else attr(comp$printfile, 'file_name')
  })
  
  return(gf)
}

#' this function removes named print components
#' @param gf		The gadgetfile object to update
#' @param comp_name named components to remove
#' @export 
gadget_discard.gadgetprint <- function(gf,comp_name) {
  ## TODO: this function should also clean up asociated data files 
  file_config <- attr(gf,'file_config')
  file_name <- attr(gf,'file_name')
  class_val <- class(gf)
  gf <- gf %>% purrr::discard(function(x) x[[1]] %in% comp_name)
  attr(gf,'file_config') <- file_config
  attr(gf,'file_name') <- file_name 
  class(gf) <- class_val
  return(gf)
}