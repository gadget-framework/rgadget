#' Create a gadgetlikelihood object
#'
#' Create a likelihood file object, from fresh or an existing file.
#'
#' @param file_name	The name of the likelihood file
#' @param path		The path to the gadget directory to read from
#' @param missingOkay	If \code{TRUE}, return an empty likelihood file object if file does not exist.
#' @return A list of likelihood components representing file
#' @examples
#' path <- './cod-model'
#' gadgetlikelihood('likelihood', path, missingOkay = TRUE)  # Read 'likelihood' likelihood file, creating it if it doesn't exist
#' @export
gadgetlikelihood <- function(file_name, path, missingOkay = FALSE) {
    gf <- read.gadget.file(path, file_name, file_type = "likelihood", missingOkay = missingOkay)
    class(gf) <- c("gadgetlikelihood", class(gf))

    return(gf)
}    

#' Update gadget likelihood components in a likelihood file
#'
#' Replace and/or append new likelihood comonents to an existing file
#'
#' @param gf		The gadgetfile object to update
#' @param component	Either a replacement \code{gadget_likelihood_component} (from MFDB or rgadget), or a component type name
#' @param ...		If a component type was provided above, the extra options to supply to \code{gadget_likelihood_component}
#'
#' @examples
#' path <- './model'
#' gadgetlikelihood('likelihood', path, missingOkay = TRUE) %>%
#'    gadget_update(gadget_likelihood_component('understocking', name = 'understocking')) %>% # Add an understocking component
#'    gadget_update('understocking', name = 'understocking') %>% # Identical to above
#'    write.gadget.file(path)
#' @export
gadget_update.gadgetlikelihood <- function(gf, component, ...) {
    if (!("gadget_likelihood_component" %in% class(component))) {
        # Assume arguments are function call for gadget_likelihood_component
        component <- gadget_likelihood_component(component, ...)
    }

    # Likelihood components always have some kind of preamble, to space out
    if (is.null(attr(component, "preamble"))) {
        attr(component, "preamble") <- ""
    }

    # Replace component with matching name/type, or append
    gf <- gadget_component_replace(gf, component, function(comp) {
        if (length(comp) == 0) return("")
        return(paste(comp$type, comp$name, sep = ":", collapse = "."))
    })

    return(gf)
}

gadget_component_replace <- function(gfile, newcomponent, namefn, component_name = "component") {
    # Iterate through, stop when we either get to the end or find a match
    newname <- namefn(newcomponent)
    for (i in seq_len(length(gfile) + 1)) {
        if (i > length(gfile)) break;
        if (namefn(gfile[[i]]) == newname) break;
    }

    gfile[[i]] <- newcomponent
    names(gfile)[[i]] <- component_name
    return(gfile)
}

#' this function removes named likelihood components
#' @param gf		The gadgetfile object to update
#' @param comp_name named components to remove
#' @export gadget_discard.gadgetlikelihood
gadget_discard.gadgetlikelihood <- function(gf,comp_name,...){
  ## TODO: this function should also clean up asociated data files 
  file_config <- attr(gf,'file_config')
  file_name <- attr(gf,'file_name')
  class_val <- class(gf)
  gf <- gf %>% purrr::discard(function(x) x$name %in% comp_name)
  attr(gf,'file_config') <- file_config
  attr(gf,'file_name') <- file_name 
  class(gf) <- class_val
  return(gf)
}
