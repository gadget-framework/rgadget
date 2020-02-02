.onLoad <- function(libname, pkgname){
  
  utils::globalVariables(".")
  utils::globalVariables("age")
  utils::globalVariables("n")
  
  
  
}

#' Deprecated functions
#' @rdname deprecated_functions
#' @param ... unused inputs
#' @export
gadget.forward <- function(...){
  stop('-- gadget.forward is deprecated, please adjust your code to gadget_project_* functions --')
}

#' @rdname deprecated_functions
#' @export
gadget.ypr<- function(...){
  stop('-- gadget.ypr is deprecated, please adjust your code to gadget_project_* functions --')
}