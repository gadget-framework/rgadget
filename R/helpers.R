#' von B formula
#'
#' This is a convenience function that creates the von B formulas for the recruitment and initial population files
#'
#' @name von_B_formula
#' @param a age
#' @param linf value or gadget formula for Linf 
#' @param k value or gadget formula for 1000*K
#' @param recl value or gadget formula mean length at recruit 
#'
#' @return vector of von B formula by age
#' @export
von_b_formula <- function(a, linf='Linf', k='k', recl='recl') {
    vapply(a, function (age) {
      if(is.character(age)){
        age <- as.symbol(age)
      }
        to.gadget.formulae(substitute(Linf * (1 - exp(
            -1 *
            (0.001 * k) *
            (age - (1 + log(1 - recl/Linf)/(0.001 * k)))
        )), list(
            Linf = as.symbol(linf),
            k = as.symbol(k),
            recl = as.symbol(recl),
            age = age)))
    }, character(1)) %>% unname()
}
