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
von_b_formula <- function(a,linf='Linf',k='k',recl='recl'){
  a %>% 
    purrr::map(~infuser::infuse("{{linf}} * (1 - exp(-1 * (0.001 * {{k}}) * ({{a}} - (1 + log(1 - {{recl}}/{{linf}})/(0.001 * {{k}})))))",
                         a=.,linf=linf,k=k,recl=recl)) %>% 
    purrr::map(~parse(text=.) %>% 
          map(to.gadget.formulae)) %>% 
    unlist()
}

