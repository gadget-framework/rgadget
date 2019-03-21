
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
    map(~infuser::infuse("{{linf}} * (1 - exp(-1 * (0.001 * {{k}}) * ({{a}} - (1 + log(1 - {{recl}}/{{linf}})/(0.001 * {{k}})))))",
                         a=.,linf=linf,k=k,recl=recl)) %>% 
    map(~parse(text=.) %>% 
          map(to.gadget.formulae)) %>% 
    unlist()
}

#' Initial parameter guess
#'
#' This is a convenience function that allows the editing of parameter files by variable (switch) names. 
#'
#' @name init_guess
#' @param dat Input data frame, read in from read.gadget.parameters
#' @param pattern name of parameter, can include grep-able wildcards
#' @param value new parameter value 
#' @param lower new lower bound
#' @param upper new upper bound
#' @param optimise should the parameter be optimised
#'
#' @return updated data frame with the new parameter values
#' @export
init_guess <- function(dat,pattern, value = 0,  lower = -999, upper = 999, optimise=0){
  dat[grepl(pattern,dat$switch),'value'] <- value
  dat[grepl(pattern,dat$switch),'upper'] <- upper
  dat[grepl(pattern,dat$switch),'lower'] <- lower
  dat[grepl(pattern,dat$switch),'optimise'] <- optimise
  return(dat)
}