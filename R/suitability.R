##' The suitability function for predation used in the R model is:
##' \deqn{S_{pred,prey}(L,l) = \frac{\delta}{1+e^{-\alpha-\beta l-\gamma L}}}{S_{pred,prey}(L,l) = \frac{delta}{1+e^{-alpha-beta l-gamma L}}}
##' With one predator, one prey and otherfood the equation becomes:
##' \deqn{C_{L,l}=N_{L}M_{L}\Psi_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA}}
##' \deqn{=N_{L}M_{L}\frac{F_{L,l}}{\sum_lF_{L,l}+OA+HA}}
##' where $O$ is the density of otherfood.
##' @title Prey suitability
##' @name suitability 
##' @param params suitability paramters
##' @param l prey length group(s)
##' @param L predator length group(s)
##' @param type suitability function
##' @param normalize Logical, should the output be normalized 
##' @param to.data.frame Logical, return a data.frame instead of an array
##' @return matrix of suitabilities, columns prey length, lines predator length
##' @export
suitability <- function(params,
                        l,
                        L=c(0),
                        type = 'exponential',
                        to.data.frame = FALSE,
                        normalize = FALSE)
{
  
  if(tolower(type) == 'andersenfleet'){
    type <- 'andersen'
    L <- params[6]
  }
  
  if(tolower(type) == 'constant'){
    S <- array(params[1],c(length(L),length(l)))
    
  } else if(tolower(type) == 'straightline') {
    S <- array(rep(l*params[1] + params[2],each = length(L)),
               c(length(L),length(l)))
    
  } else if(tolower(type) == 'exponential'){
    S <- array(params[4]/(1+exp(-(params[1]+params[2]*rep(l,each=length(L))+
                                    params[3]*rep(L,length(l))))),
               c(length(L),length(l)))
    
  } else if(tolower(type) == 'exponentiall50' |
            tolower(type) == 'expsuitfuncl50' | 
            tolower(type) == 'newexponentiall50'){
    S <- array(rep(1/(1+exp(-params[1]*(l - params[2]))),each = length(L)),
               c(length(L),length(l)))
    
  } else if(tolower(type) == 'richards') {
    S <- array(params[4]/(1+exp(-(params[1]+params[2]*rep(l,each=length(L))+
                                    params[3]*rep(L,length(l))))),
               c(length(L),length(l)))^(1/params[5])
    
  } else if(tolower(type) == 'andersen') {
    l.tmp <- rep(l,each=length(L))
    L.tmp <- rep(L,length(l))
    if(L==0)
      L.tmp <- stats::median(l.tmp)
    
    S <- array(params[1] + params[3]*
                 ifelse(log(L.tmp/l.tmp) < params[2],
                        exp(-(log(L.tmp/l.tmp)-params[2])^2/params[5]),
                        exp(-(log(L.tmp/l.tmp)-params[2])^2/params[4])),
               c(length(L),length(l)))
    
  } else if(tolower(type) == 'gamma'){
    
    S <- array(rep((l/((params[1] - 1)*params[2]*params[3]))^(params[1] -1) *
                     exp(params[1] - 1 - l/(params[2]*params[3])),
                   each = length(L)),
               c(length(L),length(l)))
  } else {
    stop(sprintf('Error in suitability -- %s not defined',type))
  }
  if(to.data.frame){
    dimnames(S) <- list(L=L,l=l)
    S <- as.data.frame.table(S,responseName = 'suit')
    if(normalize)
      S$suit <- S$suit/max(S$suit)
    S$L <- as.numeric(as.character(S$L))
    S$l <- as.numeric(as.character(S$l))
  } else {
    dimnames(S) <- list(sprintf('Pred_length_%s',L),
                        sprintf('Prey_length_%s',l))
  }
  return(S)
}

