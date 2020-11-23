
##' Read in the gadget likelihood output.
##' @title Read gadget lik.out
##' @param file string containing the name of the file
##' @param suppress logical, should file errors be suppressed
##' @return a list containing the swicthes (names of variable), weigths
##' (l?kelihood components) and data (dataframe with the parameter values,
##' likelihood component values and the final score.
##' @author Bjarki Thor Elvarsson, Hoskuldur Bjornsson
##' @export
read.gadget.lik.out <- function(file='lik.out',suppress=FALSE){
  if(!file.exists(file)){
    return(NULL)
  }
  lik <-  tryCatch(readLines(file),
                   error = function(e){
                     if(!suppress)
                       print(sprintf('file corrupted -- %s', file))
                     return(NULL)
                   })
  
  i <- grep("Listing of the switches",lik)
  i1 <- grep("Listing of the likelihood components",lik)
  i2 <- grep("Listing of the output from the likelihood",lik)
  
  if(is.null(i)|is.null(i1)|is.null(i2)){
    warning(sprintf('file %s is corrupt',file))
    return(NULL)
  }
  
  switches <- tryCatch(lapply(strsplit(lik[(i+1):(i1-2)],'\t'),unique),
                       error = function(e){
                         if(!suppress)
                           print(sprintf('file corrupted -- %s', file))
                         return(NULL)
                       })
  if(is.null(switches)){
    return(NULL)
  }
  names(switches) <- sapply(switches,function(x) x[1])
  switches <- lapply(switches,function(x) x[-1])
  
  weights <- t(sapply(strsplit(lik[(i1+3):(i2-2)],'\t'),function(x) x))
  weights <- as.data.frame(weights,stringsAsFactors=FALSE)
  weights$V2 <- as.numeric(weights$V2)
  weights$V3 <- as.numeric(weights$V3)
  names(weights) <- c('Component','Type','Weight')
  
  data <- utils::read.table(file,skip=(i2+1))
  names(data) <- c('iteration',names(switches),weights$Component,'score')
  attr(data,'Likelihood components') <- weights$Component
  attr(data,'Parameters') <- names(switches)
  lik.out <- list(switches=switches,weights=weights,data=data)
  class(lik.out) <- c('gadget.lik.out',class(lik.out))
  return(lik.out)
}
