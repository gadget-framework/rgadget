
##' \code{read.gadget.parameters} reads a gadget parameter file
##' @title Gadget parameters
##' @rdname gadget.parameters
##' @param file parameter file
##' @return dataframe
##' @author Bjarki  Thor Elvarsson
##' @export
read.gadget.parameters <- function(file='params.in'){
  
  raw <- 
    readr::read_lines(file) %>% 
    stringr::str_trim()
  
  if(length(raw) == 0){
    print(sprintf('Error in read.gadget.parameters -- %s cannot be read', file))
    return(NULL)
  }
  
  data <- 
    raw %>% 
    purrr::discard(~grepl('^; *',.))
  
  header <- 
    raw %>% 
    purrr::keep(~grepl('^; *',.))
  
  
  if(grepl("^switches",data[[1]])){
    ## parameters are stored in wide format
    data[[1]] <- 
      gsub('^switches\\s','',data[[1]])
    
    params <- 
      data %>% 
      readr::read_table2() %>% 
      structure(file_format = 'wide')
    
  } else {
    ## parameters stored in long format
    
    
    params <- 
      data %>% 
      readr::read_table2() %>% 
      structure(file_format = 'long')
  }
  
  
  num.func <- function(pre){
    post <- ' function evaluations'
    num <- as.numeric(gsub(post,'',gsub(pre,'',header[grepl(pre,header)])))
    num <- ifelse(length(num) == 0,NA,num)
    return(num)
  }
  
  ## Number of function evaluations
  sim.func.str <- '; Simulated Annealing algorithm ran for '
  sim.pos <- grep(sim.func.str,header)
  
  hook.func.str <- '; Hooke & Jeeves algorithm ran for '
  hook.pos <- grep(hook.func.str,header)
  
  bfgs.func.str <- '; BFGS algorithm ran for '
  bfgs.pos <- grep(bfgs.func.str,header)
  
  ## final likelihood values from each component
  lik.func <- function(i){
    lik <- as.numeric(gsub('; and stopped when the likelihood value was ','',
                           header[i]))
    lik <- ifelse(length(lik) == 0,NA,lik)
    return(lik)
  }
  
  ## convergence
  conv.func <- function(i){
    error <- '; because an error occured during the optimisation'
    converged <- '; because the convergence criteria were met'
    maxiter <-
      '; because the maximum number of function evaluations was reached'
    msg <- ifelse(header[i]==error,'Error in optimisation',
                  ifelse(header[i]==converged,'Convergence criteria were met',
                         ifelse(header[i]==maxiter,
                                'Maximum number of iterations',
                                'No information')))
    ifelse(length(msg)==0,NA,msg)
  }
  
  safe_dates <- 
    purrr::quietly(~gsub('; Gadget version .+ running on [A-Za-z].+','',.) %>% 
                     lubridate::parse_date_time('%a %b! %d! %H!:%M!:%S! %Y!'))
  
  date <- safe_dates(header[1]) %>% .$result
  
  
  tmp <- list(simann=data.frame(numFunc=num.func(sim.func.str),
                                lik.val=lik.func(sim.pos+1),
                                convergence=conv.func(sim.pos+2),
                                stringsAsFactors=FALSE),
              hooke=data.frame(numFunc=num.func(hook.func.str),
                               lik.val=lik.func(hook.pos+1),
                               convergence=conv.func(hook.pos+2),
                               stringsAsFactors=FALSE),
              bfgs=data.frame(numFunc=num.func(bfgs.func.str),
                              lik.val=lik.func(bfgs.pos+1),
                              convergence=conv.func(bfgs.pos+2),
                              stringsAsFactors=FALSE))
  class(params) <- c('gadgetparameters',class(params))
  attr(params,'optim.info') <- tmp
  attr(params,'date') <- date 
  
  return(params)
}

##' \code{write.gadget.parameters} writes gadget input parameters
##' @rdname gadget.parameters 
##' @param params params dataframe
##' @param file a string naming the file to write to
##' @return a string containing the text of the params file (if desired)
##' @author Bjarki Þór Elvarsson
##' @export
write.gadget.parameters <- function(params,file='params.out'){
  input.text <-
    paste("; input file for the gadget model",
          "; created by from Rgadget",
          sprintf('; %s - %s',file,date()),
          paste(names(params),collapse='\t'),
          sep='\n')
  
  if(attr(params,'file_format')=='wide')
    write.unix(paste(c('switches',names(params)),collapse='\t'),f=file)
  else
    write.unix(input.text,file)
  write.gadget.table(params,file=file,
                     quote=FALSE, row.names=FALSE, col.names=FALSE,
                     append=TRUE, sep="\t")
}


#' Initial parameter guess
#'
#' This is a convenience function that allows the editing of parameter files by variable (switch) names. 
#' 
#' @rdname gadget.parameters
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
init_guess <- function(dat,pattern='', value = 0,  lower = -999, upper = 999, optimise=0){

    dat[grepl(pattern,dat$switch),'value'] <- value
    dat[grepl(pattern,dat$switch),'upper'] <- upper
    dat[grepl(pattern,dat$switch),'lower'] <- lower
    dat[grepl(pattern,dat$switch),'optimise'] <- optimise
    return(dat)
}


#' wide parameter files
#'
#' @rdname gadget.parameters
#' @param dat gadget parameters object
#' @param value data.frame update to the parameter matrix (assumes same row count)
#'
#' @return gadget parameters object
#' @export
wide_parameters <- function(dat,value){
  if(attr(dat,'file_format')=='long'){
    dat <- 
      dat %>% 
      dplyr::select(.data$switch,.data$value) %>% 
      tidyr::spread(.data$switch,.data$value) %>% 
      dplyr::slice(rep(1,nrow(value)))     
  } 
  
  dat <- 
    dat %>% 
    dplyr::select(-c(names(value))) %>% 
    dplyr::bind_cols(value) %>% 
    structure(file_format='wide')
  
  return(dat)
}
