##' This function sets up all necessary switches and calls gadget from R
##' and attempts to read the runtime output from gadget. This has currently
##' only been tested on unix based platforms (but should in principle work on
##' windows, given that gadget can be compiled).
##' The source code for gadget can be obtained from https://github.com/hafro/gadget
##'
##' Gadget is a flexible and powerful tool for creating ecosystem models.
##' The program was developed for modelling marine ecosystems in a fisheries
##' management and biology context, however there is nothing in the program
##' that restricts it to fish , and models have been developed to examine
##' marine mammal populations. Indeed there is nothing to demand that the
##' populations being considered are marine, or even aquatic, in nature.
##' Gadget allows you to include a number of features into your model:
##' One or more species, each of which may be split into multiple stocks;
##' multiple areas with migration between areas; predation between and within
##' species; maturation; reproduction and recruitment; multiple commercial and
##' survey fleets taking catches from the populations.
##' Gadget does two separate, but related things. Firstly it takes a model
##' specification and performs a simulation using that set up. The model
##' specification dictates the form of the equations to be used to describe
##' growth, recruitment, fleet selectivity and so on, and the exact parameters
##' to be used in these equations. Gadget will calculate model population and
##' catches through time for your given set up. Note that to do this it does
##' not use real-world data (except possibly overall catch tonnage). The
##' program then compares various aspects of the modelled catches with
##' real-world data from actual catches, and produces numeric likelihood
##' scores measuring how well the model matched each data set. The program
##' also computes a single overall likelihood score. This is a single number
##' representing the 'goodness of fit' between the simulation and the data.
##' It is worth repeating this point. Gadget runs a complete simulation without
##' reference to any data. It then compares the modelled and real catches, and
##' produces a score evaluating the fit between the two.
##' If Gadget is called upon to optimise a model solution it simply iterates
##' this process, trying different parameter values for each iteration. The
##' 'best fit' will be produced by the parameter set which produces a model
##' with the lowest overall likelihood score. There are several different
##' optimisation methods utilised.
##' @title Call GADGET
##' @param l performs a likelihood (optimising) model run
##' @param s perform a single (simulation) model run
##' @param n perform a network run (using paramin)
##' @param v display version information and exit
##' @param h display this help screen and exit
##' @param i name of the file containing the model parameters
##' @param opt name of the file containing the optimising parameters
##' @param main name of the main file (optional, as default it will
##' look for 'main'
##' @param m name of the file containing additional parameters
##' (optional)
##' @param p name of the file to which the parameter estimates should
##' be output to. Default is 'params.out'
##' @param o name of the file to which the likelihood-output should be
##' saved (optional).
##' @param print Number. print -o output every <number> iterations.
##' @param precision set the precision to <number> in output files
##' @param log Name of the file to which the logging information
##' should be saved.
##' @param printinitial Name of the file to which the initial model
##' information should be saved.
##' @param printfinal Name of the file to which the final model
##' information should be saved.
##' @param gadget.exe path to the gadget executable, if not set, first looks
##' to see if the gadget R package is installed, if that fails uses the system
##' path. .Options$gadget.path will override any given parameter.
##' @param ignore.stderr should error output be ignored
##' @param parallel (experimental) should the parallel option be used
##' @return the run history
##' @export
callGadget <- function(l=NULL,
                       s=NULL,
                       n=NULL,
                       v=NULL,
                       h=NULL,
                       i=NULL,
                       opt=NULL,
                       main=NULL,
                       m=NULL,
                       p=NULL,
                       o=NULL,
                       print=NULL,
                       precision=NULL,
                       log=NULL,
                       printinitial=NULL,
                       printfinal=NULL,
                       gadget.exe='gadget',
                       ignore.stderr=FALSE,
                       parallel=NULL){
  
  if(!is.null(.Options$gadget.path)){
    gadget.exe=.Options$gadget.path
  }
  
  if (gadget.exe == 'gadget' && 'gadget2' %in% utils::installed.packages()[,1]) {
    # gadget package available, use that over searching system path
    gadget.exe=gadget2::gadget_binary()
  }
  
  switches <- paste(ifelse(is.null(l),'','-l'),
                    ifelse(is.null(s),'','-s'),
                    ifelse(is.null(n),'','-n'),
                    ifelse(is.null(v),'','-v'),
                    ifelse(is.null(h),'','-h'),
                    ifelse(is.null(i),'',paste('-i',i)),
                    ifelse(is.null(opt),'',paste('-opt',opt)),
                    ifelse(is.null(main),'',paste('-main',main)),
                    ifelse(is.null(m),'',paste('-m',m)),
                    ifelse(is.null(p),'',paste('-p',p)),
                    ifelse(is.null(o),'',paste('-o',o)),
                    ifelse(is.null(print),'',paste('-print',print)),
                    ifelse(is.null(precision),'',paste('-precision',precision)),
                    ifelse(is.null(log),'',paste('-log',log)),
                    ifelse(is.null(printinitial),'',
                           paste('-printinitial',printinitial)),
                    ifelse(is.null(printfinal),'',
                           paste('-printfinal',printfinal)),
                    ifelse(is.null(parallel),'',
                           paste('-parallel',parallel)))
  
  
  run.history <- system2(gadget.exe,
                         args = switches,
                         stdout = FALSE,
                         stderr = ignore.stderr)
  
  
  invisible(run.history)
}




#' Evaluate a gadget model 
#'
#' @param path locatoin of the gadget model
#' @param params.in parameter input file
#' @param params.out parameter output file
#' @param lik.out likelihood output file
#' @param ... additional arguments passed on to callGadget
#'
#' @return path
#' @export
gadget_evaluate <- function(path='.',params.in = NULL, params.out = NULL, lik.out = NULL,...){
  path <- variant_append_settings(path, params.in, params.out)
  Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
  callGadget(s=1,
             i = attr(path, 'params_in'),
             p = attr(path, 'params_out'),
             main = attr(path,'mainfile'),
             o=lik.out,...)
  Sys.setenv(GADGET_WORKING_DIR = '.')
  invisible(path)
} 



#' Evaluate a gadget model 
#'
#' @param path locatoin of the gadget model
#' @param params.in parameter input file
#' @param params.out parameter output file
#' @param control optimization parameters, can be the location of the optinfo file, gadgetfile object or NULL
#' @param ... additional arguments passed on to callGadget
#'
#' @return path
#' @examples 
#' \dontrun{
#' gd <- gadget.variant.dir('model_dir')
#' optinfo <- read.gadget.file('/',system.file('extdata','optinfo',package = 'Rgadget'))
#' gadget_optimize(gd,params.in = 'params.in',params.out = 'params.opt', control = optinfo)
#' }
#' @export
gadget_optimize <- function(path ,params.in = attr(path,'params_in'), params.out = attr(path,'params_out'), control = attr(path,'optinfo'),...){
  path <- variant_append_settings(path, params.in, params.out, control)
  Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
  callGadget(l=1,
             i = attr(path, 'params_in'),
             p = attr(path, 'params_out'),
             main = attr(path,'mainfile'),
             opt = attr(path,'optinfo'),
             ...)
  Sys.setenv(GADGET_WORKING_DIR = '.')
  attr(path, 'params_in') <- attr(path, 'params_out')
  invisible(path)
} 




#' gadget.variant.dir to unix line endings
#' 
#' This function converts all files in the gadget variant directory from windows line endings to unix
#'
#' @param gd gadget.variant.dir
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' gd <- gadget.variant.dir('test')
#' test <- 
#'    gadgetfile('test',components = list(test = list(a=1)))
#' write.gadget.file(test, gd)
#' 
#' gd_to_unix(gd)
#' }
gd_to_unix <- function(gd){
  list.files(path = gd, full.names = TRUE,recursive = TRUE) %>% 
    purrr::map(function(x){
      txt <- readLines(x)
      x <- file(x, open = 'wb')
      writeLines(txt, x, sep = '\n')
      close(x)
    })
  invisible(1)
}



#' Append setting to variant directories
#'
#' More description needed
#'
#' @param gd variant directory
#' @param params.in input parameters
#' @param params.out output
#' @param control optinfo file
#'
#' @return variant dir with appended attributes
variant_append_settings <- function(gd, params.in = attr(gd,'params_in'), params.out = NULL, control = NULL){
  if(!('gadget.variant' %in% class(gd))){
    warning('"path" is not a variant directory, cast to variant.dir')
    gd <- gadget.variant.dir(gd)
  }
  
  
  if(is.null(params.in)){
    warning('No input parameters specified, will run with default values')
  }
  
  if(is.null(params.out)){
    warning(paste0('No params.out file specified, will default to "params.out.', Sys.Date(),'"'))
    params.out <- variant_within_path(gd,paste0('params.out.',Sys.Date()))
  } else {
    params.out <- variant_strip_path(gd,params.out)
  }
  
  if('data.frame' %in% class(params.in)){
    tmp <- variant_within_path(gd,paste0('params.in.',Sys.Date()))
    params.in %>% write.gadget.parameters(file = variant_full_path(tmp))
    params.in <- tmp
  }
  
  attr(gd, 'params_in') <- params.in
  attr(gd, 'params_out') <- params.out  
  
  if('gadgetfile' %in% class(control)){
    attr(control, 'file_name') <- paste0('optinfo.', Sys.Date())
    control %>% write.gadget.file(gd)
    attr(gd,'optinfo') <- variant_within_path(gd,attr(control, 'file_name'))
  } else {
    attr(gd,'optinfo') <- control
  }
  
  return(gd)  
}

variant_within_path <- function(gd, file){
  if(!is.null(attr(gd,'variant_dir'))) paste(attr(gd,'variant_dir'), file, sep = '/') else file
}


