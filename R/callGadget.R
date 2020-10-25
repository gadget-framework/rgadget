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
##' @param PBS Logical, should, instead of running gadget directly,
##' a pbs script be
##' generated that can be submitted to a cluster queue (defaults to FALSE).
##' @param qsub.script The name of the qsub script that can be generated, if
##' desired. As with the PBS script R tries to set the permission to 777
##' (not wether or not this works on windows).
##' @param PBS.name Name of the pbs script (.sh will be appended).
##' @param qsub.output The directory where the output from the script is stored
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
                       PBS=FALSE,
                       qsub.script=NULL,
                       PBS.name='run',
                       qsub.output='output',
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
  
  if(!PBS){
    run.history <- try(system2(gadget.exe,
                               args = switches,
                               stdout = FALSE,
                               stderr = ignore.stderr))
  } else {
    if(file.exists(sprintf('%s.sh',PBS.name))){
      write.unix(paste(gadget.exe,switches), f=sprintf('%s.sh',PBS.name),append=TRUE)
      qsub.script <- NULL
    } else {
      PBS.header <-
        paste('#!/bin/bash',
              sprintf('# job file for pbs queue created by Rgadget at %s',
                      date()),
              '# Copy evironment, join output and error, medium queue:',
              '#PBS -V',
              '#PBS -j oe',
              '#PBS -q medium',
              '#PBS -l cput=60:00:00',
              '#PBS -m n',
              '',
              '# Go to the directory where the job was submitted from',
              'cd $PBS_O_WORKDIR',
              '',
              '# run gadget',
              sep='\n')
      
      PBS.script <- paste(PBS.header,
                          paste(gadget.exe,switches),
                          sep='\n')
      write.unix(PBS.script, f=sprintf('%s.sh',PBS.name))
      Sys.chmod(sprintf('%s.sh',PBS.name),mode = '0777')
      if(!is.null(qsub.script)){
        dir.create(qsub.output)
        qsub.string <-
          sprintf('# %1$s\nqsub -N gadget-%2$s -o %3$s/%4$s.txt %2$s.sh \n',
                  date(),PBS.name,qsub.output,gsub('/','.',PBS.name))
        if(file.exists(qsub.script)){
          write.unix(qsub.string,f=qsub.script,append=TRUE)
        } else {
          header <-
            paste('#!/bin/bash',
                  sprintf('# created by Rgadget at %s',date()),
                  sep='\n')
          write.unix(paste(header,qsub.string,sep='\n'),f=qsub.script)
          Sys.chmod(qsub.script,mode = '0777')
        }
      }
    }
    
    run.history <- NULL
  }
  
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
  if('data.frame' %in% class(params.in)){
    tmp <- tempfile()
    params.in %>% write.gadget.parameters(file = tmp)
    params.in <- tmp
  }
  
  Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
  callGadget(s=1,i=params.in,p=params.out,o=lik.out,main = attr(path,'mainfile'),...)
  Sys.setenv(GADGET_WORKING_DIR = '.')
  return(path)
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
gadget_optimize <- function(path='.',params.in = NULL, params.out = NULL, control = NULL,...){
  if('data.frame' %in% class(params.in)){
    tmp <- tempfile()
    params.in %>% write.gadget.parameters(file = tmp)
    params.in <- tmp
  }
  
  if('gadgetfile' %in% class(control)){
    control %>% write.gadget.file(path)
  }
  
  if(is.null(params.in)){
    stop('No input parameter file specified')
  }
  
  if(is.null(params.out)){
    warning('No params.out file specified, will default to "params.out"')
  }
  
  Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
  callGadget(l=1,
             i = params.in,
             p = params.out,
             main = attr(path,'mainfile'),
             opt = attr(control,'file_name'),
             ...)
  Sys.setenv(GADGET_WORKING_DIR = '.')
  return(path)
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
#' 
#' gd <- gadget.variant.dir('test')
#' gadgetfile('test',components = list(test = list(a=1))) %>% 
#' write.gadget.file(gd)
#' 
#' gd_to_unix(gd)
#' 
gd_to_unix <- function(gd){
  list.files(path = gd, full.names = TRUE,recursive = TRUE) %>% 
    map(function(x){
      txt <- readLines(x)
      x <- file(x, open = 'wb')
      writeLines(txt, x, sep = '\n')
      close(x)
    })
  invisible(1)
}