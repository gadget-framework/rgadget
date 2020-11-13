

##' Phased minimization based on variables
##' @name gadget.phasing
##' @title Gadget Phasing
##' @param phase a dataframe where the columns indicate the parameters
##' that are to be optimised in that particular phase
##' @param params.in either a filename or gadget.parameters object
##' containing the initial value for the optimisation.
##' @param main name of the main file used in the optimisation.
##' @param phase.dir output directory
##' @param optinfofile location of the optinfofile
##' @return final optimised parameter values
##' @author Bjarki Thor Elvarsson
##' @export
gadget.phasing <- function(phase,params.in='params.in',main='main',
                           phase.dir='PHASING', optinfofile='optinfofile'){
  dir.create(phase.dir, showWarnings = FALSE)
  if(class(params.in)=='character'){
    params.in <- read.gadget.parameters(params.in)
  } else if(!('gadget.parameters' %in% class(params.in))) {
    stop('params.in is not a valid gadget.parameters object')
  }
  #tmp <- params.in$optimise
  
  for(p in names(phase)){
    print(sprintf('In phase %s',p))
    params.in$optimise <- NULL
    params.in <- merge(params.in,phase[[p]],all.x=TRUE)
    params.in$optimise[is.na(params.in$optimise)] <- 0
    write.gadget.parameters(params.in,sprintf('%s/params.%s',phase.dir,p))
    callGadget(l=1,main=main,i=sprintf('%s/params.%s',phase.dir,p),
               p=sprintf('%s/params.out.%s',phase.dir,p),
               opt=optinfofile)
    callGadget(s=1,main=main,o=sprintf('%s/lik.%s',phase.dir,p),
               i=sprintf('%s/params.out.%s',phase.dir,p))
    params.in <- read.gadget.parameters(sprintf('%s/params.out.%s',phase.dir,p))
  }
  invisible(params.in)
}




#' Analytical retrospective
#'
#' \code{gadget.retro} runs an analytical retrospective model fitting run. 
#' @name gadget.retro
#' @param path location of the Gadget model, all filenames are relative to the path
#' @param main.file name of the main file, defaults to 'main'
#' @param params.file name of the starting parameter value file, defaults to 'params.in'
#' @param optinfofile name of the file containing the optimizer settings, defaults to 'optinfofile'
#' @param num.years number of years (models) should be used, defaults to 5 yeaes
#' @param pre location of the model runs, defaults to 'RETRO'
#' @param iterative logical should the iterative reweighting be used, defaults FALSE
#' @param ... additional parameter passed gadget.iterative
#' @return null
#' @export
#'
#' @examples \dontrun{
#' gadget.retro(iterative =TRUE)
#' }
gadget.retro <- function(path='.',
                         main.file='main',
                         params.file='params.in',
                         optinfofile='optinfofile',
                         num.years=5,
                         pre = 'RETRO',
                         iterative =FALSE,...){
  
  if(!file.exists(paste(path,main.file,sep='/'))){
    stop('No main file found')
  }
  
  if(!file.exists(paste(path,optinfofile, sep = '/'))){
    stop('No optinfo file found')
  }
  
  if(!file.exists(paste(path,params.file, sep = '/'))){
    stop('No parameter file found')
  }
  
  
  main <- read.gadget.file(path,main.file,file_type = 'main',recursive = FALSE)
  
  for(year in 1:num.years){
    Rdir <- gadget.variant.dir(path,variant_dir = sprintf('%s/R%s',pre,year))
    
    attributes(main)$file_name <- 'main'
    main %>% write.gadget.file(Rdir,recursive = FALSE)
    
    gadgettime(main[[1]]$timefile,path) %>% 
      gadget_update(lastyear = .[[1]]$lastyear-year) %>% 
      write.gadget.file(Rdir)
    
    
    #  lik <- gadgetlikelihood(main[['likelihood']]$likelihoodfiles,Rdir) 
    #  attr(lik,'file_config')$mainfile_overwrite <- TRUE
    #  write.gadget.file(lik,Rdir)
  }
  
  Sys.setenv(GADGET_WORKING_DIR=normalizePath(path))
  
  if(iterative){
    run.func <- function(x){
      gadget.iterative(main.file=sprintf('%s/R%s/main',pre,x),
                       params.file = params.file,
                       optinfofile=optinfofile,
                       wgts = sprintf('%s/WGTS.%s',pre,x),
                       ...)
    }
    
  } else {
    run.func <- function(x){
      callGadget(l = 1,
                 main = sprintf('%s/R%s/main',pre,x),
                 i = params.file,
                 p = sprintf('%s/params.retro.%s',pre,x),
                 opt = optinfofile)
      callGadget(s = 1,
                 main = sprintf('%s/R%s/main',pre,x),
                 i = sprintf('%s/params.retro.%s',pre,x),
                 o = sprintf('%s/lik.retro.%s',pre,x))
    }
  }
  parallel::mclapply(1:num.years,run.func, 
                     mc.cores = parallel::detectCores(logical = TRUE))
  
  Sys.unsetenv('GADGET_WORKING_DIR')
}


expL50 <- function(l50,b,l){
  1/(1+exp(-b*(l-l50)))
}

logit <- function(a,b,l){
  p <- exp(a+b*l)
  p/(1+p)
}
