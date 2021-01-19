
##' Call paramin
##' 
##' Paramin runs the optimisation routines in parallel across a network of processors 
##' which will (hopefully) result in a much faster optimisation run. The current version 
##' of paramin is implemented with MPI, Message Passing Interface, which handles all the
##' message passing between the master process, paramin, and the slave processes, which 
##' are Gadget simulation runs. The setup is very similar to a normal Gadget run, with 
##' all the Gadget input files are the same, it's only the optimisation execution that differs.
##' @title Call paramin
##' @param i input file
##' @param func function name
##' @param opt location of the optinfofile
##' @param network location of the network file
##' @param o output filename
##' @param scale deprecated
##' @param condor deprecated
##' @param paramin.exe location of the paramin binary
##' @return null
##' @author Bjarki Thor Elvarsson
callParamin <- function(i='params.in',
                        func='gadget -s -n',
                        opt='optinfo',
                        network='network',
                        o='params.out',
                        scale=NULL,
                        condor=NULL,
                        paramin.exe='paramin'){
  switches <- paste(ifelse(is.null(i),'',paste('-i',i)),
                    ifelse(is.null(opt),'',paste('-opt',opt)),
                    ifelse(is.null(o),'',paste('-o',o)),
                    ifelse(is.null(scale),'',paste('-scale',scale)),
                    ifelse(is.null(condor),'','-condor'),
                    ifelse(is.null(network),'',paste('-network',network)),
                    ifelse(is.null(func),'',paste('-function',func)))
  run.string <- paste(paramin.exe,switches)
  run.history <- try(system(run.string,intern=TRUE,ignore.stderr=TRUE))
  invisible(run.history)
}


##' An implementation of the iterative reweigthing of likelihood
##' components in gadget. It analyzes a given gadget model and, after
##' a series of optimisations where each likelihood component is
##' heavily weigthed, suggests a weigthing for the components based on
##' the respective variance.  If one (or more) components, other than
##' understocking and penalty, are 0 then the gadget optimisation with
##' the final weights will not be completed.
##'
##' In Taylor et. al an objective reweighting scheme for likelihood
##' components is described for cod in Icelandic waters. The authors
##' nota that the issue of component weighting has been discussed for
##' some time, as the data sources have different natural scales (e.g
##' g vs. kg) that should not affect the outcome. A simple heuristic,
##' where the weights are the inverse of the initial sums of squares
##' for the respective component resulting in an initials score equal
##' to the number of components, is therfor often used. This has the
##' intutitive advantage of all components being normalised. There is
##' however a drawback to this since the component scores, given the
##' initial parametrisation, are most likely not equally far from
##' their respective optima resulting in sub-optimal weighting.  The
##' iterative reweighting heuristic tackles this problem by optimising
##' each component separately in order to determine the lowest
##' possible value for each component. This is then used to determine
##' the final weights.  The resoning for this approach is as follows:
##' Conceptually the likelihood components can be thought of as
##' residual sums of squares, and as such their variance can be
##' esimated by dividing the SS by the degrees of freedom. The optimal
##' weighting strategy is the inverse of the variance.  Here the
##' iteration starts with assigning the inverse SS as the initial
##' weight, that is the initial score of each component when
##' multiplied with the weight is 1. Then an optimisation run for each
##' component with the intial score for that component set to
##' 10000. After the optimisation run the inverse of the resulting SS
##' is multiplied by the effective number of datapoints and used as
##' the final weight for that particular component.  The effective
##' number of datapoints is used as a proxy for the degrees of freedom
##' is determined from the number of non-zero datapoints. This is
##' viewed as satisfactory proxy when the dataset is large, but for
##' smaller datasets this could be a gross overestimate. In
##' particular, if the surveyindices are weigthed on their own while
##' the yearly recruitment is esimated they could be overfitted. If
##' there are two surveys within the year Taylor et. al suggest that
##' the corresponding indices from each survey are weigthed
##' simultaneously in order to make sure that there are at least two
##' measurement for each yearly recruit, this is done through
##' component grouping which is implemented. Another approach, which
##' is also implemented, for say a single survey fleet the weight for
##' each index component is estimated from a model of the form
##' \deqn{\log(I_{lts}) = \mu + Y_t + \lambda_l + \Sigma_s +
##' \epsilon_{lts}}{% log(I_lts) = mu + Y_t + lambda_l + Sigma_s +
##' e_lts} where the residual term, \eqn{\epsilon_{lts}}{e_lts}, is
##' independent normal with variance
##' \eqn{\sigma_{ls}^2}{sigma_ls^2}. The inverse of the estimated
##' variance from the above model as the weights between the
##' surveyindices.  After these weights have been determined all
##' surveyindices are weighted simultaneously.
##' @name gadget.iterative
##' @title Iterative reweighting for Gadget models
##' @param main.file a string containing the location of the main file
##' @param gadget.exe path to the gadget executable, if not set, first looks
##' to see if the gadget R package is installed, if that fails uses the system
##' path. .Options$gadget.path will override any given parameter.
##' @param params.file a string containing the location of the input
##' parameters
##' @param rew.sI logical, should survey indices be iteratively
##' reweighted (TRUE) or estimated using a linear model.
##' @param run.final logical should the final optimisation be run
##' (DEBUG)
##' @param resume.final logical should the final optimisation be
##' resumed (DEBUG)
##' @param wgts a string containing the path the folder where the
##' interim weighting results should be stored.
##' @param grouping a list naming the groups of components that should be reweighted together.
##' @param optinfofile optinfofile used in the reweighting
##' @param run.serial should the weighting run be run in parallel (used in
##' bootstrap).
##' @param method linear model or loess smoother used to calculate SI
##' weights outside the gadget model. DEPRECATED
##' @param cv.floor a value for an optional floor for survey indices
##' CV, used to prevent overfitting in the final run.
##' @param comp string vector of names of likelihood components to be
##' used in the model (if NULL use all)
##' @param inverse should inverse selection be used for likelihood
##' components
##' @param gd the gadget model directory
##' @param rew.cik logical, should the catch in kilos components be reweighted (default to FALSE)
##' @param ... pass to callGadget
##' @return a matrix containing the weights of the likelihood
##' components at each iteration (defaults to FALSE).
##' @author Bjarki Þór Elvarsson
##' @export
##' @examples \dontrun{
##' tmp <- gadget.iterative(rew.sI=TRUE,
##'                         grouping=list(sind=c('si2039','si4069','si70110'),
##'                         survey=c('ldist.survey','alkeys.survey'),
##'                         catch=c('ldist.catch','alkeys.catch')),
##'                         params.file='params.base',
##'                         wgts='WGTS')
##' }
gadget.iterative <- function(main.file='main',gadget.exe='gadget',
                             params.file='params.in',
                             rew.sI=TRUE,
                             run.final=TRUE,
                             resume.final=FALSE,
                             wgts = 'WGTS',
                             grouping = NULL,
                             optinfofile='optinfofile',
                             run.serial = FALSE,
                             method = 'lm',
                             cv.floor=NULL,
                             comp=NULL,
                             inverse=FALSE,
                             gd=NULL,
                             rew.cik = FALSE,
                             ...) {
  
  warning('Function deprecated - has been replaced by gadget_iterative_*')
  
  if(!is.null(gd)){
    ## Change the gadget working directory to whatever the gd says it should be
    Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd))
    main.file <- paste(gd,attr(gd,'mainfile'),sep='/')
    wgts <- paste(gd,wgts,sep = '/')
  }
  
  ## Ensure all files exist
  if(!file.exists(main.file)) {
    stop('Main file not found')
  }
  
  if(!file.exists(params.file)) {
    stop('Parameter file not found')
  }
  
  if(!file.exists(optinfofile)) {
    stop('Optinfofile not found')
  }
  
  
  ## store the results in a special folder to prevent clutter
  dir.create(wgts,showWarnings=FALSE)
  
  ## read model
  main <- read.gadget.main(main.file)
  printfile <- NULL
  likelihood <- read.gadget.likelihood(main$likelihoodfiles)
  if(!is.null(comp)){
    likelihood <- get.gadget.likelihood(likelihood,
                                        comp=comp,
                                        inverse = inverse)
  }
  
  ## ensure that grouped components exist
  tmp <- unlist(grouping)
  if(length(tmp) != length(intersect(tmp,likelihood$weights$name))){
    stop('Error - invalid grouping')
  }
  
  ## initial run (to determine the initial run)
  main.init <- main
  main.init$printfiles <- NULL
  main.init$likelihoodfiles <- paste(wgts,'likelihood.init',sep='/')
  write.gadget.likelihood(likelihood,file=paste(wgts,'likelihood.init',sep='/'))
  write.gadget.main(main.init,file=paste(wgts,'main.init',sep='/'))
  callGadget(s=1,main=paste(wgts,'main.init',sep='/'),
             o=paste(wgts,'lik.init',sep='/'),
             i=params.file,gadget.exe=gadget.exe,
             ...)
  
  
  ## degrees of freedom approximated by the number of datapoints
  ## and make sure we only read data from the model period
  time <- read.gadget.file('.',main$timefile,file_type = 'time')
  lik.dat <- read.gadget.data(likelihood,
                              year_range = time[[1]]$firstyear:time[[1]]$lastyear)
  restr <- !(likelihood$weights$type %in%
               c('penalty','understocking','migrationpenalty',
                 if(rew.cik) NULL else 'catchinkilos'))
  SS <- read.gadget.lik.out(paste(wgts,'lik.init',
                                  sep='/'))$data[likelihood$weights$name[restr]]
  ## NB: Internal function, so not Roxygen-ized
  ## Survey indices get special treatment
  ## @title survey index weight
  ## @param lik.dat Likelihood dataset
  ## @return internal weights for the survey index components
  ## @author Bjarki Thor Elvarsson
  sI.weights <- function(lik.dat,method='lm'){
    warning('Estimating survey weigths using a linear model is now deprecated and will be removed at later stage')
    if(method=='lm'){
      dat <- plyr::ldply(lik.dat$dat$surveyindices,
                         function(x) x)
      
      dat$y <- log(dat$number)
      dat$year <- as.factor(dat$year)
      fit <- stats::lm(y~year+length+step,dat)
      weights <- (lik.dat$df$surveyindices -
                    tapply(dat$length,dat$name,function(x) length(unique(x))))/
        tapply(stats::resid(fit),dat$name,function(x) sum(x^2))
    } else {
      weights <- plyr::ldply(lik.dat$dat$surveyindices,
                             function(x){
                               time <- x$year + (x$step-1)/4
                               fit <- stats::predict(stats::loess(log(x$number)~time))
                               length(fit)/sum((fit - log(x$number))^2)
                             })$V1
    }
    names(weights) <- names(lik.dat$dat$surveyindices)
    return(weights)
  }
  
  restr.SI <- subset(likelihood$weights,
                     likelihood$weightstype == 'surveyindices')$name
  if(!rew.sI){
    if(is.null(grouping)){
      grouping <- list(SI=intersect(likelihood$weights$name,restr.SI))
    } else {
      grouping$SI <- intersect(likelihood$weights$name,restr.SI)
    }
    sIw <- sI.weights(lik.dat,method=method)
  }
  run.string <- c(likelihood$weights$name[restr&
                                            !(likelihood$weights$name %in%
                                                unlist(grouping))])
  
  run.string <- as.list(run.string)
  names(run.string) <-
    c(likelihood$weights$name[restr&
                                !(likelihood$weights$name %in%
                                    unlist(grouping))])
  
  run.string <- append(run.string,grouping)
  
  
  ## Base run (with the inverse SS as weights)
  main.base <- main.init
  main.base$likelihoodfiles <- paste(wgts,'likelihood.base',sep='/')
  write.gadget.main(main.base,file=paste(wgts,'main.base',sep='/'))
  likelihood.base <- likelihood
  if(sum(is.infinite(1/unlist(SS))>0)){
    SS <- unlist(SS)
    txt <- 
      names(SS[is.infinite(1/SS)])
    stop(sprintf('Model error, likelihood component %s returns a value of exactly 0\n',txt))
  }
  likelihood.base$weights[names(SS),'weight'] <- 1/as.numeric(SS)
  
  
  ## NB: Internal function, so not Roxygen-ized
  ## Gadget set up stuff, needed for each component
  ## @title run iterative
  ## @param comp likelihood component
  ## @return Sums of squares
  ## @author Bjarki Thor Elvarsson
  run.iterative <- function(comp){
    
    likelihood <- likelihood.base
    which.comp <- likelihood$weights$name %in% comp
    likelihood$weights$weight[which.comp] <-
      10000*likelihood$weights$weight[which.comp]
    comp <- paste(comp,collapse='.')
    print(sprintf('Running %s',comp))
    write.gadget.likelihood(likelihood,
                            file=paste(wgts,
                                       paste('likelihood',comp,sep='.'),sep='/'))
    main <- main.base
    main$likelihoodfiles <- paste(wgts,paste('likelihood',comp,sep='.'),sep='/')
    write.gadget.main(main,file=paste(wgts,paste('main',comp,sep='.'),sep='/'))
    
    callGadget(l=1,
               main=paste(paste(wgts,'main',sep='/'),comp,sep='.'),
               i=params.file,
               p=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               opt=optinfofile,
               gadget.exe=gadget.exe,
               ...)
    callGadget(s=1,
               main=paste(wgts,paste('main',comp,sep='.'),sep='/'),
               i=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               o=paste(wgts,paste('lik',comp,sep='.'),sep='/'),
               gadget.exe=gadget.exe,
               ...)
    print(sprintf('Comp %s completed',comp))
  }
  ##
  if(!resume.final){
    ## run the bloody thing
    if(run.serial){
      res <- lapply(run.string,run.iterative)
    }  else {
      
      res <- parallel::mclapply(run.string,run.iterative,
                                mc.cores = parallel::detectCores(logical = TRUE))
    }
  }
  
  ## Do we want to run the final optimisation (only used for debug purposes,
  ## and the check should be removed in later revisions)
  
  if(run.final){
    res <- plyr::ldply(run.string,
                       function(x){
                         tmp <-
                           read.gadget.lik.out(paste(wgts,
                                                     paste('lik',
                                                           paste(x,collapse='.'),
                                                           sep='.'),
                                                     sep='/'))$data
                         tmp[likelihood$weights$name[restr]]
                       })
    row.names(res) <- res$.id
    res$.id <- NULL
    
    
    run.final <- function(comp){
      print(sprintf('Running %s',comp))
      callGadget(l=1,
                 main=sprintf('%s/main.%s',wgts,comp),
                 i=params.file,
                 p=sprintf('%s/params.%s',wgts,comp),
                 opt=optinfofile,
                 gadget.exe=gadget.exe,
                 ...)
      callGadget(s=1,
                 main=sprintf('%s/main.%s',wgts,comp),
                 i=sprintf('%s/params.%s',wgts,comp),
                 o=sprintf('%s/lik.%s',wgts,comp),
                 gadget.exe=gadget.exe,
                 ...)
      print(sprintf('Comp %s completed',comp))
    }
    
    ## read in the results from previous runs
    SS <- plyr::ldply(names(run.string),
                      function(x)
                        data.frame(group=x,comp=run.string[[x]],
                                   SS=as.numeric(res[x,run.string[[x]]])))
    df <- plyr::ldply(lik.dat$df,function(x) data.frame(df=x,comp=names(x)))
    SS <- dplyr::mutate(plyr::join(SS,df),sigmahat = SS/df)
    SS$comp <- as.character(SS$comp)
    
    ## final run
    write.files <- function(comp,weights){
      if(!is.null(cv.floor)){
        weights$sigmahat[weights$comp %in% restr.SI] <-
          pmax(weights$sigmahat[weights$comp %in% restr.SI],cv.floor)
      }
      
      if(sum(weights$sigmahat == 0) >0){
        warning(sprintf('Perfect fit for component %s, weight 10*df used',
                        weights$comp[weights$sigmahat == 0]))
        weights$sigmahat[weights$sigmahat == 0] <- 0.1
      }
      
      main <- main.base
      main$likelihoodfiles <- sprintf('%s/likelihood.%s',wgts,comp)
      write.gadget.main(main,sprintf('%s/main.%s',wgts,comp))
      
      likelihood <- likelihood.base
      if(sum(is.infinite(1/weights$sigmahat))>0){
        txt <- 
          weights %>% 
          dplyr::filter(is.infinite(1/weights$sigmahat)) %>% 
          .$comp 
        
        stop(sprintf('Model error, likelihood component %s returns a value of exactly 0',txt))
      }
      likelihood$weights[weights$comp,'weight'] <- 1/weights$sigmahat
      
      write.gadget.likelihood(likelihood,
                              file=sprintf('%s/likelihood.%s',wgts,comp))
    }
    
    
    write.files('final',SS)
    write.files('nodf',SS %>% dplyr::mutate(sigmahat=SS))
    
    comp <- c('final','nodf')
    if(!rew.sI){
      SS[restr.SI,'sigmahat'] <- sIw[restr.SI]
    }
    if(!rew.sI){
      write.files('sIw',SS)
      comp <- as.list(c('final','nodf','sIw'))
    }
    
    if(run.serial){
      lapply(comp,run.final)
    } else {
      parallel::mclapply(comp,run.final,
                         mc.cores = parallel::detectCores(logical = TRUE))
    }
    
  } else {
    comp <- NULL
  }
  
  invisible(list(comp=run.string,final=comp,wgts=wgts))
  
}


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
