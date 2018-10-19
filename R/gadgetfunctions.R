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
##' @param gadget.exe path to the gadget executable, if it is not in
##' the path or defined by .Options$gadget.path
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
                       ignore.stderr=TRUE,
                       parallel=NULL){
  
  if(!is.null(.Options$gadget.path)){
    gadget.exe=.Options$gadget.path
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
  
  run.string <- paste(gadget.exe,switches)
  if(!PBS){
    run.history <- try(system(run.string,intern=TRUE,
                              ignore.stderr=ignore.stderr))
  } else {
    if(file.exists(sprintf('%s.sh',PBS.name))){
      write.unix(run.string, f=sprintf('%s.sh',PBS.name),append=TRUE)
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
                          run.string,
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
##' @title Iterative reweighting for Gadget models
##' @param main.file a string containing the location of the main file
##' @param gadget.exe a string containing the location of the gadget
##' executable
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
##' @param PBS Logical, should the gadget runs be defined to be run in pbs
##' scripts (defaults to FALSE).
##' @param qsub.script Name of cluster submission script.
##' @param run.base should the base (inverse initial SS) parameters be estimated
##' @param run.serial should the weighting run be run in parallel (used in
##' bootstrap).
##' @param method linear model or loess smoother used to calculate SI
##' weights outside the gadget model.
##' @param cv.floor a value for an optional floor for survey indices
##' CV, used to prevent overfitting in the final run.
##' @param comp string vector of names of likelihood components to be
##' used in the model (if NULL use all)
##' @param inverse should inverse selection be used for likelihood
##' components
##' @param cl cluster references, used to parallelize this function on
##' Windows or on a actual cluster. Make sure that Rgadget is loaded on all nodes.
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
                             PBS = FALSE,
                             qsub.script = NULL,
                             run.base=FALSE,
                             run.serial = FALSE,
                             method = 'lm',
                             cv.floor=NULL,
                             comp=NULL,
                             inverse=FALSE,
                             cl=NULL,
                             gd=list(dir='.',rel.dir='WGTS'),
                             ...) {
  
  ## Change the gadget working directory to whatever the gd says it should be
  Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
  
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
  if(!is.null(main$printfiles)) {
    printfile <- read.gadget.printfile(main$printfiles)
  } else if(!is.null(main$printfile)){
    printfile <- read.gadget.printfile(main$printfile)
  } else {
    printfile <- NULL
  }
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
                 'catchinkilos'))
  SS <- read.gadget.lik.out(paste(wgts,'lik.init',
                                  sep='/'))$data[likelihood$weights$name[restr]]
  ##' Survey indices get special treatment
  ##' @title survey index weight
  ##' @param lik.dat Likelihood dataset
  ##' @return internal weights for the survey index components
  ##' @author Bjarki Thor Elvarsson
  sI.weights <- function(lik.dat,method='lm'){
    if(method=='lm'){
      dat <- plyr::ldply(lik.dat$dat$surveyindices,
                         function(x) x)
      
      dat$y <- log(dat$number)
      dat$year <- as.factor(dat$year)
      fit <- lm(y~year+length+step,dat)
      weights <- (lik.dat$df$surveyindices -
                    tapply(dat$length,dat$name,function(x) length(unique(x))))/
        tapply(resid(fit),dat$name,function(x) sum(x^2))
    } else {
      weights <- plyr::ldply(lik.dat$dat$surveyindices,
                             function(x){
                               time <- x$year + (x$step-1)/4
                               fit <- predict(loess(log(x$number)~time))
                               length(fit)/sum((fit - log(x$number))^2)
                             })$V1
    }
    names(weights) <- names(lik.dat$dat$surveyindices)
    return(weights)
  }
  
  restr.SI <- subset(likelihood$weights,type == 'surveyindices')$name
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
  likelihood.base$weights[names(SS),'weight'] <- 1/as.numeric(SS)
  
  
  ##' Gadget set up stuff, needed for each component
  ##' @title run iterative
  ##' @param comp likelihood component
  ##' @return Sums of squares
  ##' @author Bjarki Thor Elvarsson
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
    if(!is.null(printfile)){
      write.gadget.printfile(printfile,
                             sprintf('%s/%s.%s',wgts,'printfile',comp),
                             sprintf('%s/out.%s',wgts,comp))
      main$printfiles <- sprintf('%s/%s.%s',wgts,'printfile',comp)
      dir.create(sprintf('%s/out.%s',wgts,comp),showWarnings=FALSE)
    }
    main$likelihoodfiles <- paste(wgts,paste('likelihood',comp,sep='.'),sep='/')
    write.gadget.main(main,file=paste(wgts,paste('main',comp,sep='.'),sep='/'))
    
    callGadget(l=1,
               main=paste(paste(wgts,'main',sep='/'),comp,sep='.'),
               i=params.file,
               p=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               opt=optinfofile,
               gadget.exe=gadget.exe,
               PBS=PBS,
               qsub.script=qsub.script,
               PBS.name=paste(wgts,comp,sep='/'),
               ...)
    callGadget(s=1,
               main=paste(wgts,paste('main',comp,sep='.'),sep='/'),
               i=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               o=paste(wgts,paste('lik',comp,sep='.'),sep='/'),
               gadget.exe=gadget.exe,
               PBS=PBS,
               PBS.name=paste(wgts,comp,sep='/'),
               ...)
    print(sprintf('Comp %s completed',comp))
  }
  ##
  if(!resume.final){
    ## run the bloody thing
    if(run.serial){
      res <- lapply(run.string,run.iterative)
    } else if(!is.null(cl)){
      ## nasty hack, I know (stolen from http://www.r-bloggers.com/implementing-mclapply-on-windows-a-primer-on-embarrassingly-parallel-computation-on-multicore-systems-with-r/ )
      
      loaded.package.names <- c(
        ## Base packages
        sessionInfo()$basePkgs,
        ## Additional packages
        names( sessionInfo()$otherPkgs ))
      
      this.env <- environment()
      while( identical( this.env, globalenv() ) == FALSE ) {
        clusterExport(cl,
                      ls(all.names=TRUE, env=this.env),
                      envir=this.env)
        this.env <- parent.env(environment())
      }
      ## repeat for the global environment
      clusterExport(cl,
                    ls(all.names=TRUE, env=globalenv()),
                    envir=globalenv())
      
      ## Load the libraries on all the clusters
      ## N.B. length(cl) returns the number of clusters
      parLapply( cl, 1:length(cl), function(xx){
        lapply(loaded.package.names, function(yy) {
          ## N.B. the character.only option of 
          ##      require() allows you to give the 
          ##      name of a package as a string. 
          require(yy , character.only=TRUE)})
      })
      
      
      res <- parLapply(cl,run.string,run.iterative)
    } else {
      
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
                 PBS=PBS,
                 PBS.name=paste(wgts,comp,sep='/'),
                 qsub.script=qsub.script,
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
      if(!is.null(printfile)){
        write.gadget.printfile(printfile,
                               sprintf('%s/%s.%s',wgts,'printfile',comp),
                               sprintf('%s/out.%s',wgts,comp))
        main$printfiles <- sprintf('%s/%s.%s',wgts,'printfile',comp)
        dir.create(sprintf('%s/out.%s',wgts,comp),showWarnings=FALSE)
      }
      main$likelihoodfiles <- sprintf('%s/likelihood.%s',wgts,comp)
      write.gadget.main(main,sprintf('%s/main.%s',wgts,comp))
      
      likelihood <- likelihood.base
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
    } else if(Sys.info()[['sysname']]=='Windows'){
      cl <- parallel::makeCluster(parallel::detectCores(logical=TRUE))
      res <- parallel::parLapply(cl,run.string,run.final)
      parallel::stopCluster(cl)
    } else {
      parallel::mclapply(comp,run.final,
                         mc.cores = parallel::detectCores(logical = TRUE))
    }
    
  } else {
    comp <- NULL
  }
  
  invisible(list(comp=run.string,final=comp,wgts=wgts))
  
}




##' This function implements a crude sensitivity analysis of a gadget
##' simulation The test is run by changing each of the variables in
##' your model by up to +/- some percentage of the initial
##' value. Often a higher resolution near the optimum is desired than
##' is requried elsewhere.
##' @title Gadget sensitivity
##' @param file name of the input file with the initial point
##' @param outer.range The outer ranges of the parameter value
##' considered, defined in terms of percentages.
##' @param outer.stepsize The increments/stepsize within in the outer range.
##' @param inner.range Inner range where the finer mesh should be used
##' @param inner.stepsize Inner stepsize.
##' @param opt Will we be looking at only the optimized variables, or
##' all of them?
##' @param vars.all (logical) Will we be looking at all variables or
##' just some?
##' @param var.names If only a few, which ones will they be? Can be
##' blank if we are using all variables
##' @param gadget.exe name of the gadget executable
##' @param sens.in name of the resulting gadget input file
##' @param lik.out a string containing the name of the likelihood
##' output file
##' @param within.bounds should gadget be restricted to
##' @param main.file string naming the gadget main file used
##' @param sens.dir a string naming the folder where the result and
##' temporary files are stored. The funciton will create the folder if
##' it has not already been created.
##' @param calc.full (USE WITH CARE) should the the full hypercube of
##' function values be calculated. Using this switch will increase the
##' computation time required exponentially.
##' @param range The range of the sensitivity check
##' @param stepsize The stepsize used
##' @return results from lik.out
##' @author Bjarki Thor Elvarsson
##' @export
gadget.sensitivity <- function(file='params.out',
                               outer.range=0.5,
                               outer.stepsize=0.05,
                               inner.range=0.05,
                               inner.stepsize=0.01,
                               opt=TRUE,
                               vars.all=TRUE,
                               var.names='',
                               gadget.exe='gadget',
                               sens.in='sens.in',
                               lik.out='lik.sens',
                               within.bounds=TRUE,
                               main.file='main',
                               sens.dir = 'SENS',
                               calc.full = FALSE
){
  dir.create(sens.dir,showWarnings=FALSE)
  lik.out <- paste(sens.dir,lik.out,sep='/')
  params <- read.gadget.parameters(file=file)
  p.range <- 1 + sort(unique(c(seq(-outer.range,outer.range,by=outer.stepsize),
                               seq(-inner.range,inner.range,
                                   by=inner.stepsize))))
  restr <- TRUE
  if(opt){
    restr <- restr&(params$optimise==1)
  }
  if(!vars.all){
    restr <- restr&(params$switch %in% var.names)
  }
  num.points <- sum(restr)*length(p.range)
  param.table <- rep(params$value,each=num.points)
  dim(param.table) <- c(num.points,length(params$value))
  param.table <- as.data.frame(param.table)
  names(param.table) <- params$switch
  row.names(param.table) <- paste(rep(params$switch[restr],
                                      each=length(p.range)),
                                  1:length(p.range),sep='.')
  seat <- 0
  if(!calc.full){
    for(name in params$switch[restr]){
      param.res <- params$switch==name
      if(within.bounds){
        param.table[[name]][seat+1:length(p.range)] <-
          pmax(pmin(params$upper[param.res],
                    p.range*params$value[param.res]),
               params$lower[param.res])
      } else {
        param.table[[name]][seat+1:length(p.range)] <-
          p.range*params$value[param.res]
      }
      seat <- seat+length(p.range)
    }
  } else {
    param.table <-
      within(ls(),
             for(name in params$switch[restr]){
               param.res <- params$switch==name
               if(within.bounds){
                 assign(name,
                        pmax(pmin(params$upper[param.res],
                                  p.range*params$value[param.res]),
                             params$lower[param.res]))
               } else {
                 assign(name,p.range*params$value[param.res])
               }
             })
    param.table$name <- NULL
    param.table <- expand.grid(param.table)
  }
  param.table <- unique(param.table)
  header <- paste('switches',paste(names(param.table),collapse='\t'),sep='\t')
  write.unix(header,f=sens.in)
  write.gadget.table(param.table,file=sens.in,col.names=FALSE,append=TRUE,
                     quote=FALSE,sep='\t',row.names=FALSE)
  main <- read.gadget.main(main.file)
  main$printfiles <- NULL
  write.gadget.main(main,file=sprintf('%s/%s.sens',sens.dir,main.file))
  callGadget(s=TRUE,i=sens.in,o=lik.out,
             p=paste(sens.dir,'sens.out',sep='/'),
             main=sprintf('%s/%s.sens',sens.dir,main.file),
             gadget.exe=gadget.exe)
  lik.sens <- read.gadget.lik.out(lik.out)
  sens.data <- lik.sens$data
  sens.data$parameter <- row.names(param.table)
  #  attr(sens.data,'params') <- params
  #  attr(sens.data,'comps') <-
  class(sens.data) <- c('gadgetSens',class(sens.data))
  return(sens.data)
}

##' Plot the likelihood profile plot by parameter for a particular likelihood component
##'
##' @title Plot likelihood sensitivity
##' @param sens resulsts from gadget sensitivity
##' @param comp what component to plot, defaults to overall score
##' @return ggplot object
##' @export
plot.gadgetSens <- function(sens,comp='score'){
  
  sens$parameter <- sapply(strsplit(sens$parameter,'.',fixed=TRUE),
                           function(x) paste(x[-length(x)],collapse='.'))
  lik.comps <- attr(sens,'Likelihood components')
  if(!(comp %in% c(lik.comps,'score')))
    stop(sprintf('Component %s not found in lik.comps',comp))
  params <- attr(sens,'Parameters')
  tmp <- plyr::ddply(sens,'parameter',
               function(x){
                 tmp <- cbind(x[x$parameter[1]],x[comp])
                 names(tmp) <- c('Value','score')
                 tmp
               })
  plo <- ggplot(tmp, aes(Value,score)) +
    geom_line() +
    facet_wrap(~parameter,scale='free') +
    xlab('') + ylab('') +
    opts(axis.text.x=theme_text(angle=-90,hjust=0))
  
  
  return(plo)
  
}

##' Phased minimization based on variables
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




##' Calculate yield per recruit of a stock in a Gadget model
##'
##' \code{gadget.ypr} returns the results from a yield per recruit simulation. 
##' The function sets up a new Gadget run based on a given model 
##' and parameter settings where the initial conditions and recruiment 
##' parameters are all set to zero appart from one year class which is 
##' set to a nominal value (10000 individuals). The model is the run with 
##' a range of fishing effort settings. The fishing effort is formulated in
##' relation to linearfleets whose selection is the same as the one in the 
##' fleets in the model. The user can specify what fleets will be used in
##' in the yield per recruit simulation.
##' @title Gadget Yield per Recruit
##' @param params.file Parameter file for the gagdet model
##' @param main.file Main file for the gagdet model
##' @param effort The range of fishing mortality
##' @param begin Start year of the simulation
##' @param end End year of the simulation
##' @param age.range at what age range should the YPR be calculated
##' @param fleets Data frame comtaining the fleet names and ratio in
##' future catches
##' @param check.previous check if the analysis have been done before
##' @param save.results should the results be saved?
##' @param gd gadget directory object
##' @return a list containing the yield per recruit by F, estimate of
##' Fmax and F0.1
##' @author Bjarki Thor Elvarsson
##' @export
##' @examples \dontrun{
##' ypr <- gadget.ypr(ssb.stock = 'codmat',
##'                   params.file = 'WGTS/params.final')
##' plot(ypr)
##' }
gadget.ypr <- function(params.file = 'params.in',
                       main.file = 'main',
                       effort = seq(0, 1, by=0.01),
                       begin=1990,end=2020,
                       age.range=NULL,
                       fleets = data.frame(fleet='comm',ratio=1),
                       ssb.stock=NULL,
                       mat.par = NULL,
                       check.previous = FALSE,
                       save.results = TRUE,
                       gd=list(dir='.',rel.dir='YPR')){
  ypr <- paste(gd$dir,gd$rel.dir,sep='/')
  
  ## ensure that files exist
  if(!file.exists(params.file)) {
    stop('Parameter file not found')
  }
  
  if(!file.exists(main.file)) {
    stop('Main file not found')
  }
  
  ## model setup
  if(check.previous){
    if(file.exists(sprintf('%s/ypr.Rdata',ypr))){
      load(sprintf('%s/ypr.Rdata',ypr))
      return(res)
    }
  }
  
  ## File I/O
  dir.create(ypr,showWarnings = FALSE, recursive = TRUE)
  dir.create(sprintf('%s/Aggfiles',ypr),
             showWarnings = FALSE, recursive = TRUE)
  main <- read.gadget.main(main.file)
  stocks <- read.gadget.stockfiles(main$stockfiles)
  fleet <- read.gadget.fleet(main$fleetfiles)
  params <- read.gadget.parameters(params.file)
  time <- read.gadget.time(main$timefile)
  area <- read.gadget.area(main$areafile)
  
  ## basic setup
  time$lastyear <-  end
  time$firstyear <- begin
  time$laststep <- length(time$notimesteps)
  time$firststep <- 1
  
  time.grid <- expand.grid(year = time$firstyear:time$lastyear,
                           step = 1:length(time$notimesteps),
                           area = area$areas)
  
  area$temperature <- mutate(time.grid,
                             temperature = 5)
  
  main$areafile <- sprintf('%s/area',ypr)
  write.gadget.area(area,file=sprintf('%s/area',ypr))
  write.unix(sprintf('allareas %s',paste(area$areas,collapse=' ')),
             f=sprintf('%s/Aggfiles/allareas.agg',ypr))
  
  fleet <- plyr::llply(fleet,
                       function(x){
                         tmp <- subset(x,fleet %in% fleets$fleet)
                       })
  fleet$fleet <- plyr::mutate(fleet$fleet,
                              multiplicative = '1#effort',
                              amount = sprintf('%s/fleet.ypr', ypr),
                              type = 'linearfleet')
  
  fleet.predict <- plyr::ddply(fleets,'fleet',function(x){
    tmp <- plyr::mutate(time.grid,
                        ratio = x$ratio)
    return(tmp)
  })
  
  
  write.gadget.table(fleet.predict[c('year','step','area','fleet','ratio')],
                     file=sprintf('%s/fleet.ypr',ypr),
                     col.names=FALSE,row.names=FALSE,
                     quote = FALSE)
  
  main$fleetfiles <- sprintf('%s/fleet', ypr)
  write.gadget.fleet(fleet,file=sprintf('%s/fleet', ypr))
  
  write.gadget.time(time,file=sprintf('%s/time.ypr',ypr))
  main$timefile <- sprintf('%s/time.ypr',ypr)
  
  ## basic printfile
  
  print.txt <-
    paste('[component]',
          'type\tpredatorpreyprinter',
          sprintf('predatornames\t%s',
                  paste(fleets$fleet,collapse=' ')),
          'preynames\t%1$s',
          'areaaggfile\t%2$s/Aggfiles/allareas.agg',
          'ageaggfile\t%2$s/Aggfiles/%1$s.allages.agg',
          'lenaggfile\t%2$s/Aggfiles/%1$s.alllen.agg',
          'printfile\t%2$s/out/%1$s.prey',
          'yearsandsteps\tall all',
          sep = '\n')
  
  print.ssb <- NULL
  if(!is.null(ssb.stock)){
    if(sum(ssb.stock %in% names(stocks)) == length(ssb.stock)){
      print.ssb <-
        paste('[component]',
              'type\tstockprinter',
              'stocknames\t%1$s',
              'areaaggfile\t%2$s/Aggfiles/allareas.agg',
              'ageaggfile\t%2$s/Aggfiles/%1$s.allages.agg',
              'lenaggfile\t%2$s/Aggfiles/%1$s.alllen.agg',
              'printfile\t%2$s/out/%1$s.ssb',
              'yearsandsteps\tall 1',
              sep = '\n')
      
    } else {
      warning(sprintf('warning in gadget.ypr, SSB stock %s not found',
                      ssb.stock))
      
    }
  }
  print.mat <- NULL
  if(!is.null(mat.par)){
    print.mat <-
      paste('[component]',
            'type\tstockprinter',
            'stocknames\t%1$s',
            'areaaggfile\t%2$s/Aggfiles/allareas.agg',
            'ageaggfile\t%2$s/Aggfiles/%1$s.allages.agg',
            'lenaggfile\t%2$s/Aggfiles/%1$s.len.agg',
            'printfile\t%2$s/out/%1$s.mat',
            'yearsandsteps\tall 1',
            sep = '\n')
  }
  
  
  
  printfile <- paste(sprintf(print.txt,unique(fleet$prey$stock), ypr),
                     collapse='\n;\n')
  if(!is.null(ssb.stock)){
    printfile <-
      paste(printfile,
            paste(sprintf(print.ssb,ssb.stock,ypr),
                  collapse='\n;\n'),
            sep='\n;\n')
  }
  
  if(!is.null(mat.par)){
    printfile <-
      paste(printfile,
            paste(sprintf(print.mat,unique(fleet$prey$stock),ypr),
                  collapse='\n;\n'),
            sep='\n;\n')
  }
  
  
  dir.create(sprintf('%s/out',ypr),showWarnings = FALSE, recursive = TRUE)
  main$printfiles <- sprintf('%s/printfile.ypr',ypr)
  write.unix(printfile,f=sprintf('%s/printfile.ypr',ypr))
  
  
  ## remove recruitment and initialdata from the stockfiles
  
  plyr::l_ply(stocks,function(x){
    x@initialdata[,3] <- 0 ## nothing in the beginning
    if(x@doesrenew==1){
      x@renewal.data <- 
        time.grid %>% 
        dplyr::filter(step == 1) %>% 
        dplyr::bind_cols(slice(x@renewal.data %>% select(-c(year,step,area)),rep(1,nrow(.)))) %>% 
        dplyr::mutate(number = ifelse(year==begin,1,0))
      x@doesspawn <- 0
    }
    gadget_dir_write(gd,x)
  })
  
  main$stockfiles <- sprintf('%s/%s',ypr,
                             plyr::laply(stocks,function(x) x@stockname))
  
  main$likelihoodfiles <- ';'
  
  write.gadget.main(main,file=sprintf('%s/main.ypr',ypr))
  
  ## model parameters
  if(sum(names(params) %in% c('switch','value',
                              'lower','upper','optimise'))==5){
    tmp <- as.data.frame(t(params$value))
    names(tmp) <- params$switch
    params <- tmp
  }
  
  
  
  params.aug <- plyr::ldply(effort,
                      function(x){
                        tmp <- params
                        tmp$effort <- x
                        return(tmp)
                      })
  
  write.gadget.parameters(params.aug,file=sprintf('%s/params.ypr',ypr),
                          columns = FALSE)
  
  callGadget(s=1,i=sprintf('%s/params.ypr',ypr),main=sprintf('%s/main.ypr',ypr))
  
  ## read output
  
  out <- plyr::ddply(data.frame(stock = unique(fleet$prey$stock),tmp=1),
               'stock',
               function(x){
                 stock.prey <- read.table(file = sprintf("%1$s/out/%2$s.prey",
                                                         ypr,x$stock),
                                          comment.char = ';')
                 
                 names(stock.prey) <-
                   c('year', 'step','area','age','length','number.consumed',
                     'biomass.consumed','fishing.mortality')
                 
                 stock.prey$trial <-
                   rep(1:c(nrow(stock.prey)/(
                     length(unique(stock.prey$area))*
                       length(unique(stock.prey$step))*
                       length(unique(stock.prey$year)))),
                     each=length(unique(stock.prey$area))*
                       length(unique(stock.prey$year))*
                       length(unique(stock.prey$step)))
                 stock.prey <- merge(stock.prey,
                                     data.frame(trial=1:length(effort),
                                                effort=effort),
                                     all.x=TRUE)
                 stock.prey$age <- stock.prey$year - begin +
                   getMinage(stocks[[x$stock]])
                 ## clean up
                 file.remove(sprintf('%s/out/%s.prey',ypr,x$stock))
                 
                 return(stock.prey)
               })
  if(!is.null(ssb.stock)){
    ssb.out <- plyr::ldply(ssb.stock,function(x){
      ssb.out <- read.table(file = sprintf("%1$s/out/%2$s.ssb",
                                           ypr,x), comment.char = ';')
      file.remove(sprintf('%s/out/%s.ssb',ypr,x))
      names(ssb.out) <-
        c('year', 'step','area','age','length','number',
          'biomass')
      ssb.out$trial <-
        rep(1:c(nrow(ssb.out)/(
          length(unique(ssb.out$area))*
            length(unique(ssb.out$step))*
            length(unique(ssb.out$year)))),
          each=length(unique(ssb.out$area))*
            length(unique(ssb.out$year))*
            length(unique(ssb.out$step)))
      ssb.out <- merge(ssb.out,
                       data.frame(trial=1:length(effort),
                                  effort=effort),
                       all.x=TRUE)
      plyr::mutate(plyr::ddply(ssb.out,~effort,summarise,ssb=max(number*biomass)),
             ssb.ratio=ssb/max(ssb))
      
    })
    
  } else {
    ssb.out <- NULL
  }
  
  if(!is.null(mat.par)){
    mat.out <- plyr::ldply(unique(fleet$prey$stock),function(x){
      mat.out <- 
        read.table(file = sprintf("%1$s/out/%2$s.mat",
                                  ypr,x), comment.char = ';')
      file.remove(sprintf('%s/out/%s.mat',ypr,x))
      names(mat.out) <-
        c('year', 'step','area','age','length','number',
          'biomass')
      mat.out %>% 
        mutate(length = as.numeric(gsub('len','',length)),
               trial = cut(1:length(year),c(0,which(diff(year)<0),1e9),labels = FALSE)) %>% 
        left_join(data.frame(trial=1:length(effort),
                             effort=effort)) %>% 
        group_by(effort,year) %>% 
        summarise(ssb=sum(number*biomass*logit(mat.par[1],mat.par[2],length))) %>% 
        group_by(effort) %>% 
        mutate(ssb.ratio = ssb/max(ssb))
      
    })
    
  } else {
    mat.out <- NULL
  }
  
  
  
  if(!is.null(age.range)){
    out <- subset(out,age >= min(age.range) & age <= max(age.range))
  }
  res <- plyr::ddply(out,'effort',
               function(x) c(num=sum(x$number.consumed)/1e6,
                             bio=sum(x$biomass.consumed)/1e6))
  secant <- diff(res$bio)/diff(res$effort)
  f0.1 <- res$effort[min(which(secant<0.1*secant[1]))]
  fmax <- min(res$effort[which(res$bio==max(res$bio,na.rm=TRUE))])
  res <- list(params=params,out=out,ypr=res,fmax=fmax,
              f0.1=f0.1,ssb=ssb.out,mat.out=mat.out)
  
  
  class(res) <- c('gadget.ypr',class(res))
  if(save.results){
    save(res, file = sprintf('%s/ypr.Rdata',ypr))
  }
  
  return(res)
}

##' @rdname gadget.ypr
##' @export
plot.gadget.ypr <- function(ypr){
  if(!is.null(ypr$ssb)){
    tmp <- merge(mutate(ypr$ypr,bio=bio/max(bio)),
                 ypr$ssb)
  } else {
    tmp <- mutate(ypr$ypr,bio=bio/max(bio))
  }
  plo <- ggplot(tmp,aes(effort,bio)) +
    geom_line() +
    geom_segment(aes(x = effort,xend=effort,y=-Inf,yend=bio),
                 data=subset(tmp, effort == ypr$fmax)) +
    geom_segment(aes(x = effort,xend=effort,y=-Inf,yend=bio),
                 data=subset(tmp, effort == ypr$f0.1)) +
    geom_text(data=subset(tmp,effort == ypr$fmax),
              aes(label = sprintf('Fmax = %s',effort),
                  x = effort+0.04,y=0.2,angle=90)) +
    geom_text(data=subset(tmp,effort == ypr$f0.1),
              aes(label = sprintf('F0.1 = %s',effort),
                  x = effort+0.04,y=0.2,angle=90)) +
    theme_bw() +  xlab('Fishing mortality') + ylab('Yield per recruit') +
    theme(legend.position='none',plot.margin = unit(c(0,0,0,0),'cm'))
  if(!is.null(ypr$ssb)){
    plo <- plo + geom_line(aes(effort,ssb.ratio),
                           lty=2,col='gray')
  }
  return(plo)
}



#' Analytical retrospective
#'
#' \code{gadget.retro} runs an analytical retrospective model fitting run. 
#' @param path location of the Gadget model, all filenames are relative to the path
#' @param main.file name of the main file, defaults to 'main'
#' @param params.file name of the starting parameter value file, defaults to 'params.in'
#' @param optinfofile name of the file containing the optimizer settings, defaults to 'optinfofile'
#' @param num.years number of years (models) should be used, defaults to 5 yeaes
#' @param pre location of the model runs, defaults to 'RETRO'
#' @param iterative logical should the iterative reweighting be used, defaults FALSE
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
    
    gadgettime(main[[1]]$timefile,path) %>% 
      gadget_update(lastyear = .[[1]]$lastyear-year) %>% 
      write.gadget.file(Rdir)
    
  #  lik <- gadgetlikelihood(main[['likelihood']]$likelihoodfiles,Rdir) 
  #  attr(lik,'file_config')$mainfile_overwrite <- TRUE
  #  write.gadget.file(lik,Rdir)
  }
  
  Sys.setenv(GADGET_WORKING_DIR=normalizePath(path))
  
  if(iterative){
    run.func <- function(year){
      gadget.iterative(main.file=sprintf('%s/R%s/main',pre,year),
                       params.file = params.file,
                       optinfofile=optinfofile,
                       wgts = sprintf('%s/WGTS.%s',pre,year),
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
