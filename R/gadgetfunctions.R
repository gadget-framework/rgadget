##' This function sets up all necessary switches and calls gadget from R
##' and attempts to read the runtime output from gadget. This has currently
##' only been tested on unix based platforms (but should in principle work on
##' windows, given that gadget can be compiled).
##' The source code for gadget can be obtained from http://www.hafro.is/gadget
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
                       ignore.stderr=TRUE
                       ){
    
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
                           paste('-printfinal',printfinal)))

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

##' Probably defunct
##'
##' <details>
##' @title Call paramin
##' @param i
##' @param func
##' @param opt
##' @param network
##' @param o
##' @param scale
##' @param condor
##' @param paramin.exe
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
                             rew.sI=FALSE,
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
                             cl=NULL) {

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
             i=params.file,gadget.exe=gadget.exe)


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
      dat <- ldply(lik.dat$dat$surveyindices,
                   function(x) x)

      dat$y <- log(dat$number)
      dat$year <- as.factor(dat$year)
      fit <- lm(y~year+length+step,dat)
      weights <- (lik.dat$df$surveyindices -
                  tapply(dat$length,dat$name,function(x) length(unique(x))))/
                    tapply(resid(fit),dat$name,function(x) sum(x^2))
    } else {
      weights <- ldply(lik.dat$dat$surveyindices,
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
               PBS.name=paste(wgts,comp,sep='/'))
    callGadget(s=1,
               main=paste(wgts,paste('main',comp,sep='.'),sep='/'),
               i=paste(wgts,paste('params',comp,sep='.'),sep='/'),
               o=paste(wgts,paste('lik',comp,sep='.'),sep='/'),
               gadget.exe=gadget.exe,
               PBS=PBS,
               PBS.name=paste(wgts,comp,sep='/'))
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
        
        res <- mclapply(run.string,run.iterative,
                        mc.cores = detectCores(logical = TRUE))
    }
  }

  ## Do we want to run the final optimisation (only used for debug purposes,
  ## and the check should be removed in later revisions)

  if(run.final){
    res <- ldply(run.string,
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
                 qsub.script=qsub.script)
      callGadget(s=1,
                 main=sprintf('%s/main.%s',wgts,comp),
                 i=sprintf('%s/params.%s',wgts,comp),
                 o=sprintf('%s/lik.%s',wgts,comp),
                 gadget.exe=gadget.exe)
      print(sprintf('Comp %s completed',comp))
    }

    ## read in the results from previous runs
    SS <- ldply(names(run.string),
                function(x)
                data.frame(group=x,comp=run.string[[x]],
                           SS=as.numeric(res[x,run.string[[x]]])))
    df <- ldply(lik.dat$df,function(x) data.frame(df=x,comp=names(x)))
    SS <- mutate(join(SS,df),sigmahat = SS/df)
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
    comp <- 'final'
    if(!rew.sI){
      SS[restr.SI,'sigmahat'] <- sIw[restr.SI]
    }
    if(!rew.sI){
      write.files('sIw',SS)
      comp <- as.list(c('final','sIw'))
    }

    if(run.serial){
        lapply(comp,run.final)
    } else if(Sys.info()[['sysname']]=='Windows'){
        cl <- makeCluster(detectCores(logical=TRUE))
        res <- parLapply(cl,run.string,run.final)
        stopCluster(cl)
    } else {
        mclapply(comp,run.final)
    }

  } else {
    comp <- NULL
  }

  return(list(comp=run.string,final=comp,wgts=wgts))

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
  tmp <- ddply(sens,'parameter',
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
##' @param optinfofile
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



##' <description>
##'
##' <details>
##' @title Bootstrap control
##' @param bs.likfile Likelihood template file
##' @param bs.samples number (or vector of numbers) indicating what bootstrap
##' samples should be used
##' @param main Main file for the gagdet model
##' @param optinfofile optinfofile used in the optimization.
##' @param bs.wgts folder containing the resulting reweights
##' @param bs.data folder containing the bootstrap dataset obtain from the DW
##' @param params.file
##' @param rew.sI should the survey indices be reweighted seperately
##' @param grouping list of grouped likelihood components
##' @param qsub.script name of the qsub script if the calculations is meant to be run on a cluser
##' @param run.final logical, is this the final run or weighting run.
##' @param PBS logical, is this a cluster run?
##' @return NULL
##' @author Bjarki Thor Elvarsson
##' @export
gadget.bootstrap <- function(bs.likfile = 'likelihood.bs',
                             bs.samples = 1,
                             main='main',
                             optinfofile='optinfo',
                             bs.wgts='BS.WGTS',
                             bs.data=NULL,
                             params.file = 'params.in',
                             rew.sI = FALSE,
                             grouping = NULL,
                             qsub.script = 'bootstrap.sh',
                             run.final = FALSE,
                             PBS=TRUE,
                             cv.floor=NULL
                             ){
  ## Ensure all files exist
  if(!file.exists(main)) {
    stop('Main file not found')
  }

  if(!file.exists(params.file)) {
    stop('Parameter file not found')
  }

  if(!file.exists(optinfofile)) {
    stop('Optinfofile not found')
  }

  if(!file.exists(bs.likfile)) {
    stop('likelihoodfile not found')
  }

  ## Do stuff

  dir.create(bs.wgts,showWarnings=FALSE)
  main <- read.gadget.main(main)
  bs.lik <- read.gadget.likelihood(bs.likfile)

  foreach(i=bs.samples) %dopar% {
    print(i)
    dir.create(sprintf('%s/BS.%s',bs.wgts,i),showWarnings=FALSE)
    bs.lik.file <- sprintf('%s/BS.%s/likelihood',bs.wgts,i)
    bs.main.file <- sprintf('%s/BS.%s/main',bs.wgts,i)
    if(!run.final) {
      bs.main <- main
      bs.main$likelihoodfiles <- bs.lik.file
      write.gadget.main(bs.main,bs.main.file)
      write.gadget.likelihood(bs.lik,bs.lik.file,bs.data,sprintf('.%s',i))
      tmp <- gadget.iterative(main.file=bs.main.file,
                              params.file = params.file,
                              grouping = grouping,
                              optinfofile = optinfofile,
                              rew.sI = rew.sI,
                              PBS = PBS,
                              qsub.script = qsub.script,
                              wgts = sprintf('%s/BS.%s',bs.wgts,i),
                              run.final = FALSE,
                              run.serial = TRUE)
      if(PBS)
        write.unix(sprintf('# bootstrap sample %s',i),f=qsub.script,append=TRUE)
      if(i > 100 & PBS)
        write.unix('sleep 6m',f=qsub.script,append=TRUE)
      else
        print(sprintf('# bootstrap sample %s',i))

    } else {
      tmp <- gadget.iterative(main.file = bs.main.file,
                              params.file = params.file,
                              grouping = grouping,
                              PBS = PBS,
                              optinfofile = optinfofile,
                              rew.sI = rew.sI,
                              qsub.script = qsub.script,
                              wgts = sprintf('%s/BS.%s',bs.wgts,i),
                              resume.final = TRUE,
                              run.final = TRUE,
                              run.serial = TRUE,
                              cv.floor = cv.floor)
      if(PBS)
        write.unix(sprintf('# bootstrap sample %s',i),f=qsub.script,append=TRUE)
      if(i > 100 & PBS)
          write.unix('sleep 6m',f=qsub.script,append=TRUE)
      else
        print(sprintf('# bootstrap (final) sample %s',i))

    }
  }
  return(NULL)
}

##' Calculate yield per recruit of a stock in a Gadget model
##'
##' Assumes sed is present in the command line
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

  fleet <- llply(fleet,
                 function(x){
                   tmp <- subset(x,fleet %in% fleets$fleet)
                 })
  fleet$fleet <- mutate(fleet$fleet,
                        multiplicative = '1#effort',
                        amount = sprintf('%s/fleet.ypr', ypr),
                        type = 'linearfleet')

  fleet.predict <- ddply(fleets,'fleet',function(x){
    tmp <- mutate(time.grid,
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

  l_ply(stocks,function(x){
    x@initialdata[,3] <- 0 ## nothing in the beginning
    if(x@doesrenew==1){
      tmp <- subset(time.grid,step == 1)
      tmp <- mutate(tmp,
                    age = x@renewal.data[1,4],
                    number = 0,
                    mean = x@renewal.data[1,6],
                    stddev = x@renewal.data[1,7],
                    alpha = x@renewal.data[1,8],
                    beta = x@renewal.data[1,9])
      tmp$number[1] <- 100
      x@renewal.data <- tmp
      x@doesspawn <- 0
    }
    gadget_dir_write(gd,x)
  })

  main$stockfiles <- sprintf('%s/%s',ypr,
                             laply(stocks,function(x) x@stockname))

  main$likelihoodfiles <- ';'

  write.gadget.main(main,file=sprintf('%s/main.ypr',ypr))

  ## model parameters
  if(sum(names(params) %in% c('switch','value',
                              'lower','upper','optimise'))==5){
    tmp <- as.data.frame(t(params$value))
    names(tmp) <- params$switch
    params <- tmp
  }



  params.aug <- ldply(effort,
                      function(x){
                        tmp <- params
                        tmp$effort <- x
                        return(tmp)
                      })

  write.gadget.parameters(params.aug,file=sprintf('%s/params.ypr',ypr),
                          columns = FALSE)

  callGadget(s=1,i=sprintf('%s/params.ypr',ypr),main=sprintf('%s/main.ypr',ypr))

  ## read output

  out <- ddply(data.frame(stock = unique(fleet$prey$stock),tmp=1),
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
    ssb.out <- ldply(ssb.stock,function(x){
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
      mutate(ddply(ssb.out,~effort,summarise,ssb=max(number*biomass)),
             ssb.ratio=ssb/max(ssb))

    })

  } else {
    ssb.out <- NULL
  }

  if(!is.null(mat.par)){
    mat.out <- ldply(unique(fleet$prey$stock),function(x){
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
  res <- ddply(out,'effort',
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
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Plot yield per recruit
##' @param ypr gadget.ypr object
##' @return ggplot object
##' @author Bjarki Thor Elvarsson
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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Gadget bootstrap yield per recruit
##' @param params.file
##' @param main.file
##' @param effort
##' @param begin
##' @param end
##' @param fleets
##' @param ypr
##' @param bs.wgts
##' @param bs.samples
##' @param .parallel
##' @return yield per recruit for the bootstrap
##' @author Bjarki Thor Elvarsson
##' @export
gadget.bootypr <- function(params.file='params.final',
                           main.file = 'main.final',
                           effort = seq(0, 1, by=0.01),
                           begin=1990,end=2020,
                           fleets = data.frame(fleet='comm',ratio=1),
                           ypr='YPR',
                           bs.wgts = 'BS.WGTS',
                           bs.samples = 1:1000,
                           .parallel = TRUE){
  tmp <-
    llply(bs.samples,function(x){

      tryCatch(gadget.ypr(params.file = sprintf('%s/BS.%s/%s',
                            bs.wgts,x,params.file),
                 main.file = sprintf('%s/BS.%s/%s',bs.wgts,x,main.file),
                 effort = effort,
                 begin = begin, end = end, fleets = fleets,
                 ypr = sprintf('%s/BS.%s/%s',bs.wgts,x,ypr)),
               error = function(e){
                 print(sprintf('YPR run %s was corrupted',x))
               })

    },.parallel = .parallel)
  print('ypr finished -- tidying up')
  names(tmp) <- sprintf('BS.%s',bs.samples)
  tmp.names <- c('params','out','ypr','fmax','f0.1')
  names(tmp.names) <- c('params','out','ypr','fmax','f0.1')
  bsypr <- llply(tmp.names,
                 function(x) ldply(tmp,function(y) y[[x]]))
  save(bsypr,file=sprintf('%s/bsypr.RData',bs.wgts))
  return(bsypr)
}



#' Gadget forward 
#'
#' This function implements a crude forward simulation for a Gadget model.
#' NOTE: the function currently assumes at least one recruiting stock. 
#' -- details to come--
#' @param years Number of years to predictc
#' @param params.file parameter file to base the prediction upon
#' @param main.file main file for the model
#' @param num.trials number of projection repliactes
#' @param fleets data frame with at least two columns, fleet and ratio, 
#' which are the names of the fleets and the ratio of the harvestable biomass they consume.
#' @param effort proportion of the harvestable biomass taken per year. Note that this relates to 
#' fishing mortality of fully recruited through the relation F=-log(1-E)
#' @param rec.scalar scaling schedule for recruitment going forward. Data frame with year, stock and rec.scalar
#' @param check.previous Should previous results be loaded? Defaults to FALSE
#' @param save.results Should the results be saved? Defaults to TRUE
#' @param stochastic Should the projection be stochastic (default) or deterministic (assuming the average three year recruitment)?
#' @param rec.window What timeperiod should be used to estimate the distribution of recruits.
#' @param compact should ssb and total bio be calculated
#' @param mat.par parameters determining the maturity ogive
#' @param gd gadget directory
#'
#' @return
#' @export
gadget.forward <- function(years = 20,params.file = 'params.out',
                           main.file = 'main', num.trials = 10,
                           fleets = data.frame(fleet='comm',ratio = 1),
                           effort = 0.2,
                           rec.scalar = NULL,
                           check.previous = FALSE,
                           save.results = TRUE,
                           stochastic = TRUE,
                           rec.window = NULL,
                           compact = TRUE,
                           mat.par=c(0,0),
                           gd=list(dir='.',rel.dir='PRE'),
                           method = 'AR1',
                           ref.years=NULL,
                           printfile=NULL){
  ## TODO fix stocks that are spawning
  pre <- paste(gd$dir,gd$rel.dir,sep='/') 
  
  if(check.previous){
    if(file.exists(sprintf('%s/out.Rdata',pre))){
      load(sprintf('%s/out.Rdata',pre))
      return(out)
    }
  }
  
  dir.create(pre,showWarnings = FALSE, recursive = TRUE)
  dir.create(sprintf('%s/Aggfiles',pre), showWarnings = FALSE)
  
  
  ## read in model files
  main <- 
    read.gadget.main(file = main.file)
  stocks <- 
    read.gadget.stockfiles(main$stockfiles)
  time <- 
    read.gadget.time(main$timefile)
  area <- 
    read.gadget.area(main$areafile)
  fleet <- 
    read.gadget.fleet(main$fleetfiles)
  all.fleets <- 
    paste(fleet$fleet$fleet,collapse = ' ')
  params <-
    read.gadget.parameters(params.file)
  rec <- 
    get.gadget.recruitment(stocks,params,collapse = FALSE) %>% 
    na.omit() 
  
  if(is.null(ref.years)){
    ref.years <- 
      min(rec$year)
  }
  
  rec.step.ratio <- 
    rec %>% 
    dplyr::group_by(stock,step) %>% 
    dplyr::filter(year %in% ref.years) %>% 
    dplyr::summarise(rec.ratio = sum(recruitment)) %>% 
    dplyr::mutate(rec.ratio = rec.ratio/sum(rec.ratio))
  
  rec <- 
    rec %>% 
    dplyr::group_by(stock,year) %>% 
    dplyr::summarise(recruitment = sum(recruitment)) %>% 
    dplyr::arrange(stock,year)
  
  
  ## Write agg files
  plyr::l_ply(stocks,
              function(x){
                writeAggfiles(x,folder=sprintf('%s/Aggfiles',pre))
              })
  
  ## adapt model to include predictions
  sim.begin <- 
    time$lastyear + 1
  rec <- 
    rec %>% 
    dplyr::filter(year < sim.begin)
  
  if(nrow(rec) == 0)
    stop('No recruitment info found')
  
  time$lastyear <- sim.begin + years - 1
  write.gadget.time(time,file = sprintf('%s/time.pre',pre))
  main$timefile <- sprintf('%s/time.pre',pre)
  
  time.grid <- 
    expand.grid(year = time$firstyear:time$lastyear,
                step = 1:length(time$notimesteps),
                area = area$areas)
  
  ## hmm check this at some point
  area$temperature <- 
    dplyr::mutate(time.grid,
                  temperature = 5)
  
  main$areafile <- sprintf('%s/area',pre)
  write.gadget.area(area,file=sprintf('%s/area',pre))
  
  ## fleet setup 
  fleet <- llply(fleet,
                 function(x){
                   tmp <- subset(x,fleet %in% fleets$fleet)
                 })
  
  fleet$fleet <- mutate(fleet$fleet,
                        fleet = sprintf('%s.pre',fleet),
                        multiplicative = '#rgadget.effort',   #effort,
                        amount = sprintf('%s/fleet.pre', pre),
                        type = 'linearfleet')

  fleet$prey <- mutate(fleet$prey,
                       fleet = sprintf('%s.pre',fleet))
  
  
  fleet.predict <- ddply(fleets,'fleet',function(x){
    tmp <- mutate(subset(time.grid,
                         (year >= sim.begin | (year==(sim.begin-1) &
                                                 step > time$laststep)) &
                           area %in% fleet$fleet$livesonareas
    ),
    fleet = sprintf('%s.pre',x$fleet),
    ratio = x$ratio)
    return(tmp)
  })
  
  
  write.gadget.table(arrange(fleet.predict[c('year','step','area','fleet','ratio')],
                             year,step,area),
                     file=sprintf('%s/fleet.pre',pre),
                     col.names=FALSE,row.names=FALSE,
                     quote = FALSE)
  
  main$fleetfiles <- c(main$fleetfiles,sprintf('%s/fleet', pre))
  write.gadget.fleet(fleet,file=sprintf('%s/fleet', pre))
  
  
  
  ## recruitment 
  
  if(!is.null(rec.window)){
    if(length(rec.window)==1){
      tmp <- 
        rec %>% 
        dplyr::filter(year < rec.window)
    } else {
      tmp <- 
        rec %>% 
        dplyr::filter(year <= max(rec.window) & year >= min(rec.window))
    }
  } else {
    tmp <- rec
  }
  
  ## todo: consider covariates in recruitment
  
  if(stochastic){
    if(tolower(method) == 'bootstrap'){
      prj.rec <-
        tmp %>% 
        split(.$stock) %>% 
        purrr::map(~dplyr::select(.,recruitment) %>% 
                     dplyr::slice(plyr::rlply(ceiling(num.trials*years/nrow(tmp)),
                                              c(sample(rec.window[2]:rec.window[3]-rec.window[1]+1,
                                                       replace = TRUE),
                                                sample(rec.window[1]:rec.window[2]-rec.window[1]+1,
                                                       replace = TRUE))) %>%                                           
                                    unlist())) %>%
        purrr::map(~dplyr::slice(.,1:(num.trials*years))) %>% 
        purrr::map(~tibble::data_frame(year = rep((sim.begin):(sim.begin+years-1),num.trials),
                                       trial = rep(1:num.trials,each=years),
                                       recruitment = .$recruitment)) %>% 
        dplyr::bind_rows(.id='stock') %>% 
        select(stock,year,trial,recruitment)
        
    } else {
    ## fit an AR model to the fitted recruiment
      prj.rec <- 
        tmp %>% 
        split(.$stock) %>% 
        purrr::map(~lm(.$recruitment[-1]~head(.$recruitment,-1))) %>%
        purrr::map(~dplyr::bind_cols(broom::glance(.),
                                     as.data.frame(t(broom::tidy(.)$estimate)))) %>% 
        purrr::map(~dplyr::rename(.,a=V1,b=V2)) %>% 
        purrr::map(~data.frame(year = rep((sim.begin):(sim.begin+years-1),num.trials),
                               trial = rep(1:num.trials,each=years),
                               x = pmax(rnorm(years*num.trials,.$a,.$sigma),0),
                               a = .$a,
                               b = .$b,
                               sigma = .$sigma)) %>% 
        dplyr::bind_rows(.id='stock')  %>% 
        dplyr::mutate(rec = x + b*dplyr::lag(x),
                      rec = ifelse(is.na(rec),x,rec)) %>% 
        select(stock,year,trial,recruitment = rec)
      
    ## project next n years
    }
  } else {
    prj.rec <- 
      tmp %>% 
      dplyr::filter(year > sim.begin - 4) %>%
      dplyr::group_by(stock) %>% 
      dplyr::summarise(recruitment = mean(recruitment)) %>% 
      dplyr::left_join(expand.grid(stock = stocks %>% 
                                     purrr::map(Rgadget:::getStockNames) %>% 
                                     unlist,
                                   year = (sim.begin):(sim.begin+years-1),
                                   trial = 1:num.trials) %>% 
                         dplyr::arrange(stock,year,trial))
  }
  
  if(num.trials == 1 & length(effort)==1){
    prj.rec %>% 
      dplyr::mutate(switch = paste(stock,'rec',year,sep='.'),
                    lower = 0,
                    upper = recruitment + 1,
                    optimise = 0) %>% 
      dplyr::select(switch,value=recruitment,lower,upper,optimise) %>% 
      dplyr::bind_rows(params,
                       data.frame(switch = 'rgadget.effort',
                                  value = effort,
                                  lower = 0.0001,
                                  upper = 100, 
                                  optimise = 0,
                                  stringsAsFactors = FALSE)) %>% 
      write.gadget.parameters(file=sprintf('%s/params.forward', pre))
  } else {
    params %>% 
      dplyr::select(switch,value) %>% 
      tidyr::spread(switch,value) %>% 
      dplyr::slice(rep(1,num.trials*length(effort))) %>% 
      dplyr::bind_cols(prj.rec %>% 
                         dplyr::mutate(switch = paste(stock,'rec',year,sep='.')) %>% 
                         dplyr::select(trial,switch,recruitment) %>% 
                         tidyr::spread(switch,recruitment) %>% 
                         dplyr::select(-trial) %>% 
                         dplyr::slice(rep(1:num.trials,each=length(effort))) %>% 
                         dplyr::mutate(rgadget.effort=rep(effort,num.trials))) %>% 
      write.gadget.parameters(file=sprintf('%s/params.forward', pre),
                              columns = FALSE)
  }
  
  
  ## add recruitment scalar
  if(is.null(rec.scalar)){
    prj.rec <- 
      prj.rec %>% 
      mutate(rec.scalar=1)
  } else {
    if(sum(rec.scalar$stock %in% prj.rec$stock)==0)
      warning('No stocks found in rec.scalar')
    prj.rec <- 
      prj.rec %>% 
      left_join(rec.scalar %>% 
                  select(stock, year, rec.scalar)) %>% 
      mutate(rec.scalar = ifelse(is.na(rec.scalar),1,rec.scalar))
  }
  ## end fix
  
  ## create the output files
  print.txt <-
    paste('[component]',
          'type             stockprinter',
          'stocknames       %1$s',
          'areaaggfile      %2$s/Aggfiles/%1$s.area.agg',
          'ageaggfile       %2$s/Aggfiles/%1$s.allages.agg',
          'lenaggfile       %2$s/Aggfiles/%1$s.len.agg',
          'printfile        %2$s/out/%1$s.lw',
          'printatstart     0',
          'yearsandsteps    all 1',
          sep = '\n')
  
  catch.print <-
    paste('[component]',
          'type\t\tpredatorpreyprinter',
          'predatornames\t\t%3$s',
          'preynames\t\t%1$s',
          'areaaggfile      %2$s/Aggfiles/%1$s.area.agg',
          'ageaggfile       %2$s/Aggfiles/%1$s.allages.agg',
          'lenaggfile       %2$s/Aggfiles/%1$s.alllen.agg',
          'printfile        %2$s/out/catch.%1$s.lw',
          'yearsandsteps    all all',
          sep = '\n')
  
  
  printfile <-
    paste(
      printfile,
      paste(sprintf(catch.print, unique(fleet$prey$stock), pre,
                    paste(all.fleets, paste(fleet$fleet$fleet,collapse=' '))),
            collapse='\n'),
      paste(sprintf(print.txt,unique(fleet$prey$stock),
                    pre),
            collapse = '\n'),
      ';',
      '[component]',
      'type\tlikelihoodsummaryprinter',
      'printfile\t.jnk',
      sep = '\n')
  
  
  dir.create(sprintf('%s/out/',pre),showWarnings = FALSE, recursive = TRUE)
  
  main$printfiles <- sprintf('%s/printfile',pre)
  write.unix(printfile,f = sprintf('%s/printfile',pre))
  
  main$likelihoodfiles <- ';'
  
  llply(stocks,function(x){
    tmp <- 
      prj.rec %>% 
      dplyr::filter(stock == x@stockname,trial == 1) %>% 
      dplyr::left_join(rec.step.ratio,by=c('stock'))
    
    if(x@doesrenew==1){
      x@renewal.data <-
        x@renewal.data %>% 
        dplyr::filter(year < sim.begin) %>% 
        dplyr::bind_rows(x@renewal.data %>% 
                           dplyr::filter(year == min(ref.years)) %>% 
                           dplyr::slice(rep(1:n(),nrow(tmp))) %>% 
                           dplyr::mutate(year=as.character(tmp$year),
                                         number = sprintf('(* (* 0.0001 #%s.rec.%s ) %s)',
                                                          x@stockname,year, 
                                                          tmp$rec.scalar*tmp$rec.ratio)) %>% 
                           dplyr::select_(.dots = names(x@renewal.data))) %>% 
        as.data.frame()
    }
    gadget_dir_write(gd,x)
  })

  main$stockfiles <- paste(sprintf('%s/%s',pre,
                                   laply(stocks,function(x) x@stockname)),
                           collapse = ' ')
  
  
  write.gadget.main(main,file=sprintf('%s/main.pre',pre))
  
  
  callGadget(s = 1, i = sprintf('%s/params.forward',pre),
             main = sprintf('%s/main.pre',pre))
  
  time <- new('gadget-time',
              firstyear = time$firstyear,
              firststep = time$firststep,
              lastyear = time$lastyear,
              laststep = time$laststep,
              notimesteps = time$notimesteps)
  
  
  
  out <- list(
    lw = ldply(unique(fleet$prey$stock),
               function(x){
                 numsteps <- 
                   nrow(subset(getTimeSteps(time),step==1))
                 tmp <- 
                   read.table(sprintf('%s/out/%s.lw',pre,x),
                              comment.char = ';')
                 file.remove(sprintf('%s/out/%s.lw',pre,x))
                 names(tmp) <-  
                   c('year', 'step', 'area', 'age',
                     'length', 'number', 'weight')
                 tmp$stock <- x
                 if(num.trials > 1){
                   tmp2 <- 
                     length(unique(tmp$area))*numsteps*
                     length(unique(tmp$length))
                   
                   tmp <- 
                     cbind(trial = as.factor(rep(1:num.trials, each = length(effort)*tmp2)),#as.factor(rep(trials.tmp,each = nrow(tmp)/length(trials.tmp))),
                           effort = as.factor(rep(effort,each=tmp2,num.trials)),
                           tmp)
                 } else {
                   tmp2 <- 
                     length(unique(tmp$area))*numsteps*
                     length(unique(tmp$length))
                   
                   tmp$trial <- as.factor(1)
                   tmp$effort <- as.factor(rep(effort,each=tmp2))
                 }
                 tmp$length <- as.numeric(gsub('len','',tmp$length))
                 
                 if(compact){
                   tmp <- ddply(tmp,~year+step+trial+effort+stock,
                                summarise,
                                total.bio = sum(number*weight),
                                ssb = sum(logit(mat.par[1],mat.par[2],
                                                length)*number*weight))
                 }
                 return(tmp)
               }),
    catch =
      ldply(unique(fleet$prey$stock),
            function(x){
              numsteps <- nrow(getTimeSteps(time))
              trials.tmp <- rep(1:num.trials,each=length(effort))
              
              tmp <-
                read.table(sprintf('%s/out/catch.%s.lw',pre,x),
                           comment.char = ';')
              file.remove(sprintf('%s/out/catch.%s.lw',pre,x))
              names(tmp) <-  c('year', 'step', 'area', 'age',
                               'length', 'number.consumed',
                               'biomass.consumed','mortality')
              tmp$stock <- x
              
              if((num.trials > 1) | (length(effort)>1)) {
                
                tmp2 <- 
                  length(unique(tmp$area))*
                  numsteps
                
                tmp <-
                  cbind(trial = as.factor(rep(1:num.trials, each = length(effort)*tmp2)),#as.factor(rep(trials.tmp,each = nrow(tmp)/length(trials.tmp))),
                        effort = as.factor(rep(effort, each=tmp2,num.trials)),
                        tmp)
              } else {
                tmp$trial <- as.factor(1)
                tmp2 <- length(unique(tmp$area))*
                  numsteps
                tmp$effort <- as.factor(rep(effort,each=tmp2))
              }
              return(tmp)
            }),
    recruitment = prj.rec,
    num.trials = num.trials,
    stochastic = stochastic,
    sim.begin = sim.begin
  )
  class(out) <- c('gadget.forward',class(out))
  if(save.results){
    save(out,file = sprintf('%s/out.Rdata',pre))
  }
  return(out)
}



plot.gadget.forward <- function(gadfor,type='catch',quotayear=FALSE){
    if(type=='catch'){
        ggplot(ddply(gadfor$catch,~year+effort+trial,summarise,
                     catch=sum(biomass.consumed)/1e6),
               aes(year,catch,col=effort,lty=trial)) +
        geom_rect(aes(ymin=-Inf,ymax=Inf,
                      xmin=gadfor$sim.begin,xmax=Inf),
                  fill='gray',col='white')+
        geom_line()+ theme_bw() +
        ylab("Catch (in '000 tons)") + xlab('Year')     
    } else if(type=='ssb'){
        ggplot(ddply(gadfor$lw,~year,summarise,ssb=sum(ssb)/1e6),
               aes(year,catch,col=effort,lty=trial)) +
        geom_bar(stat=='identity') + theme_bw() +
        ylab("SSB (in '000 tons)") + xlab('Year')     
    } else if(type=='rec'){
        ggplot(ddply(gadfor$recruitment,~year,summarise,catch=sum(catch)),
               aes(year,catch,col=effort,lty=trial)) +
        geom_bar(stat='identity') + theme_bw() +
        ylab("Recruitment (in millions)") + xlab('Year')     
    }
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Gadget bootstrap forward
##' @param years
##' @param params.file
##' @param main.file
##' @param pre
##' @param effort
##' @param fleets
##' @param num.trials
##' @param bs.wgts
##' @param bs.samples
##' @param check.previous
##' @param rec.window
##' @param mat.par
##' @param stochastic
##' @param .parallel
##' @return list of bootstrap results
##' @author Bjarki Thor Elvarsson
##' @export
gadget.bootforward <- function(years = 20,
                               params.file='params.final',
                               main.file = 'main.final',
                               pre = 'PRE',
                               effort = 0.2,
                               fleets = data.frame(fleet='comm',ratio=1),
                               num.trials = 10,
                               bs.wgts = 'BS.WGTS',
                               bs.samples = 1:1000,
                               check.previous = FALSE,
                               rec.window = NULL,
                               mat.par = NULL,
                               stochastic = TRUE,
                               .parallel = TRUE){
  tmp <-
    llply(bs.samples,function(x){
        gadget.forward(years = years,
                       num.trials = num.trials,
                       params.file = sprintf('%s/BS.%s/%s',
                       bs.wgts,x,params.file),
                       rec.window = rec.window,
                       main.file = sprintf('%s/BS.%s/%s',bs.wgts,x,main.file),
                       effort = effort, fleets = fleets,
                       pre = sprintf('%s/BS.%s/%s',bs.wgts,x,pre),
                       check.previous = check.previous,
                       mat.par = mat.par,
                       stochastic=stochastic,
                       save.results = FALSE)

    },.parallel = .parallel)
  names(tmp) <- sprintf('BS.%s',bs.samples)

  out <- list(lw = ldply(tmp,function(y) y[[1]]),
              catch = ldply(tmp,function(y) y[[2]]),
              recruitment = ldply(tmp,function(y) y[[3]]),
              effort = effort,
              fleets = fleets)

  save(out,file=sprintf('%s/bsforward.RData',bs.wgts))
  return(out)
}


#' gadget.retro
#'
#' @param path 
#' @param main.file 
#' @param params.file 
#' @param optinfofile 
#' @param num.years 
#' @param pre 
#'
#' @return
#' @export
#'
#' @examples
gadget.retro <- function(path='.',main.file='main',params.file='params.in',
                         optinfofile='optinfofile',num.years=5,
                         pre = 'RETRO',
                         iterative =FALSE,...){
  
  main <- read.gadget.file(path,main.file,file_type = 'main')
  
  for(year in 1:num.years){
    attributes(main)$file_name <- paste('main',year,sep='.') 
    retrodir <- gadget.variant.dir(path, pre, main = attributes(main)$file_name)
    main[[1]]$timefile[[1]]$lastyear <- 
      main[[1]]$timefile[[1]]$lastyear - 1
    attributes(main[[1]]$timefile)$file_config$mainfile_section <- 'timefile'
    attributes(main[[1]]$timefile)$file_name <- paste('time',year,sep='.')
    write.gadget.file(main,retrodir,recursive = FALSE)
    write.gadget.file(main[[1]]$timefile,retrodir)
  }
  if(iterative){
    for(year in 1:num.years){
      gadget.iterative(main.file=sprintf('%s/main.%s',pre,year),
                       params.file = params.file,
                       optinfofile=optinfofile,
                       wgts = sprintf('%s/WGTS.%s',pre,year),
                       ...)
    }
  } else {
    run.func <- function(x){
      callGadget(l = 1,
                 main = sprintf('%s/main.%s',pre,x),
                 i = params.file,
                 p = sprintf('%s/params.retro.%s',pre,x),
                 opt = optinfofile)
      callGadget(s = 1,
                 main = sprintf('%s/main.%s',pre,x),
                 i = sprintf('%s/params.retro.%s',pre,x),
                 o = sprintf('%s/lik.retro.%s',pre,x))
      
    }
    mclapply(1:num.years,run.func, mc.cores = detectCores(logical = TRUE))
  }
}


expL50 <- function(l50,b,l){
  1/(1+exp(-b*(l-l50)))
}

logit <- function(a,b,l){
  p <- exp(a+b*l)
  p/(1+p)
}
