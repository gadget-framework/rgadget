##' This function creates a list of default values of all necessary
##' switches for gadget.simulation and gadget.skeleton functions. The
##' user can then change the values of the switches and use the
##' changed list as input to those functions.  Here the default values for
##' consumption (predation and fleet operations), migration,
##' maturation (via stock 'movement'), number of areas and their basic
##' properties, various attributes of the sub stocks such as age,
##' length and weight along with growth parameters. Also length of the
##' simulation is given a default value.  If the user wants to change
##' the default values he will need to make the changes on the
##' resulting list.
##' @title Gagdet options
##' @return a list of swithes
##' @author Bjarki Thor Elvarsson
##' @examples
##' opt <- gadget.options
##' ## change the length of the simulation to 13 years
##' opt$numobs <- 13
##' @export
##' @param type 
gadget.options <- function(type=c('simple2stock','spawning')){
  warning('The gadget-skeleton functions are deprecated, please adapt your scripts to the new gadget-update functions')
  opt <- list(
#############################################################
#
# This file contains all variables and switches
# for a 2 stock 1 area Gadget model.
#
    stocks = 
    list(imm=list(name='imm',
             init.abund = 1e6*exp(-(1:3)*0.2),
             minage = 1,
             maxage = 3,
             minlength = 5,
             maxlength = 90,
             dl = 1,
             M=0.2,
             doeseat = 0,
             iseaten = 1,
             doesmigrate = 0,
             Migration = array(c(1,0,.4,.6,.6,.4,0,1,
                 .6,.4,0,1,1,0,.4,.6),
                 c(2,2,4)),
             ## density dependent migration
             doesfuncmigrate = 0,
             diffusion = NULL,
             driftx = NULL,
             drifty = NULL,
             lambda = NULL,
             doesmature = 0,
             doesmove = 1,
             transitionstocksandratios = 'mat  1',
             transitionstep = 1,
             doesspawn = 0,
             livesonareas=1,
             doesgrow = 1,
             growth = c(linf=115,
                 k=0.09,
                 ## binn is maximum updating length
                 binn=15,
                 ## Beta for beta-binomial
                 beta=200,
                 recl=0),
             weight = c(a=10^(-5),
                 b=3),
             doesrenew = 1,
             renewal=list(minlength=4,maxlength=30),
             doesspawn = 0,
             renewal.step = 1,
             ## The standard deviation of length at i years old
             ## This vector must the same length as the number of ages.
             sigma=c(2.2472, 2.8982, 4.0705, 4.9276,
                 5.5404, 5.8072, 6.0233, 8, 9, 9),
             ## number of recruits
             n = 1e6,
             ## Meanlength for the recruits
             murec=NULL
             ),
         mat = list(name='mat',
           init.abund = 1e6*exp(-(4:10)*0.2),
           minage = 4,
           maxage = 10,
           minlength = 5,
           maxlength = 90,
           dl = 1,
           M=0.2,
           doeseat = 1,
           ## alpha and beta for the predation suitability function
           spalpha = 4.5,
           spbeta = -0.2,
           spgamma = -0.3,
           spdelta = 1,
           
           ## For the maximum consumption
           m0=10^(-2),
           m3=3,
           H=4*10^3,
           
           ## otherfrac is the fraction of otherfood that is eaten
           otherfrac=0.8,
           otherfood=50000,
           iseaten = 1,
           doesmigrate = 0,
           Migration = array(c(1,0,.4,.6,.6,.4,0,1,
             .6,.4,0,1,1,0,.4,.6),
             c(2,2,4)),
           ## density dependent migration
           doesfuncmigrate = 0,
           diffusion = NULL,
           driftx = NULL,
           drifty = NULL,
           lambda = NULL,
           doesmove = 0,
           doesmature = 0,
           doesspawn = 0,
           livesonareas=1,
           doesgrow = 1,
           growth = c(linf=115,
             k=0.09,
             ## binn is maximum updating length
             binn=15,
             ## Beta for beta-binomial
             beta=200,
               recl=0),
           weight = c(a=10^(-5),
             b=3),
           doesrenew = 0,
           doesspawn = 0,
           ## The standard deviation of length at i years old
           ## This vector must the same length as the number of ages.
           sigma=c(2.2472, 2.8982, 4.0705, 4.9276,
             5.5404, 5.8072, 6.0233, 8, 9, 9,9,9,9,9,9,9,9,9,9)
           )),
       
    area = list(
      ## Number of areas (cannot currently be more than 2)
      numofareas = 1,
      ## The portion of the stock in area 1.
      probarea=1,
      ## The temperature of the area
      area.temperature=5,
      ## areas assumed to be of equal size
      areasize=2*10^5
      ),

    time = list(numobs=20,
      firstyear = 1,
      lastyear = 20,
      ## numoftimesteps is a number of timesteps in each year
      notimesteps=c(3,3,3,3),
      firststep = 1,
      laststep = 4
      ),

    ## Variables for the catch
    ## Fleet operations              
    fleets = list(surv=list(name = 'surv',
                    livesonareas = 1,                    
                    catchstep = 2,
                    suitability = 
                    data.frame(stock = c('imm','mat'),
                               suitability = rep('exponential',2),
                               alpha= rep(-4.5,2),
                               beta= rep(0.3,2),
                               gamma = 0,
                               delta = 1,
                               stringsAsFactors = FALSE),
                    type='linearfleet',
                    multiplicative='1',
                    Fy=1e-6),
      comm=list(name = 'comm',
        livesonareas = 1,
        catchstep = 1:4,
        suitability = 
        data.frame(stock = c('imm','mat'),
                   suitability = rep('exponential',2),
                   alpha= rep(-8,2),
                   beta= rep(0.22,2),
                   gamma = 0,
                   delta = 1,
                   stringsAsFactors = FALSE),
        type='linearfleet',
        multiplicative='1',
        Fy=0.2)))

  if(type=='spawning'){
     
    opt$stocks$imm$doesrenew <- 0
    opt$stocks$mat$doesspawn <- 1
    opt$stocks$mat$spawnsteps <- 1
    opt$stocks$mat$spawnareas <- 1
    opt$stocks$mat$firstspawnyear <- opt$time$firstyear
    opt$stocks$mat$lastspawnyear <- opt$time$lastyear
    opt$stocks$mat$spawnmu <- 0.36 ## warning this is based on number of spawners not biomass as in Gadget
  }

  if(type=='ricker'){
      opt$stocks$imm$doesrenew <- 0
      opt$stocks$mat$doesspawn <- 1
      opt$stocks$mat$spawnsteps <- 1
      opt$stocks$mat$spawnareas <- 1
      opt$stocks$mat$firstspawnyear <- opt$time$firstyear
      opt$stocks$mat$lastspawnyear <- opt$time$lastyear
      opt$stocks$mat$spawnfunc <- 'ricker'
      opt$stocks$mat$spawnparameters <- c(p1=1,
                                          p2=3.5e-11)
  }

  if(type=='simple1stock'){
      opt$stocks$mat <- NULL
      opt$stocks$imm$init.abund <- 1e6*exp(-(1:10)*0.2)
      opt$stocks$imm$maxage <- 10
      opt$stocks$imm$doesmove <- 0
      opt$fleets$surv$suitability <- subset(opt$fleets$surv$suitability, stock=='imm')
      opt$fleets$comm$suitability <- subset(opt$fleets$comm$suitability, stock=='imm')
  }
  
  class(opt) <- c('gadget.options',class(opt))
  return(opt)
}

##' Build gadget model object based on simple lists
##'
##' Gadget skeleton, as the names suggests, creates a gadget-main
##' object, as S4 class, that can be used to create model input files
##' or drive the simple Gadget-like simulator implemented in
##' gadget.simulate
##' @title Gadget skeleton
##' @param time list describing the time step of the model
##' @param area list describing the spatial grid of the model
##' @param stocks list describing the stocks of the model
##' @param fleets list describing the fleets
##' @return gadget-model class
##' @author Bjarki Þór Elvarsson
##' @examples
##' opt <- gadget.options()
##' gm <- gadget.skeleton(time=opt$time,area=opt$area,
##'                       stocks=opt$stocks,fleets=opt$fleets)
##' @export
gadget.skeleton <- function(time,area,stocks,fleets=NULL){
  warning('The gadget-skeleton functions are deprecated, please adapt your scripts to the new gadget-update functions')
  ## Definition of time
  time <- new('gadget-time',
              firstyear = time$firstyear,
              firststep = time$firststep,
              lastyear = time$lastyear,
              laststep = time$laststep,
              notimesteps = time$notimesteps)
  
  ## area definitions
  area.temp <- getTimeSteps(time)
  tmp <- rep(1:area$numofareas,
             each = length(area.temp$year))
  
  area.temp$area <- 1
  area.temp$temperature <- area$area.temperature
  
  area.temp <- plyr::ddply(data.frame(area=tmp),~area,
                     function(x){
                       area.temp$area <- x$area
                       return(area.temp)
                     })
  area <- new('gadget-area',
              areas = 1:area$numofareas,
              size = area$areasize,
              temperature = area.temp)
  
  ## stock definitions

  ## recruitment age
  rec.age <- min(sapply(stocks,function(x) x$minage))
  stocks <- 
    plyr::llply(stocks,function(x){
        print(x$name)
        Growth <- new('gadget-growth',
                      growthfunction = 'lengthvbsimple',
                      growthparameters = c(x$growth[c('linf','k')],x$weight),
                      beta = x$growth['beta'], 
                      maxlengthgroupgrowth = x$growth['binn'])
        if(!is.na(x$growth['recl']) & (x$growth['recl']>0 |
                                       is.character(x$growth['recl']))){
            t0 <- sprintf('(+ %s (/ (log (- 1 (/ %s %s))) %s))',rec.age,
                          x$growth['recl'],
                          x$growth['linf'],
                          x$growth['k'])
        } else {
            t0 <- 0
        }
        mu.string <- '( * %s (-  1 (exp (* (* -1 %s ) (- %s %s)))))'
        mu <- sprintf(mu.string,
                      x$growth['linf'],x$growth['k'],
                      x$minage:x$maxage,t0)
#                      x$growth['linf'] * (1 - exp(-x$growth['k'] * 1:x$maxage))
        
        refweight <- mutate(data.frame(length = seq(x$minlength,
                                           x$maxlength,by=x$dl)),
                            weight = x$weight['a']*length^x$weight['b'])
        lengths <- refweight$length
        lenAgg <- data.frame(length = paste('len',tail(lengths,-1), sep = ''),
                             min = head(lengths,-1),
                             max = tail(lengths,-1))
        
        info <- new('gadget-prey',
                    name = x$name,
                    preylengths = lenAgg,
                    energycontent = 1)
        
        if(length(x$init.abund)==1){
          tmp <- sprintf('(exp (* -1 (* %s %s)))',
                         x$minage:x$maxage,
                         x$M)
#            exp(-(x$minage:x$maxage)*x$M)
          alive <- sprintf('(/ (* %s %s) (+ %s))',
                           tmp,x$init.abund,sum(tmp))
        } else if(length(x$init.abund)==(x$maxage - x$minage + 1)*
                    getNumOfAreas(area)){
          alive <- x$init.abund
        } else if(length(x$init.abund)==(x$maxage - x$minage + 1)){
          alive <- rep(x$probarea,each=length(x$init.abund))*
            rep(x$init.abund,getNumOfAreas(area))  
        } else {
          warning('length init.abund is not a multiple of num. stock agegroups')
          alive <- tryCatch(rep(x$init.abund[1:(x$maxage - x$minage + 1)],
                                getNumOfAreas(area)),
                            error = rep(x$init.abund[1],getNumOfAreas(area)*
                                (x$maxage - x$minage + 1)))
        }
        if(length(x$sigma)==1) {
            x$sigma <- rep(x$sigma,length(x$minage:x$maxage))
        } else if(length(x$sigma) >= x$maxage & x$minage>0){
            recsigma <- x$sigma[1]
            x$sigma <- x$sigma[x$minage:x$maxage]
        } else if(length(x$sigma)>length(x$minage:x$maxage)) {
            x$sigma <- x$sigma[1:length(x$minage:x$maxage)]
        } else {
            stop('Error -- initial sigma useless')
        }
        init <- data.frame(age = x$minage:x$maxage, 
                           area = rep(1:getNumOfAreas(area),
                                      each = x$maxage - x$minage + 1), 
                           age.factor = alive,               
                           area.factor = 1, 
                           mean =  rep(mu,
                             getNumOfAreas(area)), 
                           stddev = x$sigma,
                           alpha = as.numeric(x$weight['a']),
                           beta = as.numeric(x$weight['b']))
        if(x$doesrenew==1){ 
          if(is.null(x$renewal.data)){
            x$renewal.data <- 
              plyr:::mutate(subset(getTimeSteps(time), 
                                   step == x$renewal.step),
                            area = 1:getNumOfAreas(area),
                            
                            age = x$minage,
                            number = x$n,
                            mean = mu[1],
                            stddev = recsigma,
                            a = x$weight['a'],
                            b = x$weight['b'])
          }          
        }else {
          x$renewal.data <- data.frame()
          x$renewal <- list()
        }
        
        if(x$doesmove==0){
          x$transitionstocksandratios <- ''
          x$transitionstep <- 0
        }
        if(x$doesspawn==1){
          spawndata <- new('gadget-spawning',
                           spawnsteps = x$spawnsteps,
                           spawnareas = x$spawnareas,
                           firstspawnyear = x$firstspawnyear,
                           lastspawnyear = x$lastspawnyear,
                           spawnstocksandratio = data.frame(stock = 'imm',ratio = 1),
                           proportionfunction = c(func = 'constant', alpha = 1),
                           mortalityfunction = c(func = 'constant', alpha = 0),
                           weightlossfunction = c(func = 'constant', alpha = 0),
                           recruitment = c(func = x$spawnfunc,
                               mu = x$spawnparameters),
                           stockparameters = data.frame(mean = sprintf(mu.string,
                                                            x$growth['linf'],
                                                            x$growth['k'],1,t0),
                             stddev = recsigma, 
                             alpha = x$weight['a'], beta = x$weight['b']))
        } else {
          spawndata <- new('gadget-spawning')
        }

        if(x$doesmature==1){
          maturityfunction <- x$maturityfunction
          maturestocksandratios <- x$maturestocksandratios
          coefficients <- x$maturity.coefficients
          maturitysteps <- x$maturitysteps
        } else {
          maturityfunction <- ''
          maturestocksandratios <- '' 
          coefficients <-''
          maturitysteps <- '0'
        }
         
      if(length(x$M)==1){
        M <- rep(x$M,x$maxage - x$minage + 1)
      } else if(length(x$M)==x$maxage - x$minage + 1){
        M <- x$M
      } else {
        stop(sprintf('Length M of stock %s not equal to maxage - minage + 1', x$name))
      }
      
        
        stock <- new('gadget-stock',
                     stockname = x$name,
                     livesonareas = x$livesonareas,
                     minage = x$minage,
                     maxage = x$maxage,
                     minlength = x$minlength,
                     maxlength = x$maxlength,
                     dl = x$dl,
                     refweight = refweight,
                     doesgrow = x$doesgrow,
                     growth = Growth,
                     naturalmortality = M,
                     iseaten = x$iseaten,
                     preyinfo = info, 
                     initialconditions = list(minage = x$minage,
                                              maxage = x$maxage,
                                              minlength = x$minlength,
                                              maxlength = x$maxlength,
                                              dl = x$dl,
                                              sdev = 1),
                     initialdata = init,
                     doesspawn = x$doesspawn,
                     spawning = spawndata,
                     doesmigrate = x$doesmigrate,
                     doesrenew = x$doesrenew,
                     renewal = x$renewal,
                     renewal.data = x$renewal.data,
                     doesmove = x$doesmove,
                     transitionstocksandratios = x$transitionstocksandratios,
                     transitionstep = x$transitionstep,
                     doesmature = x$doesmature,
                     maturityfunction = maturityfunction,
                     maturestocksandratios = maturestocksandratios,
                     coefficients = coefficients,
                     maturitysteps = maturitysteps
                     )
        
  })
  
  
  ## fleet operations

  if(!is.null(fleets)){
      fleets <- plyr::llply(fleets,
                      function(x){
                          if(x$type %in% c('linearfleet','effortfleet')){
                              fleetdat <- 
                                  mutate(area.temp[c('year','step','area')],
                                         fleet=x$name,
                                         Fy=x$Fy)
                              fleetdat <- subset(fleetdat,step==x$catchstep)
                          } else {
                              fleetdat <- x$amount
                          }
                          
                          tmp <- plyr::ddply(x$suitability,~stock,function(x)
                                       c(params=paste(x[,-(1:2)],collapse=' ')))
                          fleet.suit <- data.frame(fleet=x$name,
                                                   stock=x$suitability$stock,
                                                   suitability = x$suitability$suitability,
                                                   params = tmp$params)
                          new('gadget-fleet',
                              name = x$name,
                              type = x$type,
                              livesonareas = x$livesonareas,
                              multiplicative = 1,
                              suitability = fleet.suit,
                              amount = fleetdat
                              )
                      }
                      )
  } else {
      fleets <- new('gadget-fleet')
  }
  
  model <- new('gadget-main',
                 model.name='gadget-model',
                 time = time,
                 area = area,
                 print = list(),
                 stocks = stocks,
                 tags = new('gadget-tagging'),
                 otherfood = list(),
                 fleets = fleets,
                 likelihood = list()
  )
  
}

