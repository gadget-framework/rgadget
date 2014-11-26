##' This function creates a list of default values of all necessary switches
##' for the RGadget simulation. The user can then change the
##' values of the switches and use the changed list as input to RGadget.
##' Here the default values for consumption (predation and fleet operations),
##' migration, maturation (via stock 'movement'), number of areas and their
##' basic properties, various attributes of the sub stocks such as age, length
##' and weight along with growth parameters. Also length of the simulation is
##' given a default value.
##' If the user wants to change the default values he will need to make the
##' changes on the resulting list.
##' @title Gagdet options
##' @return a list of swithes
##' \item{stocks}{names of the stocks in the simulation}
##' \item{doeseat}{Does the 'mature' stock eat the 'immature'}
##' \item{doescatchsurv}{Is there a survey fleet}
##' \item{doescatchcomm}{Is there a commercial fleet}
##' \item{doesmigrateimm}{Does the immature stock migrate}
##' \item{doesmigratemat}{Does the mature stock migrate}
##' \item{immMigration}{Migration matrix for the immmature substock}
##' \item{matMigration}{Migration matrix for the mature substock}
##' \item{doesfuncmigrate}{(migration) pde's used to describe migration.}
##' \item{diffusion}{(migration) diffusion parameter}
##' \item{driftx}{(migration) drift in x coordinate}
##' \item{drifty}{(migration) drift in y coordinate}
##' \item{doesmove}{Does the immature stock mature into the mature stock}
##' \item{numofareas}{Number of gadget areas}
##' \item{probarea}{A vector of proportions in a given area, assumed equal for both stocks}
##' \item{areasize}{Size of the gadget area (assumed equal for all areas}
##' \item{area.temperature}{Average temperature of the area}
##' \item{immminage}{Minimum age of the immmature stock}
##' \item{immmaxage}{Maximum age of the immmature stock}
##' \item{matminage}{Minimum age of the mature stock}
##' \item{matmaxage}{Maximum age of the mature stock}
##' \item{minlen}{Minimum length of both stocks}
##' \item{maxlen}{Maximum length of both stocks}
##' \item{lengthgrouplen}{Size of each lengthgroup. We assume the size of the 
##' lengthgroups is the same for both stocks.}
##' \item{a}{a in the length-weight relationship a*l^b}
##' \item{b}{b in the length-weight relationship a*l^b}
##' \item{sigma}{The standard deviation of length at i years old. 
##' This vector must the same length as the number of ages.}
##' \item{n}{Number of recruits per year.}
##' \item{murec}{If specified this will be the meanlength of recruits}
##' \item{lsup}{L-infinity. Bertalanffy growth parameters lsup, and k for 
##' the growth function (used for all ages > 1)}
##' \item{binn}{binn is the maximum updating length}
##' \item{beta}{Beta for beta-binomial}
##' \item{numobs}{number of years observed}
##' \item{numoftimesteps}{number of timesteps in each year}
##' \item{z}{z is the natural mortality constant used to calculate the size 
##' of the initial population for age 2 +}
##' \item{spalpha}{alpha for the predation suitability function}
##' \item{spbeta}{beta for the predation suitability function}
##' \item{spagamma}{gamma for the predation suitability function}
##' \item{spdelta}{delta for the predation suitability function}
##' \item{m0}{m0 for the maximum consumption}
##' \item{m3}{m3 for the maximum consumption}
##' \item{H}{H The density (biomass per area unit) of available food at which 
##' the predator can consume half maximum consumption}
##' \item{otherfrac}{the fraction of otherfood that is eaten}
##' \item{otherfood}{The maximum portion consumed, in Gadget it is 0.95, this is 
##' known as understocking in Gadget}
##' \item{survstep}{timestep(s) for the survey}
##' \item{commstep}{timestep(s) for the commercial effort}
##' \item{salphasurv}{for the suitability function - survey}
##' \item{sbetasurv}{for the suitability function - survey}
##' \item{survfleettype}{Fleettype for the survey}
##' \item{survmultiplicative}{For the fleettype}
##' \item{Fysurv}{Fishing effort of the survey}
##' \item{surv.catches}{What stocks does the survey fleet catch from}
##' \item{salphacomm}{for the suitability function - commerical catch}
##' \item{sbetacomm}{for the suitability function - commercial catch}
##' \item{commfleettype}{Fleettype for the commercial catch}
##' \item{comm.catches}{What stocks does the commercial fleet catch from}
##' \item{commmultiplicative}{For the fleettype}
##' \item{Fycomm}{Fishing effort of the commercial catch}
##' @author Bjarki Thor Elvarsson, Asta Jenny Sigurdardottir and Elinborg Ingunn Olafsdottir
##' @examples
##' opt <- gadget.options
##' ## change the length of the simulation to 13 years
##' opt$numobs <- 13
gadget.options <- function(type=c('simple2stock','spawning')){
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
           doesmigrate = 1,
           Migration = array(c(1,0,.4,.6,.6,.4,0,1,
             .6,.4,0,1,1,0,.4,.6),
             c(2,2,4)),
           ## density dependent migration
           doesfuncmigrate = 0,
           diffusion = NULL,
           driftx = NULL,
           drifty = NULL,
           lambda = NULL,
           doesmove = 1,
           transitionstocksandratios = data.frame(stock='mat',ratio=1),
           transitionstep = 1,
           doesspawn = 0,
           livesonareas=1,
           doesgrow = 1,
           growth = c(linf=115,
             k=0.09,
             ## binn is maximum updating length
             binn=15,
             ## Beta for beta-binomial
             beta=200),
           weight = c(a=10^(-5),
             b=3),
           doesrenew = 1,
           renewal=list(minage=4,maxage=30),
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
           doesmigrate = 1,
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
           doesspawn = 0,
           livesonareas=1,
           doesgrow = 1,
           growth = c(linf=115,
             k=0.09,
             ## binn is maximum updating length
             binn=15,
             ## Beta for beta-binomial
             beta=200),
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
  
  class(opt) <- c('gadget.options',class(opt))
  return(opt)
}

##' This function is a helper function for RGadget.
##' it calculates additional options and switches that can be derived from
##' the gadget options lists.
##' @title Derived options
##' @param opt gadget options list
##' @return augmented gadget options list
derivedOptions <- function(opt){
  within(opt,{
    time$lengthoftimesteps <- 12/time$numoftimesteps  
    if(length(area$probarea) < area$numofareas){
      area$probarea <- rep(1,area$numofareas)/area$numofareas
      warning('length(opt$area$probarea)<opt$area$numofareas - 
              equal initial area probabilites assumed')
    }
    for(stock in names(stocks)){
      if(stocks[[stock]]$doesrenew==1){
        if(length(stocks[[stock]]$doesrenew)==1){
          stocks[[stock]]$n <- 
            rep(c(stocks[[stock]]$n,rep(0,time$numoftimesteps)),time$numobs)
        } else if(length(stocks[[stock]]$doesrenew)==time$numobs){
          stocks[[stock]]$n <- as.numeric(rbind(stocks[[stock]]$n,
                                                array(0,c(time$numoftimesteps-1,time$numobs))))
        } else if(length(stocks[[stock]]$doesrenew)!=time$numobs*time$numoftimesteps){
          warning('Recruitment vector has length less the total number of timesteps. Expect errors.')
        }
      }
      if(length(stocks[[stock]]$M)){
        stocks[[stock]]$M <- rep(stocks[[stock]]$M,
                                 stocks[[stock]]$maxage-stocks[[stock]]$minage+1)   
      } else if (length(stocks[[stock]]$M) != stocks[[stock]]$maxage-
                   stocks[[stock]]$minage+1) {
        warning('Natural mortality ill defined')
      }
      if(stocks[[stock]]$doeseat==1){
        if(length(stocks[[stock]]$otherfood)==1){
          stocks[[stock]]$otherfood <- 
            rep(stocks[[stock]]$otherfood,
                time$numobs*time$numoftimesteps*area$numofareas)
          dim(stocks[[stock]]$otherfood) <- 
            c(time$numofareas,time$numobs*time$numoftimesteps)
        }
      } 
      stocks[[stock]]$l <- seq(stocks[[stock]]$minlen,
                               stocks[[stock]]$maxlen,
                               stocks[[stock]]$dl)
      
      stocks[[stock]]$numoflgroups <- length(stocks[[stock]]$l)-1
      stocks[[stock]]$lt <- (stocks[[stock]]$l[2:length(stocks[[stock]]$l)]+
                               stocks[[stock]]$l[1:(length(stocks[[stock]]$l)-1)])/2
      
      stocks[[stock]]$w <- stocks[[stock]]$weight['a']*
        stocks[[stock]]$lt^stocks[[stock]]$weight['b']
      
      if(stocks[[stock]]$doesgrow == 1){
        stocks[[stock]]$mu <- stocks[[stock]]$growth['linf']*
          (1-exp(-stocks[[stock]]$growth['k']*1:stocks[[stock]]$maxage))
      }
      if(!is.null(stocks[[stock]]$murec)){
        stocks[[stock]]$mu[1] <- stocks[[stock]]$murec
      }
      
      if(stocks[[stock]]$doeseat == 1){      
        stocks[[stock]]$maxConsumption <- 
          stocks[[stock]]$m0*stocks[[stock]]$lt^stocks[[stock]]$m3*12*time$dt 
      }
      if(area$numofareas==1){
        stocks[[stock]]$doesmigrate <- 0
      } 
    }
    for(fleet in names(fleets)){
      fleets[[fleet]]$Fy <- rep(fleets[[fleet]]$Fy, 
                                time$numobs/length(fleets[[fleet]]$Fy))
    }
    time$dt <- 1/time$numoftimesteps
  })
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
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
gadget.skeleton <- function(time,area,stocks,fleets){
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
  
  area.temp <- ddply(data.frame(area=tmp),~area,
                     function(x){
                       area.temp$area <- x$area
                       return(area.temp)
                     })
  area <- new('gadget-area',
              areas = 1:area$numofareas,
              size = area$areasize,
              temperature = area.temp)
  
  ## stock definitions
  stocks <- 
    llply(stocks,function(x){
      print(x$name)
        Growth <- new('gadget-growth',
                      growthfunction = 'lengthvbsimple',
                      growthparameters = c(x$growth[c('linf','k')],x$weight),
                      beta = x$growth['beta'], 
                      maxlengthgroupgrowth = x$growth['binn'])
        
        mu <- sprintf('( * %s (-  1 (exp (* (* -1 %s) %s))))',
                      x$growth['linf'],x$growth['k'],1:x$maxage)
#                      x$growth['linf'] * (1 - exp(-x$growth['k'] * 1:x$maxage))
        
        refweight <- mutate(data.frame(length = seq(x$minlength,x$maxlength,by=x$dl)),
                            weight =  x$weight['a']*length^x$weight['b'])
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
          alive <- tryCatch(rep(x$init.abund[1:(x$maxage - x$minage + 1)],getNumOfAreas(area)),
                            error = rep(x$init.abund[1],getNumOfAreas(area)*
                              (x$maxage - x$minage + 1)))
        }
        init <- data.frame(age = x$minage:x$maxage, 
                           area = rep(1:getNumOfAreas(area),
                                      each = x$maxage - x$minage + 1), 
                           age.factor = alive,               
                           area.factor = 1, 
                           mean =  rep(mu[x$minage:x$maxage],
                             getNumOfAreas(area)), 
                           stddev = x$sigma[x$minage:x$maxage],
                           alpha = x$weight['a'],
                           beta = x$weight['b'])
        if(x$doesrenew==1){ 
          if(is.null(x$renewal.data)){
            x$renewal.data <- 
              mutate(subset(getTimeSteps(time), 
                            step == x$renewal.step),
                     area = 1:getNumOfAreas(area),
                     
                     age = x$minage,
                     number = x$n,
                     mean = mu[x$minage],
                     stddev = x$sigma[x$minage],
                     a = x$weight['a'],
                     b = x$weight['b'])
          }          
        }else {
          x$renewal.data <- data.frame()
          x$renewal <- list()
        }
        
        if(x$doesmove==0){
          x$transitionstocksandratios <- data.frame()
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
                           recruitment = data.frame(func = 'simplessb', mu = x$spawnmu),
                           stockparameters = data.frame(mean = mu[x$minage],
                             sttdev = x$sigma[x$minage], 
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
          maturestocksandratios <- list()
          coefficients <- list()
          maturitysteps <- 0
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

  
  fleets <- llply(opt$fleets,
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
                      
                    tmp <- ddply(x$suitability,~stock,function(x)
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
