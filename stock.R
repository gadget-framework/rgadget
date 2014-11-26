##' Rgadget is the driver function for the ecosystem simulation. It takes a
##' gadget options list, where all parameters of the simulation are set, as
##' input and runs the simulation for a set period of time (the length of the
##' timesteps and number of observations (years) are also defined in gadget
##' options).
##'
##' The Rgadget simulator replicates some of the more commonly used features of
##' Gadget, with use of some of these features optional. The most complex model
##' possible consists of two substocks, with the younger substock optionally
##' maturing into the older substock at a given age and timestep, ie the
##' maturation process is not modelled. Both substocks are subject to growth
##' and natural mortality and live on a number of areas. Two fleets are possible, eg
##' one representing the commercial fleet and the other an annual survey, with
##' the timesteps on which these fleets operate optional. There can be
##' migration between the areas. The mature substock can also predate upon the
##' immature stock. As with Gadget, the fleets are modelled as predators. The
##' order in which the population processes are modelled on each timestep is
##' identical to that in Gadget.
##' 
##' As Gadget is a forward simulation model, it is started with an initial
##' population. The length distribution of each age group of the population
##' is assumed to be normally distributed with a given mean and standard
##' deviation (specified by the user). The youngest age group in each year
##' is entered into the population in a similar manner.
##' Fleets are modelled as predators, as in Gadget, and operate on some or all
##' areas and at some or all timesteps which are defined by the user.
##' Consumption (predation or harvesting) implemented through length based
##' suitability functions of the form:
##'  \deqn{S_{pred,prey}(L,l) = \frac{\delta}{1+e^{-\alpha-\beta l-\gamma L}}}
##' where of l is the prey length and L predator length. For fleets L should be
##' irrelevant and therefore \eqn{\gamma = 0}{gamma  = 0} for fleets.
##' 
##' Maturation from stock A to stock B is modelled by moving the oldest
##' agegroup of A into with the age increasing if done on the last timestep
##' of the year. This replicates the Gadget process \emph{doesmove}.
##'
##' Growth follows a beta-binomial distribution with mean update length as
##' stipulated by the von Bertanlanffy curve.
##' 
##' The order of calculations is the same as in Gadget and is as follows:
##'
##' 1. Migration between areas
##' 
##' 2. Consumption, including catch by the fleets
##' 
##' 3. Natural mortality
##' 
##' 4. Growth
##' 
##' 5. Recruitment
##' 
##' 6. Remove the stock, here immature, that will move
##' 
##' 7. Increase the age
##' 
##' 8. Replace the stock, here to the mature, that has moved and increase the age. 
##' @title Rgadget
##' @param opt gadget options list, as defined by 'gadget.options'
##' @return a list of arrays:
##' \item{Rec}{Recruits for all years}
##' \item{immStart}{Initial starting population for the immature stock age 2 and older}
##' \item{matStart}{Initial starting population for the mature stock age 2 and older}
##' \item{immNumRec}{Immature stock population for all timesteps, areas, ages and lengths}
##' \item{matNumRec}{Mature stock population for all timesteps, areas, ages and lengths}
##' \item{immCsurv}{Survey catches of the immature stock}
##' \item{matCsurv}{Survey catches of the mature stock}
##' \item{immCcomm}{Commercial catches of the immature stock}
##' \item{matCcomm}{Commercial catches of the mature stock}
##' \item{Eat}{Amount consumed of immatures by matures}
##' \item{GrowthProb}{Growthprobability matrix}
##' \item{immMort}{Natural mortality for the immature stock}
##' \item{matMort}{Natural mortality for the mature stock}
##' \item{opt}{Gadget options list used in the simulation}
##'  @author Bjarki Thor Elvarsson, Asta Jenny Sigurdardottir and Elinborg Ingunn Olafsdottir
##'  @export
##' @examples
##' opt <- gadget.options('simple2stock')
##' gm <- gadget.skeleton(time=opt$time,area=opt$area,stock=opt$stocks,opt$fleets)
##' sim <- gadget.simulate(gm)
gadget.simulate <- function(gm, params=data.frame(),
                            maxratioconsumed = 0.95){
  
  ## stock in numbers
  stkArr <- llply(gm@stocks,function(x){    
    tm <- getTimeSteps(gm@time)
    stk <- 
      array(0,c(getNumOfAreas(gm),
                getNumLengthGroups(x),
                getMaxage(x),
                nrow(tm)),
            dimnames = list(area = getAreas(gm),
                            length = getLengthGroups(x),
                            age = 1:getMaxage(x),
                            time = sprintf('Year_%s_Step_%s',tm$year,tm$step))
      )
    ## initial data:
    if(x@doesrenew==1){
      tmp <- 1
    } else {
      tmp <- 0
    }

    initData <- subset(getInitData(x,params),
                       age %in% (getMinage(x)+tmp):getMaxage(x))
    lg <- getLengthGroups(x)
    lg <- c(min(lg)-x@dl,lg)
#    print(x@stockname)
    stk[getAreas(gm),,(getMinage(x)+tmp):getMaxage(x),1] <- 
      acast(ddply(initData,~age+area,function(y){
        data.frame(length = lg[-1],
                   num=y$age.factor*
                     distr(y$mean,y$stddev,lg))
        }),
        area~length~age,value.var='num')[getAreas(gm),,]
    
    ## recruitment
    if(x@doesrenew == 1){
      rec <- subset(getRecruitment(x,params),
                    year <= tail(tm$year,1) &
                      step <= tail(tm$step,1))
      stk[getAreas(gm),,getMinage(x),
          sprintf('Year_%s_Step_%s',rec$year,rec$step)] <- 
        acast(ddply(rec,~year+step+area+age,function(y){
          data.frame(length = lg[-1],
                     num=y$number*distr(y$mean,y$stddev,lg))
        }),
        area~length~year+step,value.var='num')[getAreas(gm),,]
    }
    return(stk)
  })  

  dt <- (gm@time@notimesteps)/12
  
  ## Natural (unexplained) mortality
  M <- llply(gm@stocks, function(x){
    mort <- getMortality(x,par=params)$V1
    ## if getMinage(x)>1
    outer(diag(c(rep(0,getMinage(x)-1),exp(-mort))),dt,'^')
  })
  

  # Defines the catch matrices
  fleetArr <- llply(stkArr, function(x){
    tmp <- 0*((1:getNumFleets(gm))%o%x)
    dimnames(tmp)[[1]] <- getFleetNames(gm)
    names(dimnames(tmp))[1] <- 'fleet'
    tmp
  })
  
  fleetSuit <- getFleetSuitability(gm,params)
  
  
  #################################
  #
  # Predation
  #
  
  # Consumptions of other stocks
  
  if(getNumPredators(gm)>0){
    EatArr <- llply(stkArr, function(x){
      tmp <- (1:getNumStocks(gm))%o%x
      dimnames(tmp)[[1]] <- getStockNames(gm)
      names(dimnames(tmp))[1] <- 'Predator'
      tmp
    })    
    predSuit <- getPredatorSelection(gm)
  } else {
    EatArr <- NULL
    predSuit <- NULL
  } 
  
  ## Length update matrix
  stkG <- llply(gm@stocks,
                  function(x){
                    getGrowth(x,params)
                  })

  
  ## migration matrix
  stkMig <- llply(gm@stocks,
                     function(x){
                       if(x@doesmigrate == 1){
                         getMigrationMatrix(x,params)
                       } else {
                         NA
                       }
                     })

  
  clock <- getTimeSteps(gm@time)
  for(i in 1:nrow(clock))    {
#    print(i)
    curr.step <- clock$step[i]
    curr.year <- clock$year[i]
    
    if(curr.step != 1){    
      for(stock in getStockNames(gm)){ 
        stkArr[[stock]][,,,i] <- stkArr[[stock]][,,,i-1]
      }
    } else if(i>1) { 
      ###### if we are in timestep 1 we have to update age ######
      
      for(stock in getStockNames(gm)){
        minage <- getMinage(gm@stocks[[stock]])  
        maxage <- getMaxage(gm@stocks[[stock]])
        
        stkArr[[stock]][,,-(1:minage),i] <- 
          stkArr[[stock]][,,minage:(maxage-1),i-1]
        
        ## plus groups
        if(gm@stocks[[stock]]@doesmove != 1){
          stkArr[[stock]][,,maxage,i] <-
            stkArr[[stock]][,,maxage,i] + 
            stkArr[[stock]][,,maxage,i-1]
        }
      }
      
      for(stock in getStockNames(gm)){
        ## hack for one area
        if(gm@stocks[[stock]]@doesspawn == 1){
          if(curr.step %in% gm@stocks[[stock]]@spawning@spawnsteps){
            tmp <- gm@stocks[[stock]]@spawning@spawnstocksandratio
            tmp$stock <- as.character(tmp$stock)
            for(stkInd in 1:nrow(tmp)){
              lg <- getLengthGroups(gm@stocks[[tmp[stkInd,1]]])
              total.num <- gm@stocks[[stock]]@spawning@recruitment$mu[1]*
                sum(stkArr[[stock]][,,,i-1])*tmp[stkInd,2]
              y <- gm@stocks[[stock]]@spawning@stockparameters
              stkArr[[tmp[stkInd,1]]][,,getMinage(gm@stocks[[tmp[stkInd,1]]]),i] <-
                 acast(data.frame(length = lg[-1],
                                  num=total.num*distr(y$mean,y$stddev,lg)),
                       area~length,value.var='num')
            }
          }
        }
      
        if(gm@stocks[[stock]]@doesmove == 1){
          tmp <- gm@stocks[[stock]]@transitionstockandratios
          tmp$stock <- as.character(tmp$stock)
          for(stkInd in 1:nrow(tmp)){
            stkArr[[tmp[stkInd,1]]][,,getMinage(gm@stocks[[tmp[stkInd,1]]]),i] <-
              stkArr[[tmp[stkInd,1]]][,,getMinage(gm@stocks[[tmp[stkInd,1]]]),i] + 
              stkArr[[stock]][,,getMaxage(gm@stocks[[stock]]),i-1]*tmp[stkInd,2]
          }
        }
        
      }
     
    
      if(getNumOfAreas(gm)>1){
        for(stock in getStockNames(gm)){
          if(gm@stocks[[stock]]@doesmigrate==1)
            stkArr[[stock]][,,,i] <- migrate(stkArr[[stock]][,,,i-1],
                                             stkMig[[stock]][,,curr.step])
        }
      }
    }
    

    ############
    # Consumption calculations
    
    if(getNumPredators(gm) > 0){
      for(stock in getStockNames(gm)){ 
        if(gm@stocks[[stock]]@doeseat == 1){
          for(prey in names(predSuit[[stock]])){
            EatArr[[prey]][stock,,,,i] <- eat(stkArr[[prey]],stkArr[[stock]],i,predSuit[[stock]][[prey]])
          }
          
        }
      } 
    }
    
    for(fleet in getFleetNames(gm)){
      tmp <- subset(gm@fleets[[fleet]]@amount,
                    year==curr.year & step == curr.step)
      
      if(nrow(tmp)>0){
        for(stock in getStockNames(gm)){
          if(stock %in% gm@fleets[[fleet]]@suitability$stock){
            l <- getLengthGroups(gm@stocks[[stock]])
            suit <- array(as.numeric(fleetSuit[[fleet]][[stock]](l)),
                          dim = c(length(l),getMaxage(gm@stocks[[stock]])))
            if(gm@fleets[[fleet]]@type == 'linearfleet'){
              fleetArr[[stock]][fleet,tmp$area,,,i] <- 
                as.numeric(tmp$Fy)*dt[curr.step]*
                stkArr[[stock]][tmp$area,,,i]*suit
              
            } else if (gm@fleets[[fleet]]@type == 'totalfleet'){
              w  <- getWeight(gm@stocks[[stock]],l,params)
              harv.biomass <- 
                sum(stkArr[[stock]][tmp$area,,,i]*suit*w)
              fleetArr[[stock]][fleet,tmp$area,,,i] <- 
                (as.numeric(tmp$amount)*
                   stkArr[[stock]][tmp$area,,,i]*suit)/harv.biomass
                                    
            }            
          }
        }    
      }
    }
    
  #############
  # Overconsumption check
  tempC<-adjustconsumption(catches = fleetArr,
                           predation = EatArr,
                           stocks = stkArr,
                           i = i,
                           maxratioconsumed)
                                  
  #############
  # Subtract Consumption from stock
  for(stock in getStockNames(gm)){
    stkArr[[stock]][,,,i] <- stkArr[[stock]][,,,i] - tempC[[stock]]      
    x <- gm@stocks[[stock]]
    for(area in 1:getNumOfAreas(gm)){
      for(age in getMinage(x):getMaxage(x)){
        stkArr[[stock]][area,,age,i] <- 
          t(stkG[[stock]](dt[curr.step]))%*%
          stkArr[[stock]][area,,age,i]%*%
          M[[stock]][age,age,curr.step]    
      }
    }
  }



  ###########
  # Length update and natural mortality
      
  ###########
  # Recruits
     
    }

  
  sim <- list(gm=gm,
              stkArr=stkArr,
              fleetArr=fleetArr,
              EatArr = EatArr,
              params = params)
  class(sim) <- c('gadget.sim',class(sim))
  return(sim)
}

