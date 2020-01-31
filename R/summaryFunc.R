##' Calculates the survey indices for the simulated stocks. 
##' @name survey.index
##' @title Survey indices
##' @param stock.dat Results from a Rgadget simulation
##' @param split Number of length groups to divide data into
##' @param sigma sigma for a log-normal noise for the indicies
##' @param alpha log(q), i.e. the constant term 
##' @param beta power for the N, i.e. the slope term
##' @return Dataframe with the survey indices 
##' @author Bjarki Þór Elvarsson
survey.index <- function(stock.dat,split,sigma=0,alpha=0,beta=1){
  ##Calculates the total catch  
  
  stock.dat$SIgroup <- cut(stock.dat$length,split)
  sidat <- stock.dat %>%
    dplyr::group_by(.data$SIgroup,.data$year,.data$step) %>%
    dplyr::summarise(SI=exp(.data$alpha)*sum(.data$num)^.data$beta)
  if(sum(abs(sigma))!=0){
      if(length(sigma) == (length(split)-1)){
          sigma <- rep(sigma,each=length(unique(sidat$year)))
      }
      sidat$SI <- sidat$SI*exp(stats::rnorm(nrow(sidat),0,sigma^2)-sigma^2/2)
  }
  return(sidat)
}

##' @title Length distributions
##' @name ldist
##' @param stock.dat input data
##' @param sigma std. deviation
##' @param dl length group size
##' @return data.frame
##' @author Bjarki Thor Elvarsson
ldist <- function(stock.dat,sigma=0,dl=1){
  stock.dat$lgroup <- cut(stock.dat$length,
                          seq(min(stock.dat$length)-1,
                              max(stock.dat$length),
                              by=dl))
  ldist <- stock.dat %>%
    dplyr::group_by(.data$lgroup,.data$year,.data$step) %>%
      dplyr::summarise(num=sum(.data$num))
  if(sigma!=0){
      
    ldist$num <- ldist$num*exp(stats::rnorm(nrow(ldist),0,sigma^2)-sigma^2/2)
  }
  ldist <- ldist %>%
    dplyr::group_by(.data$year,.data$step,add=FALSE) %>%
    dplyr::mutate(p=.data$num/sum(.data$num))
  return(ldist)
}

##' @title Age length dist
##' @name aldist 
##' @param stock.dat input data
##' @param sigma std. deviations
##' @param dl length group size
##' @return data.frame
##' @importFrom rlang .data
##' @author Bjarki Thor Elvarsson
aldist <- function(stock.dat,sigma=0,dl=1){
  stock.dat$lgroup <- cut(stock.dat$length,
                          seq(min(stock.dat$length)-1,
                              max(stock.dat$length)+1,
                              by=dl))
  aldist <- stock.dat %>%
    dplyr::group_by(.data$age,.data$lgroup,.data$year,.data$step) %>%
      dplyr::summarise(num=sum(.data$num))
  if(sigma!=0){
    aldist$num <- aldist$num*exp(stats::rnorm(nrow(aldist),0,sigma^2)-sigma^2/2)
  }
  aldist <- aldist %>%
    dplyr::group_by(.data$year,.data$step,add=FALSE) %>%
    dplyr::mutate(p=.data$num/sum(.data$num))
  return(aldist)
}

##' @title toDataFrame
##' @name toDataFrame
##' @param sim gadget simulation object (from gadget.simulate)
##' @return data.frame
##' @author Bjarki Thor Elvarsson
toDataFrame <- function(sim){
  worker.fun <- function(x){
    tmp <- as.data.frame.table(x,responseName='num',
                               stringsAsFactors=FALSE)
    tmp$length <- as.numeric(tmp$length)
    tmp$age <- as.numeric(tmp$age)
    tmp$year <- sapply(strsplit(tmp$time,'_'),
                       function(y) as.numeric(y[2]))
    tmp$step <- sapply(strsplit(tmp$time,'_'),
                       function(y) as.numeric(y[4]))

    return(tmp)
  }
  stocks <- plyr::ldply(sim$stkArr,worker.fun)
  fleets <- plyr::ldply(sim$fleetArr,worker.fun)
  return(list(stocks=stocks,fleets=fleets))
}
  


##' Calculate the survey length index based on the provided lengthgroups
##' @title Survey length index
##' @name survey.indexlen
##' @param sim Results from a Rgadget simulation
##' @param length.groups A vector of cutoff points for the length groups
##' @param sigma sigma for a log-normal error
##' @return Dataframe containing the length index from the 
##' @author Bjarki Þór Elvarsson
survey.indexlen <- function(sim,length.groups=c(4,14,90),sigma=0){
  opt <- sim$opt
  Index <- rbind(as.data.frame.table(sim$immNumRec,stringsAsFactors=FALSE),
                 as.data.frame.table(sim$matNumRec,stringsAsFactors=FALSE))
  Index$year <- sapply(strsplit(Index$time,'_'),
                       function(x) as.numeric(x[2]))
  Index$step <- sapply(strsplit(Index$time,'_'),
                       function(x) as.numeric(x[4]))
  Index <- Index[Index$step==opt$survstep,]
  Index$length <- as.numeric(Index$length)
  Index$length.group <- cut(Index$length,
                            breaks=length.groups,
                            labels=sprintf('lengp%s',
                              1:(length(length.groups)-1)))
  IndexLen <- stats::aggregate(Index$Freq,
                        by=list(
                          year=Index$year,
                          step=Index$step,
                          area=sprintf('area%s',Index$area),
                          length.group=Index$length.group),
                        sum)
  names(IndexLen)[5] <- 'index'
  IndexLen$x <- IndexLen$index*exp(stats::rnorm(length(IndexLen$index),0,
                                     sigma^2) - sigma^2/2)
  
  IndexLen$time <- IndexLen$year+(IndexLen$step - 1)/opt$numoftimesteps
  
  class(IndexLen) <- c('Rgadget',class(IndexLen))
  attr(IndexLen,'formula') <- index~time|area
  attr(IndexLen,'plotGroups') <- 'length.group'
  attr(IndexLen,'plotType') <- 'l'
  attr(IndexLen,'xaxis') <- 'Year'
  attr(IndexLen,'yaxis') <- 'Survey Length Index'
  attr(IndexLen,'plotFun') <- 'xyplot'
  return(IndexLen)
}

##' Calculate the length distribution from the fleet by length groups and time.
##' @title Length Dist 
##' @name lengthDist
##' @param sim Results from a Rgadget simulation
##' @param sigma sigma for a lognormal noise
##' @return Dataframe containing the fleet length distribution.
##' @author Bjarki Þór Elvarsson
lengthDist <- function(sim,sigma=0){
  opt <- sim$opt
  tmp.surv <- rbind(as.data.frame.table(sim$immCsurv,stringsAsFactors=FALSE),
                    as.data.frame.table(sim$matCsurv,stringsAsFactors=FALSE))
  tmp.comm <- rbind(as.data.frame.table(sim$immCcomm,stringsAsFactors=FALSE),
                    as.data.frame.table(sim$matCcomm,stringsAsFactors=FALSE))
  tmp.surv$fleet <- 'surv'
  tmp.comm$fleet <- 'comm'
  Index <- rbind(tmp.surv,tmp.comm)
  Index$year <- sapply(strsplit(Index$time,'_'),
                          function(x) as.numeric(x[2]))
  Index$step <- sapply(strsplit(Index$time,'_'),
                          function(x) as.numeric(x[4]))
  Index <- Index[Index$fleet=='comm'|Index$step==opt$survstep,]
  IndexLen <- stats::aggregate(Index$Freq,
                         by=list(
                           year=Index$year,
                           step=Index$step,
                           area=sprintf('area%s',Index$area),
                           age=rep('allages',length(Index$year)),
                           length=as.numeric(Index$length),
                           fleet=Index$fleet),                           
                         sum)
  IndexLen$x <- IndexLen$x*exp(stats::rnorm(length(IndexLen$x),0,
                                     sigma^2) - sigma^2/2)
  IndexLen$time <- IndexLen$year+(IndexLen$step - 1)/opt$numoftimesteps
  class(IndexLen) <- c('Rgadget',class(IndexLen))
  attr(IndexLen,'formula') <- x~length|as.ordered(year)+as.factor(area):as.factor(fleet)
  attr(IndexLen,'plotGroups') <- 'step'
  attr(IndexLen,'plotType') <- 'l'
  attr(IndexLen,'xaxis') <- 'Year'
  attr(IndexLen,'yaxis') <- 'Length Index'
  attr(IndexLen,'plotFun') <- 'xyplot'
  
  return(IndexLen)
}
##' Calculates the age-length-key for the survey and commercial fleet.
##' @title Age length key
##' @name age.length.key
##' @param sim Results from a Rgadget simulation
##' @param age.agg The desired age aggregation
##' @param len.agg The desired length aggregation
##' @return Dataframe containing the age - length key
##' @author Bjarki Þór Elvarsson
age.length.key <- function(sim,age.agg,len.agg){
   ## age length table
  opt <- sim$opt
  alk.table <- function(catch,age.agg,len.agg){
    catch.table <- as.data.frame.table(catch,stringsAsFactors=FALSE)
    catch.table$year <- sapply(strsplit(catch.table$time,'_'),
                               function(x) as.numeric(x[2]))
    catch.table$step <- sapply(strsplit(catch.table$time,'_'),
                               function(x) as.numeric(x[4]))
    catch.table$age.agg <-
      ordered(1 + round((as.numeric(catch.table$age) - opt$minage)/age.agg))
    levels(catch.table$age.agg) <- paste('age',
                                         levels(catch.table$age.agg),
                                         sep='')
    catch.table$length.agg <-
      ordered(1 + round((as.numeric(catch.table$length) - opt$minlen)/len.agg))
    levels(catch.table$length.agg) <- paste('len',
                                            levels(catch.table$length.agg),
                                            sep='')
    tmp <- stats::aggregate(catch.table$Freq,
                     by=list(
                       year=catch.table$year,
                       step=catch.table$step,
                       area=paste('area',catch.table$area,sep=''),
                       age=catch.table$age.agg,
                       length=catch.table$length.agg),
                     sum)    
    if(len.agg==(opt$maxlen-opt$minlen))
      tmp$length <- ordered('alllen')
    if(age.agg==(opt$maxage))
      tmp$age <- ordered('allages')
    return(tmp)
  }

  immComm <- alk.table(sim$immCcomm,
                       age.agg,
                       len.agg)
  matComm <- alk.table(sim$matCcomm,
                       age.agg,
                       len.agg)
  immSurv <- alk.table(sim$immCsurv,
                       age.agg,
                       len.agg)
  matSurv <- alk.table(sim$matCsurv,
                       age.agg,
                       len.agg)
  comm <- merge(immComm,matComm,
                by=c('year','step','area','length','age'),
                all=TRUE,
                suffixes=c('imm','mat'))
  comm$ximm[is.na(comm$ximm)] <- 0
  comm$xmat[is.na(comm$xmat)] <- 0
  comm$total.catch <- comm$ximm + comm$xmat
  comm <- comm[!(comm$total.catch==0),]
  comm$ximm <- NULL
  comm$xmat <- NULL
  comm$fleet <- 'comm'
  surv <- merge(immSurv,matSurv,
                by=c('year','step','area','length','age'),
                all=TRUE,
                suffixes=c('imm','mat'))
  surv$ximm[is.na(surv$ximm)] <- 0
  surv$xmat[is.na(surv$xmat)] <- 0
  surv$total.catch <- surv$ximm + surv$xmat
  surv <- surv[!(surv$total.catch==0),]
  surv$ximm <- NULL
  surv$xmat <- NULL
  surv$fleet <- 'surv'
  alk <- rbind(surv,comm)

  alk$time <- alk$year + (alk$step-1)/4
  class(alk) <- c('Rgadget',class(alk))
  attr(alk,'formula') <- total.catch~as.numeric(age)+as.numeric(length)|as.ordered(time) + as.factor(area):as.factor(fleet)
  attr(alk,'plotGroups') <- ''
  attr(alk,'plotType') <- ''
  attr(alk,'xaxis') <- 'Year'
  attr(alk,'yaxis') <- 'Age - Length - Key'
  attr(alk,'plotFun') <- 'contour'
  attr(alk,'layout') <- ''
  return(alk)

}
##' Calculates the overall weigth of the catches by time step and area.
##' @title Catch in Kilos 
##' @name catch.in.kilos
##' @param sim Results from a Rgadget simulation
##' @return Dataframe with the catch in kilos by timestep and ared.
##' @author Bjarki Þór Elvarsson
catch.in.kilos <- function(sim){
  opt <- sim$opt
  commAmount <- apply(apply(sim$immCcomm,c(1,3,4),
                            function(x) opt$w*x),
                      c(2,4),sum) +
                        apply(apply(sim$matCcomm,c(1,3,4),
                                    function(x) opt$w*x),
                              c(2,4),sum)
  commAmount <- as.data.frame.table(commAmount,stringsAsFactors=FALSE)
  commAmount$year <- sapply(strsplit(commAmount$time,'_'),
                          function(x) as.numeric(x[2]))
  commAmount$step <- sapply(strsplit(commAmount$time,'_'),
                          function(x) as.numeric(x[4]))
  commAmount$time <- commAmount$year+(commAmount$step - 1)/opt$numoftimesteps
  commAmount <- commAmount[commAmount$Freq!=0,]
  commAmount$area <- sprintf('area%s',commAmount$area)
  commAmount$fleet <- 'comm'
  commAmount <- commAmount[c('year','step','area','fleet','Freq','time')]
  names(commAmount)[5] <- 'catch.in.kilos'

  class(commAmount) <- c('Rgadget',class(commAmount))
  attr(commAmount,'formula') <- catch.in.kilos~time|area
  attr(commAmount,'plotGroups') <- ''
  attr(commAmount,'plotType') <- 'l'
  attr(commAmount,'xaxis') <- 'Year'
  attr(commAmount,'yaxis') <- 'Catch in kilos'
  attr(commAmount,'plotFun') <- 'xyplot'
  attr(commAmount,'layout') <- ''
  return(commAmount)
}

##' summary of the simulation defined by gadget.options
##' @title Summary of gadget.options
##' @name summary.gadget.options
##' @param opt gadget.options list
summary.gadget.options <- function(opt){
  summary.text <- paste('Summary of gadget options:',
                        '',
                        sprintf('There are %s years split into %s steps',
                                opt$numobs,opt$numoftimesteps),
                        sprintf('Area(s):\t%s',paste(1:opt$numofareas,
                                                     collapse=' ')),
                        sprintf('Areasize:\t%s',opt$areasize),
                        '',
                        'Stocks',
                        '',
                        '- Immature stock',
                        sprintf('- Ages between %s and %s',
                                opt$immminage, opt$immmaxage),
                        sprintf('- Lengths between %s and %s',
                                opt$minlen,opt$maxlen),
                        sprintf('- Length-weight relationship %s*l^%s',opt$a,opt$b),
                        sprintf('- Von Bertanlanffy growth parameters: lsup = %s and k = %s',
                                opt$lsup, opt$k),
                        sprintf('- Beta for the beta-binomial length update: %s',
                                opt$beta),
                        sprintf('- Maximum length update %s',opt$binn),
                        '',
                        '- Mature stock',
                        sprintf('- Ages between %s and %s',
                                opt$matminage, opt$matmaxage),
                        sprintf('- Lengths between %s and %s',
                                opt$minlen,opt$maxlen),
                        sprintf('- Length-weight relationship %s*l^%s',opt$a,opt$b),
                        sprintf('- Von Bertanlanffy growth parameters: lsup = %s and k = %s',
                                opt$lsup, opt$k),
                        sprintf('- Beta for the beta-binomial length update: %s',
                                opt$beta),
                        sprintf('- Maximum length update %s',opt$binn),
                        '',
                        'Fleets:',
                        '',
                        '- Survey fleet:',
                        sprintf('- suitability parameters\talpha:%s\tbeta%s',
                                opt$salphasurv,opt$sbetasurv),
                        sprintf('- Effort:\t%s',opt$Fysurv),
                        sprintf('- Timestep(s):\t%s',
                                paste(opt$survstep,collapse=' ')),
                        sprintf('- Harvests in area(s) %s',
                                paste(opt$doescatchsurv,collapse=' ')),
                        '',
                        '- Commercial fleet:',
                        sprintf('- suitability parameters\talpha:%s\tbeta%s',
                                opt$salphacomm,opt$sbetacomm),
                        sprintf('- Effort:\t%s',opt$Fycomm),
                        sprintf('- Timestep(s):\t%s',
                                paste(opt$survstep,collapse=' ')),
                        sprintf('- Harvests in area(s) %s',
                                paste(opt$doescatchcomm,collapse=' ')),
                        '\n',
                        sep='\n')
  cat(summary.text)
  invisible(summary.text)
}
                               
##' This function formats the output from RGadget to a dataframe and adds some 
##' trivial calculated values
##' @name as.data.frame.gadget.sim
##' @title as.data.frame.gadget.sim
##' @param sim the results from RGadget
##' @importFrom rlang .data
##' @return A dataframe 
as.data.frame.gadget.sim <- function(sim){
  weights <- plyr::ldply(sim$gm@stocks,function(x) {
    l <- getLengthGroups(x)
    data.frame(length=l,mean.weight=getWeight(x,l,sim$params))
  })
  stocks <- dplyr::mutate(plyr::ldply(sim$stkArr,as.data.frame.table,responseName = "number", stringsAsFactors = FALSE),
                          length = as.numeric(.data$length),
                          age = as.numeric(.data$age),
                          year = gsub('_Step_[0-9]','',gsub('Year_','',.data$time)),
                          step = gsub('Year_[0-9]+_Step_','',.data$time))
  stocks <- dplyr::arrange(merge(stocks,weights,all.x=TRUE),.data$time,.data$age,.data$length)
  
  catches <- dplyr::mutate(plyr::ldply(sim$fleetArr,as.data.frame.table,responseName = "number", stringsAsFactors = FALSE),
                           length = as.numeric(.data$length),
                           age = as.numeric(.data$age),                  
                           year = gsub('_Step_[0-9]','',gsub('Year_','',.data$time)),
                           step = gsub('Year_[0-9]+_Step_','',.data$time))
  catches <- dplyr::arrange(merge(catches,weights,all.x=TRUE),
                            .data$time,.data$age,.data$length)
  
  
  imm <- as.data.frame.table(sim$immNumRec,stringsAsFactors=FALSE)
  names(imm)[length(names(imm))] <- 'Num.indiv'
  catch.C.imm <- as.data.frame.table(sim$immCcomm,stringsAsFactors=FALSE)
  names(catch.C.imm)[length(names(catch.C.imm))] <- 'Commercial.catch'
  catch.S.imm <- as.data.frame.table(sim$immCsurv,stringsAsFactors=FALSE)
  names(catch.S.imm)[length(names(catch.S.imm))] <- 'Survey.catch'
  tmp.imm <- merge(imm,catch.C.imm,all=TRUE)
  tmp.imm <- merge(tmp.imm,catch.S.imm,all=TRUE)
  tmp.imm$year <- sapply(strsplit(tmp.imm$time,'_'),
                         function(x) as.numeric(x[2]))
  tmp.imm$step <- sapply(strsplit(tmp.imm$time,'_'),
                         function(x) as.numeric(x[4]))
  tmp.imm$length <- as.numeric(tmp.imm$length)
  tmp.imm$age <- as.numeric(tmp.imm$age)
  tmp.imm$weight <- sim$opt$a*tmp.imm$length^sim$opt$b
  tmp.imm$stock <- 'imm'
  
  mat <- as.data.frame.table(sim$matNumRec,stringsAsFactors=FALSE)
  names(mat)[length(names(mat))] <- 'Num.indiv'
  catch.C.mat <- as.data.frame.table(sim$matCcomm,stringsAsFactors=FALSE)
  names(catch.C.mat)[length(names(catch.C.mat))] <- 'Commercial.catch'
  catch.S.mat <- as.data.frame.table(sim$matCsurv,stringsAsFactors=FALSE)
  names(catch.S.mat)[length(names(catch.S.mat))] <- 'Survey.catch'
  tmp.mat <- merge(mat,catch.C.mat,all=TRUE)
  tmp.mat <- merge(tmp.mat,catch.S.mat,all=TRUE)
  tmp.mat$year <- sapply(strsplit(tmp.mat$time,'_'),
                         function(x) as.numeric(x[2]))
  tmp.mat$step <- sapply(strsplit(tmp.mat$time,'_'),
                         function(x) as.numeric(x[4]))
  tmp.mat$length <- as.numeric(tmp.mat$length)
  tmp.mat$age <- as.numeric(tmp.mat$age)
  tmp.mat$weight <- sim$opt$a*tmp.mat$length^sim$opt$b
  tmp.mat$stock <- 'mat'

  tmp <- rbind(tmp.imm,tmp.mat)
  tmp$time <- NULL
  return(tmp)
}

##' Calculate the Von Bertanlaffy curve according to the formula
##' \deqn{L(a) = L_\infty (1-e^{-\kappa a})}{L(a) = L_infty (1-e^{-kappa a})}
##' @title Von Bertalanffy 
##' @name vonB
##' @param lsup \eqn{L_\infty }{L_infty} terminal length
##' @param k \eqn{\kappa}{kappa}, the growth parameter
##' @param a age of the individual
##' @return a vector of length(a) with the calculated VB curve at age(s) a.
##' @author Bjarki Þór Elvarsson
vonB <- function(lsup,k,a){
  lsup <- lsup*(1-exp(-k*a))
}

##' Simulated length at age
##' @title Length at age
##' @name length.at.age
##' @param sim The results from a Rgadget simulation
##' @return a dataframe containin the length at age for both fleets
##' @author Bjarki Þór Elvarsson
length.at.age <- function(sim){
  sim.dat <- as.data.frame(sim)
  cols <- c('age','year','step','length.at.age','fleet')
  comm.lat <- stats::aggregate(cbind(Commercial.catch,Commercial.catch*length)~
                        age+year+step,sim.dat[sim.dat$step %in% sim$opt$commstep,],sum)
  comm.lat$length.at.age <-  comm.lat[,5]/comm.lat[,4]
  comm.lat$fleet <- 'comm'
  comm.lat <- comm.lat[cols]
  
  surv.lat <- stats::aggregate(cbind(Survey.catch,Survey.catch*length)~
                        age+year+step,sim.dat[sim.dat$step %in% sim$opt$survstep,],sum)
  surv.lat$length.at.age <-  surv.lat[,5]/surv.lat[,4]
  surv.lat$fleet <- 'surv'
  surv.lat <- surv.lat[cols]
  lat <- rbind(surv.lat,comm.lat)
  class(lat) <- c('Rgadget',class(lat))
  attr(lat,'formula') <- length.at.age~age|year+step
  attr(lat,'plotGroups') <- 'fleet'
  attr(lat,'plotType') <- 'l'
  attr(lat,'xaxis') <- 'Age'
  attr(lat,'yaxis') <- 'Length'
  attr(lat,'plotFun') <- 'xyplot'
  

  return(lat)
  
}

tagging.recaptures <- function(sim,lambda,N){
  ## recaptures in WI
  U <- apply(sim$Tagged.C,7,sum)
#  p <- lambda/(1+lambda)
  
  rec <- plyr::adply(U[-1],1,function(x) stats::rnbinom(N,mu=x,size=x/lambda))

  if(!is.null(sim$opt$dispersion)){
    Cstock <- c('C1','C2','C3')
    ci <- sim$opt$quota*10
    Ri <- stats::rpois(N,ci*(ci-1)/(2*sum(sim$opt$init.abund[Cstock])))
    rho <- apply(rec[,-1],2,sum)/Ri
  } else {
    ci <-t(stats::rmultinom(N,
                     size=sum(sim$Catches),
                     prob=apply(sim$Catches,2,sum)
                     ))
    names(ci) <- dimnames(sim$Abundance)$stocks
    Ri <- ci*(ci-1)/(2*sim$opt$init.abund)
    rho <- apply(rec[,-1],2,sum)/apply(Ri,1,sum)
  }
  

  rec <- reshape2::melt(rec,id='X1')  
  return(list(rec=rec,rho=rho,ci=ci,Ri=Ri))
}
