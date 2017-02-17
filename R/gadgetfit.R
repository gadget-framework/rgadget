##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Gadget fit
##' @param wgts
##' @param main.file
##' @param fleet.predict
##' @param mat.par
##' @param params.file
##' @param f.age.range
##' @return list of fit things
##' @author Bjarki Thor Elvarsson
##' @export
gadget.fit <- function(wgts = 'WGTS', main.file = 'main',
                       fleet.predict = data.frame(fleet='comm',ratio=1),
                       mat.par=NULL, params.file=NULL,
                       f.age.range=NULL, fit.folder = 'FIT',
                       compile.fleet.info = TRUE){
  
  main <- read.gadget.main(file = main.file)
  
  if(!is.null(wgts)){
    resTable <- read.gadget.results(wgts=wgts)
    nesTable <- read.gadget.results(wgts=wgts,normalize = TRUE)
    params <- read.gadget.parameters(sprintf('%s/params.final',wgts))
    lik <- read.gadget.likelihood(sprintf('%s/likelihood.final',wgts))
  } else {
    resTable <- list()
    nesTable <- list()
    wgts <- fit.folder
    dir.create(fit.folder,showWarnings=FALSE)
    params <- read.gadget.parameters(params.file)
    lik <- read.gadget.likelihood(main$likelihoodfiles)
  }
  
  lik.dat <- read.gadget.data(lik)
  
  ## model output, i.e printfiles
  make.gadget.printfile(main = main.file,
                        file = sprintf('%s/printfile.fit',wgts),
                        out = sprintf('%s/out.fit',wgts),
                        aggfiles = sprintf('%s/print.aggfiles',wgts))
  
  main$printfiles <- sprintf('%s/printfile.fit',wgts)
  write.gadget.main(main,file = sprintf('%s/main.print',wgts))
  
  callGadget(s=1,
             i = ifelse(is.null(params.file),
                        sprintf('%s/params.final',wgts),
                        params.file),
             main = sprintf('%s/main.print',wgts),
             o = sprintf('%s/SS.print',wgts))
  
  out <- read.printfiles(sprintf('%s/out.fit',wgts))
  SS <- read.gadget.lik.out(sprintf('%s/SS.print',wgts))
  stocks <- read.gadget.stockfiles(main$stockfiles)
  fleets <- read.gadget.fleet(main$fleetfiles)
  catches <- get.gadget.catches(fleets,params)
  
  gss.suit <- 
    plyr::ldply(stocks,
                function(x){
                  tryCatch(subset(get.gadget.suitability(fleets,params,
                                                         getLengthGroups(x)),
                                  stock == x@stockname),
                           error = function(y){
                             print('warning -- suitability parameters could not be read')
                             0
                           })
                })
  
  stock.growth <-
    tryCatch(get.gadget.growth(stocks,params,age.based=TRUE),
             warning = function(x) NULL,
             error = function(x) NULL)
  stock.recruitment <- get.gadget.recruitment(stocks,params)
  
  harv.suit <- function(l,stockname){
    tryCatch(ddply(merge(
      subset(get.gadget.suitability(fleets,params,l),
             stock==stockname),
      fleet.predict),~l,
      summarise, harv=sum(ratio*suit))$harv,
      error = function(x){
        print('warning -- fleet parameters could not be read')
        return(0)
      })
  }
  
  stock.full <-
    ldply(stocks,function(x){
      mutate(out[[sprintf('%s.full',getStockNames(x))]],
             length=as.numeric(gsub('len','',length)))
    })
  
  stock.std <- ldply(stocks,function(x){
    out[[sprintf('%s.std',getStockNames(x))]]
  })
  
  
  if(compile.fleet.info){
    ## this if statement is here due to incompatibility with timevariables
    fleet.catches <- 
      ddply(fleets$fleet,~fleet,function(x){
        tmp <- 
          read.table(file=x$amount,comment.char = ';')
        names(tmp) <- 
          c('year','step','area','fleet','amount')
        tmp$amount <- as.numeric(tmp$amount)
        filter(tmp,fleet == x$fleet)
      }) 
    
    fleet.info <- 
      stock.full %>%
      dplyr::mutate(area = as.numeric(gsub('area','',area))) %>%
      dplyr::left_join(select(gss.suit,.id,length=l,suit,fleet,stock)) %>%
      dplyr::group_by(year,step,area,fleet) %>%
      dplyr::summarise(harv.bio = sum(suit*number*mean.weight)) %>%
      dplyr::left_join(fleet.catches %>% 
                         dplyr::group_by(year,fleet,area) %>% 
                         dplyr::summarise(amount=sum(amount))) %>%
      dplyr::group_by(year,step,area,fleet) %>%
      dplyr::mutate(amount = ifelse(is.na(amount),0,amount),
                    harv.rate = amount/harv.bio)
  } else {
    fleet.info <- data.frame()
  }
  
  ## merge data and estimates
  if('surveyindices' %in% names(lik.dat$dat)){
    
    
    sidat <- plyr::ldply(names(lik.dat$dat$surveyindices),
                         function(x){
                           sidat <-
                             merge(lik.dat$dat$surveyindices[[x]],
                                   out[[x]],
                                   by.y=c('year','label','step','area'),                             
                                   by.x=intersect(c('year','length','age','survey',
                                                    'step','area'),
                                                  names(lik.dat$dat$surveyindices[[x]])),
                                   all.y=TRUE)
                           if('lengths' %in% sidat$sitype){
                             sidat$length <- paste(sidat$lower,
                                                   sidat$upper, sep = ' - ')
                           }
                           sidat$name <- x
                           sidat <- merge(sidat,
                                          subset(lik$surveyindices,
                                                 select=c(name,stocknames)),
                                          by='name')
                           si.stocks <-
                             unique(unlist(strsplit(unique(sidat$stocknames),'\t')))
                           if('lengths' %in% sidat$sitype){
                             ## note this assumes length based survey indices atm
                             si.labels <-
                               arrange(unique(sidat[c('length','lower','upper')]),
                                       lower)
                             sibio <-
                               stock.full %>%
                               filter(.id %in% si.stocks) %>%
                               mutate(sigroup = cut(length,
                                                    breaks=c(si.labels$lower,
                                                             max(si.labels$upper)),
                                                    labels=si.labels$length))%>%
                               group_by(year,sigroup) %>%
                               summarise(bio=sum(number*mean.weight)/sum(number))
                             sidat <- merge(sidat,sibio,by.x=c('year','length'),
                                            by.y=c('year','sigroup'),all.x=TRUE)
                           }
                           return(sidat)
                         })
    
  } else {
    sidat <- NULL
  }
  
  
  
  if('catchdistribution' %in% names(lik.dat$dat)){
    catchdist.fleets <-
      ldply(names(lik.dat$dat$catchdistribution),
            function(x){
              
              ldist <-
                merge(lik.dat$dat$catchdistribution[[x]],
                      join(out[[x]],
                           attr(lik.dat$dat$catchdistribution[[x]],'len.agg'),
                           by='length'),
                      by=c('length', 'year',
                           'step', 'area','age','upper','lower'),
                      all.y=TRUE)
              ldist$name <- x
              ldist$age <- as.character(ldist$age)
              ldist$area <- as.character(ldist$area)
              ldist$upper <- as.double(ldist$upper)
              ldist$lower <- as.double(ldist$lower)
              
              ldist <-
                ldist %>%
                group_by(year, step,  area, add=FALSE) %>%
                mutate(total.catch = sum(number.x,na.rm=TRUE),
                       total.pred = sum(number.y,na.rm=TRUE),
                       observed = number.x/sum(number.x,na.rm=TRUE),
                       predicted = number.y/sum(number.y,na.rm=TRUE)) %>%
                group_by(length,age,add=FALSE) %>%
                mutate(upper = as.double(max(ifelse(is.na(upper),0.0,
                                                    upper))),
                       lower = as.double(max(ifelse(is.na(lower),0.0,
                                                    lower))),
                       avg.length = as.numeric((lower+upper)/2),
                       residuals = as.numeric(observed - predicted))
              ldist <- merge(ldist,
                             subset(lik$catchdistribution,
                                    select=c(name,fleetnames,stocknames)),
                             by = 'name')
              return(ldist)
            })
  } else {
    catchdist.fleets <- NULL
  }
  
  
  if(sum(grepl('.std',names(out),fixed = TRUE))>0){
    
    res.by.year <-
      llply(laply(stocks,function(x) x@stockname),function(x){
        if(is.null(f.age.range)){
          f.age.range <- c(max(out[[sprintf('%s.prey',x)]]$age),
                           max(out[[sprintf('%s.prey',x)]]$age))
        }
        f.by.year <- 
          out[[sprintf('%s.prey',x)]] %>%
          dplyr::group_by(year,area) %>%
          dplyr::summarise(catch=sum(biomass.consumed),
                           num.catch=sum(number.consumed),
                           F=mean(mortality[age>=min(f.age.range)&age<=max(f.age.range)]))
        
        
        bio.by.year <- 
          out[[sprintf('%s.full',x)]] %>%
          dplyr::filter(step == 1) %>%
          dplyr::group_by(year,area) %>%
          dplyr::summarise(total.number = sum(number),
                           total.biomass = sum(number*mean.weight),
                           harv.biomass =
                             sum(mean.weight*
                                   harv.suit(as.numeric(gsub('len','',length)),x)*
                                   number),
                           ssb = sum(mean.weight*logit(mat.par[1],
                                                       mat.par[2],
                                                       as.numeric(gsub('len','',length)))*
                                       number)) %>% 
          dplyr::left_join(f.by.year) %>% 
          dplyr::mutate(stock = x)
        
        return(bio.by.year)
      }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::left_join(stock.recruitment %>% 
                         dplyr::mutate(area = paste0('area',area),
                                       year = as.numeric(year)))
  } else {
    res.by.year <- NULL
  }
  
  #  if('catchinkilos' %in% names(lik.dat$dat)){
  #    catchdist <-
  #      ldply(names(lik.dat$dat$catchinkilos),
  #            function(x){
  #
  #            }
  #  }
  
  if('stockdistribution' %in% names(lik.dat$dat)){
    stockdist <-
      ldply(names(lik.dat$dat$stockdistribution),
            function(x){
              stockdist <-
                merge(lik.dat$dat$stockdistribution[[x]],
                      join(out[[x]],
                           attr(lik.dat$dat$stockdistribution[[x]],'len.agg'),
                           by='length'),
                      by=c('length', 'year',
                           'step', 'area','age',
                           'stock','upper','lower'),
                      all.y=TRUE)
              
              stockdist$name <- x
              stockdist <-
                stockdist %>% 
                dplyr::group_by(year, step, area, age, length) %>% 
                dplyr::mutate(obs.ratio = number.x/sum(number.x,na.rm=TRUE),
                              pred.ratio = number.y/sum(number.y),
                              length2 =  (lower+upper)/2) %>% 
                dplyr::inner_join(lik$stockdistribution %>% 
                                    dplyr::select(name,fleetnames,stocknames),
                                  by='name')
              return(stockdist)
            })
    
    
  } else {
    stockdist <- NULL
  }
  
  
  if('stomachcontent' %in% names(lik.dat$dat)){
    stomachcontent <-
      ldply(names(lik.dat$dat$stomachcontent),
            function(x){
              
              dat <-
                merge(lik.dat$dat$stomachcontent[[x]],
                      join(join(out[[x]],
                                attr(lik.dat$dat$stomachcontent[[x]],
                                     'prey.agg'),
                                by='prey'),
                           attr(lik.dat$dat$stomachcontent[[x]],'pred.agg'),
                           by='predator'),
                      all.y=TRUE) %>%
                group_by(year,step,predator) %>%
                mutate(observed=ratio/sum(ratio,na.rm=TRUE),
                       predicted=number/sum(number,na.rm=TRUE),
                       prey.length = (prey.lower+prey.upper)/2,
                       pred.length = (lower+upper)/2,
                       component=x)
            })
    
  } else {
    stomachcontent <- NULL
  }
  
  out <- list(sidat = sidat, resTable = resTable, nesTable = nesTable,
              suitability = gss.suit, stock.growth = stock.growth,
              stock.recruitment = stock.recruitment,
              res.by.year = res.by.year, stomachcontent = stomachcontent,
              likelihoodsummary = out$likelihoodsummary,
              catchdist.fleets = catchdist.fleets, stockdist = stockdist,
              out.fit=out, SS = SS,
              stock.full = stock.full, stock.std = stock.std,
              fleet.info = fleet.info)
  class(out) <- c('gadget.fit',class(out))
  save(out,file=sprintf('%s/WGTS.Rdata',wgts))
  return(out)
}


#' Merge gadget.fit objects
#'
#' This function merges gadget.fit objects from different gadget runs. This allow for simpler comparisons between models.
#' @param ... an arbitrary number of gadget.fit objects as named input. 
#'
#' @return a merged gadget.fit object
#' @export
bind.gadget.fit <- function(...){
  tmp <- 
    list(...) %>% 
    purrr::transpose() %>% 
    purrr::map(~purrr::map_if(.,is.null,data.frame)) %>% 
    purrr::map(~purrr::map_if(.,is.factor,as.character)) %>% 
    purrr::map(purrr::safely(~dplyr::bind_rows(.,.id='model'))) %>% 
    purrr::map('result')
  class(tmp) <- c('gadget.fit',class(tmp))
  return(tmp)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title bootstrap fit
##' @param main
##' @param dparam.file
##' @param bsprint.file
##' @param fleet.predict
##' @param mat.par
##' @param .parallel
##' @return list of bootstrap fit things
##' @author Bjarki Thor Elvarsson
gadget.bootfit <- function(main = 'main', dparam.file = 'bsres_v1.RData',
                           bsprint.file = 'bsprint.RData',
                           fleet.predict = data.frame(fleet='comm',ratio=1),
                           mat.par=NULL, .parallel = TRUE
){
  
  
  load(dparam.file)
  load(bsprint.file)
  main <- read.gadget.main(main)
  lik <- read.gadget.likelihood(main$likelihoodfiles)
  lik.dat <- read.gadget.data(lik)
  
  fleets <- read.gadget.fleet(main$fleetfiles)
  stocks <- read.gadget.stockfiles(main$stockfiles)
  dfinal <- subset(dparam,comp=='final')
  dfinal$comp <- NULL
  boot.rec <-
    ddply(melt(dfinal,id.vars='bs.data',variable.name = 'switch'),
          ~bs.data,function(x){
            rownames(x) <- x$switch
            get.gadget.recruitment(stocks,x)
          })
  boot.sel <-
    ddply(melt(dfinal,id.vars='bs.data',variable.name = 'switch'),
          ~bs.data,function(x){
            rownames(x) <- x$switch
            get.gadget.suitability(fleets,x,getLengthGroups(stocks[[1]]))
          })
  boot.growth <-
    ddply(melt(dfinal,id.vars='bs.data',variable.name = 'switch'),
          ~bs.data,function(x){
            rownames(x) <- x$switch
            get.gadget.growth(stocks,x,age.based = TRUE)
          })
  
  #  boot.ldistfit <-
  #    rbindlist(llply(names(lik.dat$dat$catchdistribution),
  #                    function(x){
  #                      si <-
  #                        data.table(noageprint[[x]]) %.%
  #                        group_by(.id,year,step,area) %.%
  #                        mutate(proportion = number/sum(number)) %.%
  #                        group_by(year,step,age,length,add=FALSE) %.%
  #                        summarise(upper = quantile(proportion,0.975,na.rm=TRUE),
  #                                  lower = quantile(proportion,0.025,na.rm=TRUE))
  #                      si$fleet <- x
  #                      return(si)
  #                    }))
  
  harv.suit <- function(l, .id){
    x <- subset(melt(dfinal,id.vars='bs.data',variable.name = 'switch'),
                bs.data == .id)
    rownames(x) <- x$switch
    ddply(merge(get.gadget.suitability(fleets,x,l),fleet.predict),~l,
          summarise, harv=sum(ratio*suit))$harv
  }
  
  res.by.year <-
    ldply(laply(stocks,function(x) x@stockname),function(x){
      f.by.year <- ddply(bsprint[[sprintf('%s.prey',x)]],
                         ~year + .id,
                         summarise,
                         F=max(Z-0.15,na.rm=TRUE)) ### ATH!!!!!!!!
      ## making sure this works for a relic from the good old times:)
      txt <- ifelse(sum(grepl('.full',names(bsprint),fixed=TRUE))==1,
                    sprintf('%s.full',x), sprintf('%s.lw',x))
      
      bio.by.year <- ddply(subset(bsprint[[txt]],
                                  step == 1),
                           ~year + area + .id,
                           plyr::here(summarise),
                           total.biomass = sum(number*mean.weight),
                           harv.biomass =
                             sum(mean.weight*
                                   harv.suit(as.numeric(gsub('len','',length)),
                                             .id[1])*
                                   number),
                           ssb = sum(mean.weight*logit(mat.par[1],
                                                       mat.par[2],as.numeric(gsub('len','',length)))*
                                       number),
                           .parallel = .parallel)
      
      bio <- merge(f.by.year,bio.by.year)
      bio$stock <- x
      return(bio)
    })
  res.by.year <- merge(res.by.year,boot.rec,all.x = TRUE)
  boot.fit <- list(bootparams = dfinal,res.by.year = res.by.year,
                   boot.rec = boot.rec, boot.sel = boot.sel,
                   boot.growth = boot.growth)
  save(boot.fit,file='digestedBoot.RData')
  invisible(boot.fit)
}