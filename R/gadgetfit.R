##' This function calls Gadget and compiles fit statistics, relevant biomass trajectories and consumption
##' @title Gadget fit
##'
##' @param wgts Location of the iterative reweighting folder, if null gadget.fit requires a parameter file
##' @param main.file if the main file is different from the 'main' or 'wgts/main.final'
##' @param fleet.predict the basis fleets used to calculate the harvestable biomass
##' @param mat.par parameters for the maturity ogive
##' @param params.file parameter file used in the fit (defaults to "WGTS/params.final")
##' @param fit.folder location of the output
##' @param compile.fleet.info should the harvest rate be calculated 
##' @param printfile.printatstart should the stock standared output be printed at the beginning or the end of the timestep
##' @param printfile.steps what steps should be printed
##' @param f.age.range data.frame describing the desired age range where the F's are calculated, if null this defaults to the apical F fro all stocks. 
##' Input columns should include stock, age.min and age.max 
##' @param rec.len.param Logical. TRUE if you want growth calculated as age.based and using a formula other than get.gadget.growth default
##'
##' @return list of fit things
##' @author Bjarki Thor Elvarsson
##' @export
gadget.fit <- function(wgts = 'WGTS', main.file = NULL,
                       fleet.predict = data.frame(fleet='comm',ratio=1),
                       mat.par=NULL, params.file=NULL,
                       f.age.range=NULL, fit.folder = 'FIT',
                       compile.fleet.info = TRUE,
                       printfile.printatstart = 1, printfile.steps = 1,
                       rec.len.param = FALSE){
  
  if(!is.null(f.age.range) & class(f.age.range) != 'data.frame'){
    stop('F age range should be specified as a data.frame with columns stock, age.min and age.max')
  }
  
  if(is.null(main.file)) {
    if(is.null(wgts)){
      main.file <- 'main'
    } else {
      main.file <- sprintf('%s/main.final',wgts)
    }
  }
  
  main <- read.gadget.main(file=main.file)
  
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
                        aggfiles = sprintf('%s/print.aggfiles',wgts),
                        printatstart = printfile.printatstart,
                        steps = printfile.steps)
  
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
    tryCatch(get.gadget.growth(stocks,params,age.based=TRUE,
                               recl = rec.len.param),
             warning = function(x) NULL,
             error = function(x) NULL)
  stock.recruitment <- get.gadget.recruitment(stocks,params)
  
  harv.suit <- function(l,stockname){
    tryCatch(plyr::ddply(merge(
      subset(get.gadget.suitability(fleets,params,l),
             stock==stockname),
      fleet.predict),~l,
      plyr::summarise, harv=sum(ratio*suit))$harv,
      error = function(x){
        print('warning -- fleet parameters could not be read')
        return(0)
      })
  }
  
  stock.full <-
    out[sprintf('%s.full',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::mutate(length=as.numeric(gsub('len','',length))) %>% 
    dplyr::as_data_frame()
  
  stock.std <- 
    out[sprintf('%s.std',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::as_data_frame()
  
  stock.prey <- 
    out[sprintf('%s.prey',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::as_data_frame()
  
  predator.prey <- 
    out[grepl('.+\\.prey\\..+',names(out))] %>% 
    purrr::set_names(.,names(.)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    tidyr::separate(stock,c('prey','predator'),sep='\\.prey\\.') %>% 
    dplyr::as_data_frame()
  
  
  if(compile.fleet.info){
    ## this if statement is here due to incompatibility with timevariables
    fleet.catches <- 
      plyr::ddply(fleets$fleet,~fleet,function(x){
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
    sidat <- 
      out[names(lik.dat$dat$surveyindices)] %>% 
      purrr::set_names(.,names(.)) %>%
      dplyr::bind_rows(.id='name') %>% 
      dplyr::left_join(lik$surveyindices %>% 
                         dplyr::select(name,stocknames,sitype,fittype), 
                       by='name') %>% 
      dplyr::bind_rows(dplyr::data_frame(length=character(0),
                                         age=character(0),
                                         survey = character(0),
                                         fleet = character(0))) %>% 
      dplyr::mutate(age = ifelse(sitype == 'ages',label,age),
                    length = ifelse(sitype %in% c('lengths','fleets'),label,length),
                    fleet = ifelse(sitype == 'effort',label,fleet),
                    survey = ifelse(sitype == 'accoustic',label,survey)) %>% 
      dplyr::left_join(lik.dat$dat$surveyindices %>% 
                         purrr::set_names(.,names(.)) %>% 
                         dplyr::bind_rows(.id='name') %>% 
                         dplyr::rename(observed=number)) %>% 
      dplyr::mutate(length = ifelse(sitype %in% c('lengths','fleets'),
                                    paste(lower,upper,sep=' - '),
                                    length)) %>% 
      dplyr::as_data_frame()
  } else {
    sidat <- NULL
  }
  
  
  
  if('catchdistribution' %in% names(lik.dat$dat)){
    dat.names <- names(lik.dat$dat$catchdistribution)
    
    aggs <- 
      dat.names %>% 
      purrr::set_names(.,.) %>% 
      purrr::map(~attr(lik.dat$dat$catchdistribution[[.]],'len.agg')) %>% 
      dplyr::bind_rows(.id='name') %>% 
      dplyr::as_data_frame()
    
    catchdist.fleets <-
      lik.dat$dat$catchdistribution %>% 
      purrr::set_names(.,names(.)) %>%
      purrr::map(. %>% dplyr::mutate(age = as.character(age))) %>%
      dplyr::bind_rows(.id='name') %>%  
      dplyr::right_join(out[dat.names] %>%
                          purrr::set_names(.,dat.names) %>%
                          purrr::map(. %>% dplyr::mutate(age = as.character(age))) %>%
                          dplyr::bind_rows(.id='name') %>% 
                          dplyr::left_join(aggs,by=c('name','length')) ,
                        by=c('name','length', 'year',
                             'step', 'area','age','upper','lower')) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(name,year, step,  area) %>%
      dplyr::mutate(total.catch = sum(number.x,na.rm=TRUE),
                    total.pred = sum(number.y,na.rm=TRUE),
                    observed = number.x/sum(number.x,na.rm=TRUE),
                    predicted = number.y/sum(number.y,na.rm=TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(name,length,age) %>%
      dplyr::mutate(upper = as.double(max(ifelse(is.na(upper),0.0,
                                                 upper))),
                    lower = as.double(max(ifelse(is.na(lower),0.0,
                                                 lower))),
                    avg.length = as.numeric((lower+upper)/2),
                    residuals = as.numeric(observed - predicted)) %>% 
      dplyr::inner_join(lik$catchdistribution %>% 
                          select(name,fleetnames,stocknames),
                        by = 'name')
  } else {
    catchdist.fleets <- NULL
  }
  
  
  if(sum(grepl('.std',names(out),fixed = TRUE))>0){
    
    if(is.null(f.age.range)){
      f.age.range <- 
        stock.prey %>% 
        dplyr::group_by(stock) %>% 
        dplyr::summarise(age.min = max(age),age.max=max(age))
    }
    
    
    f.by.year <- 
      stock.prey %>% 
      dplyr::left_join(f.age.range,by="stock") %>% 
      dplyr::group_by(stock,year,area) %>%
      dplyr::summarise(catch=sum(biomass.consumed),
                       num.catch=sum(number.consumed),
                       F=mean(mortality[age>=age.min&age<=age.max]))
    
    res.by.year <- 
      stock.full %>% 
      dplyr::filter(step == 1) %>%
      dplyr::group_by(stock,year,area) %>%
      dplyr::summarise(total.number = sum(number),
                       total.biomass = sum(number*mean.weight),
                       harv.biomass =
                         sum(mean.weight*
                               harv.suit(length,stock)*
                               number),
                       ssb = sum(mean.weight*logit(mat.par[1],
                                                   mat.par[2],
                                                   length)*
                                   number)) %>% 
      dplyr::left_join(f.by.year %>%
                         mutate(area = area),
                       by = c("stock","year","area")) %>% 
      dplyr::left_join(stock.recruitment %>% 
                         dplyr::mutate(stock = as.character(stock),
                                       area = paste0('area',area),
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
    dat.names <- names(lik.dat$dat$stockdistribution) 
    
    aggs <- 
      dat.names %>% 
      purrr::set_names(.,.) %>% 
      purrr::map(~attr(lik.dat$dat$stockdistribution[[.]],'len.agg')) %>% 
      dplyr::bind_rows(.id='name') %>% 
      dplyr::as_data_frame()
    
    stockdist <-
      lik.dat$dat$stockdistribution %>% 
      purrr::set_names(.,names(.)) %>%
      dplyr::bind_rows(.id='name') %>% 
      dplyr::right_join(out[dat.names] %>%
                          purrr::set_names(.,dat.names) %>% 
                          dplyr::bind_rows(.id='name') %>% 
                          left_join(aggs,by=c('name','length')),
                        by=c('name','length', 'year',
                             'step', 'area','age',
                             'stock','upper','lower'),
                        suffix = c('.y','.x')) %>% 
      dplyr::group_by(name,year, step, area, age, length) %>% 
      dplyr::mutate(obs.ratio = number.x/sum(number.x,na.rm=TRUE),
                    pred.ratio = number.y/sum(number.y)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(length = (lower+upper)/2) %>% 
      dplyr::inner_join(lik$stockdistribution %>% 
                          dplyr::select(name,fleetnames,stocknames) %>% 
                          dplyr::distinct(),
                        by='name')
    
  } else {
    stockdist <- NULL
  }
  
  
  if('stomachcontent' %in% names(lik.dat$dat)){
    stomachcontent <-
      plyr::ldply(names(lik.dat$dat$stomachcontent),
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
                      dplyr::group_by(year,step,predator) %>%
                      dplyr::mutate(observed=ratio/sum(ratio,na.rm=TRUE),
                                    predicted=number/sum(number,na.rm=TRUE),
                                    prey.length = (prey.lower+prey.upper)/2,
                                    pred.length = (lower+upper)/2,
                                    component=x)
                  })
    
  } else {
    stomachcontent <- NULL
  }
  
  out <- 
    list(sidat = sidat, resTable = resTable, nesTable = nesTable,
         suitability = gss.suit, 
         stock.growth = stock.growth,
         stock.recruitment = stock.recruitment,
         res.by.year = res.by.year, stomachcontent = stomachcontent,
         likelihoodsummary = out$likelihoodsummary,
         catchdist.fleets = catchdist.fleets, 
         stockdist = stockdist,
         #out.fit=out, 
         SS = SS,
         stock.full = stock.full, 
         stock.std = stock.std,
         stock.prey = stock.prey,
         fleet.info = fleet.info,
         predator.prey = predator.prey,
         params = params)
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
