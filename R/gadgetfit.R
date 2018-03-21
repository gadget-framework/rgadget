##' \code{gadget.fit} calls Gadget and compiles fit statistics, relevant biomass trajectories and consumption.
##' It returns an object of class 'gadget.fit' which is essentially a list containing the following objects: \describe{
##' \item{sidat}{data.frame containing the model fit to the surveyindices}
##' \item{resTable}{Table of likelihood component scores from the different stages of the iterative reweighting run}
##' \item{nesTable}{Same as resTable but normalised with the minimum score for each component}
##' \item{suitability}{The model estimated fleet predator suitability}
##' \item{stock.growth}{Model estimated growth}
##' \item{stock.recruitment}{Recruitment by stock}
##' \item{res.by.year}{Results by year}
##' \item{stomachconten}{Fit combined with data from the stomachcontent likelihood}
##' \item{likelihoodsummary}{Likelihood component scores by year and step} 
##' \item{catchdist.fleets}{Data compared with model output for the catchdistribution components}
##' \item{stockdist}{Data compared with model output for the stockdistribution components}
##' \item{SS}{The likelihood output from the model}
##' \item{stock.full}{Modeled abundance and mean weight by year,step, length and stock}
##' \item{stock.std}{Modeled abundance, mean weight, number consumed by age, stock and year}
##' \item{stock.prey}{Consumption of prey (or fleet catches) by prey, length, year and step}
##' \item{fleet.info}{Information on catches, harvest rate and harvestable biomass by fleet, year and step}
##' \item{predator.prey}{consumption of preys by predators, length, year and step}
##' \item{params}{parameter values used for the fit}
##' \item{catchstatistics}{Data compared with model output for the catchdstatistics components}
##' }
##' @title Gadget fit
##' @rdname gadget.fit
##' @param wgts Location of the iterative reweighting folder, if null gadget.fit requires a parameter file
##' @param main.file if the main file is different from the 'main' or 'wgts/main.final'
##' @param fleet.predict data.frame containing the basis fleets used to calculate the harvestable biomass. Fleet names should be specified in the fleet column.
##' @param mat.par parameters for the maturity ogive
##' @param params.file parameter file used in the fit (defaults to "WGTS/params.final")
##' @param fit.folder location of the output
##' @param printfile.printatstart should the stock standared output be printed at the beginning or the end of the timestep
##' @param printfile.steps what steps should be printed
##' @param f.age.range data.frame describing the desired age range where the F's are calculated, if null this defaults to the apical F fro all stocks. 
##' Input columns should include stock, age.min and age.max 
##' @param rec.len.param Logical. TRUE if you want growth calculated as age.based and using a formula other than get.gadget.growth default
##' @return list containing the output from Gadget. 
##' @author Bjarki Thor Elvarsson
##' @export
gadget.fit <- function(wgts = 'WGTS', 
                       main.file = NULL,
                       fleet.predict = NULL,
                       mat.par=NULL, 
                       params.file=NULL,
                       f.age.range=NULL, 
                       fit.folder = 'FIT',
                       printfile.printatstart = 1, 
                       printfile.steps = 1,
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
  
  main <- read.gadget.main(main.file)
  
  if(!is.null(wgts)){
    resTable <- read.gadget.results(wgts=wgts)
    nesTable <- read.gadget.results(wgts=wgts,normalize = TRUE)
    if(is.null(params.file)){
      params.file <- sprintf('%s/params.final',wgts)
    }
    params <- read.gadget.parameters(params.file)
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
             i = params.file,
             main = sprintf('%s/main.print',wgts),
             o = sprintf('%s/SS.print',wgts))
  
  out <- read.printfiles(sprintf('%s/out.fit',wgts))
  SS <- read.gadget.lik.out(sprintf('%s/SS.print',wgts))
  stocks <- read.gadget.stockfiles(main$stockfiles)

  stock.growth <-
    tryCatch(get.gadget.growth(stocks,params,age.based=TRUE,
                               recl = rec.len.param),
             warning = function(x) NULL,
             error = function(x) NULL)
  stock.recruitment <- get.gadget.recruitment(stocks,params)
  
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
    purrr::keep(~'number.consumed' %in% names(.)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    tidyr::separate(stock,c('prey','predator'),sep='\\.prey\\.') %>% 
    dplyr::as_data_frame() %>% 
    dplyr::group_by(year,step,prey,predator) %>% 
    dplyr::mutate(suit = mortality/max(mortality),
                  suit = ifelse(is.finite(suit),suit,0),
                  length = gsub('len', '', length) %>% 
                    as.numeric())
  
  
  fleet.catches <- 
    predator.prey %>% 
    dplyr::group_by(year,area,predator,prey) %>% 
    dplyr::summarise(amount = sum(biomass.consumed)) %>% 
    dplyr::rename(fleet = predator, stock = prey)
  
  fleet.info <- 
    stock.full %>%
    dplyr::left_join(predator.prey %>% 
                       dplyr::select(year,
                                     step, 
                                     area,
                                     fleet = predator, 
                                     stock = prey, 
                                     length, 
                                     suit)) %>%
    dplyr::group_by(year,step,area,fleet) %>%
    dplyr::summarise(harv.bio = sum(suit*number*mean.weight)) %>%
    dplyr::left_join(fleet.catches %>% 
                       dplyr::group_by(year,fleet,area) %>% 
                       dplyr::summarise(amount=sum(amount))) %>%
    dplyr::group_by(year,step,area,fleet) %>%
    dplyr::mutate(amount = ifelse(is.na(amount),0,amount),
                  harv.rate = amount/harv.bio)
  
  if(!is.null(fleet.predict)){
    d <- 
      predator.prey %>% 
      dplyr::filter(predator %in% fleet.predict$fleet)
  } else {
    d <- predator.prey
  }
  
  harv.suit <- 
    d %>% 
    dplyr::group_by(year,step,prey,length) %>% 
    dplyr::filter(biomass.consumed > 0) %>% 
    dplyr::summarise(suit = sum(biomass.consumed*suit)/sum(biomass.consumed)) %>% 
    dplyr::rename(stock = prey)
  
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
                    survey = ifelse(sitype == 'acoustic',label,survey)) %>% 
      dplyr::left_join(lik.dat$dat$surveyindices %>% 
                         purrr::set_names(.,names(.)) %>% 
                         dplyr::bind_rows(.id='name') %>% 
                         dplyr::rename(observed=number)) %>% 
      dplyr::mutate(length = ifelse(sitype %in% c('lengths','fleets'),
                                    paste(lower,upper,sep=' - '),
                                    length)) %>% 
      dplyr::as_data_frame() %>% 
      dplyr::mutate(predict = ifelse(grepl('loglinearfit',tolower(fittype)),
                                     exp(intercept)*number^slope,
                                     intercept + slope*number))
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
      dplyr::left_join(harv.suit) %>% 
      dplyr::group_by(stock,year,area) %>%
      dplyr::summarise(total.number = sum(number),
                       total.biomass = sum(number*mean.weight),
                       harv.biomass = sum(number*suit*mean.weight),
                       ssb = sum(mean.weight*logit(mat.par[1],
                                                   mat.par[2],
                                                   length)*
                                   number)) %>% 
      dplyr::left_join(f.by.year %>%
                         dplyr::mutate(area = area),
                       by = c("stock","year","area")) %>% 
      dplyr::left_join(stock.recruitment %>% 
                         dplyr::mutate(stock = as.character(stock),
                                       area = paste0('area',area),
                                       year = as.numeric(year))) %>% 
      dplyr::ungroup()
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
      dplyr::mutate(pred.ratio = number.x/sum(number.x,na.rm=TRUE),
                    obs.ratio = number.y/sum(number.y)) %>% 
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
    pred.agg <- 
      names(lik.dat$dat$stomachcontent) %>% 
      purrr::set_names(.,.) %>% 
      purrr::map(~attr(lik.dat$dat$stomachcontent[[.]],'pred.agg')) %>% 
      dplyr::bind_rows(.id='component')
    
    prey.agg <- 
      names(lik.dat$dat$stomachcontent) %>% 
      purrr::set_names(.,.) %>% 
      purrr::map(~attr(lik.dat$dat$stomachcontent[[.]],'prey.agg')) %>% 
      dplyr::bind_rows(.id='component')
    
    
    stomachcontent <-
      lik.dat$dat$stomachcontent %>% 
      dplyr::bind_rows(.id = 'component') %>% 
      dplyr::select(-c(prey.lower,prey.upper,lower,upper)) %>% 
      dplyr::right_join(
        out[lik.dat$dat$stomachcontent %>% 
              names()] %>%
          dplyr::bind_rows(.id = 'component') %>% 
          left_join(prey.agg) %>% 
          left_join(pred.agg),
        by = c('component','predator','prey','year','step','area')) %>% 
      dplyr::group_by(component,year,step,predator) %>%
      dplyr::mutate(observed=ratio/sum(ratio,na.rm=TRUE),
                    predicted=number/sum(number,na.rm=TRUE),
                    prey.length = (prey.lower+prey.upper)/2,
                    pred.length = (lower+upper)/2) %>% 
      dplyr::as_tibble()
    
  } else {
    stomachcontent <- NULL
  }
  
  if('catchstatistics' %in% names(lik.dat$dat)){
    catchstatistics <- 
      lik.dat$dat$catchstatistics %>% 
      names() %>% 
      purrr::set_names(.,.) %>% 
      purrr::map(function(x){
        out[[x]] %>% 
          dplyr::rename(fitted_mean = mean,
                        fitted_number = number) %>% 
          dplyr::left_join(lik.dat$dat$catchstatistics[[x]],
                           by = c("year", "step", "area",  "age")) %>% 
          dplyr::rename(observed_mean = mean,
                        observed_number = number)
        }) %>% 
      dplyr::bind_rows(.id = 'name') %>% 
      dplyr::as_tibble()
  } else {
    catchstatistics <- NULL
  }
  
  
  out <- 
    list(sidat = sidat, resTable = resTable, nesTable = nesTable,
         suitability = predator.prey %>% 
           select(year,step,stock=prey,fleet=predator,length,suit),# gss.suit, 
         stock.growth = stock.growth,
         stock.recruitment = stock.recruitment,
         res.by.year = res.by.year, 
         stomachcontent = stomachcontent,
         likelihoodsummary = out$likelihoodsummary %>% dplyr::as_tibble(),
         catchdist.fleets = catchdist.fleets, 
         stockdist = stockdist,
         #out.fit=out, 
         SS = SS,
         stock.full = stock.full, 
         stock.std = stock.std,
         stock.prey = stock.prey,
         fleet.info = fleet.info,
         predator.prey = predator.prey,
         params = params,
         catchstatistics = catchstatistics)
  class(out) <- c('gadget.fit',class(out))
  save(out,file=sprintf('%s/WGTS.Rdata',wgts))
  return(out)
}


#' @param ... any number of gadget.fit objects
#'
#' @rdname gadget.fit
#' @description \code{bind.gadget.fit} combines two (or more) gadget.fit objects into a 
#' single list. Each object in the list will have an extra column indicating the model, 
#' if the list is named those names will be used to identify the model. 
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


