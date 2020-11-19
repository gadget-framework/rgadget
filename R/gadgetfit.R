##' \code{gadget.fit} calls Gadget and compiles fit statistics, relevant biomass trajectories and consumption.
##' It returns an object of class 'gadget.fit' which is essentially a list containing the following objects: \describe{
##' \item{sidat}{data.frame containing the model fit to the surveyindices}
##' \item{resTable}{Table of likelihood component scores from the different stages of the iterative reweighting run}
##' \item{nesTable}{Same as resTable but normalised with the minimum score for each component}
##' \item{suitability}{The model estimated fleet predator suitability}
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
##' @param printatstart should the stock standard output be printed at the beginning or the end of the timestep
##' @param steps what steps should be printed
##' @param f.age.range data.frame describing the desired age range where the F's are calculated, if null this defaults to the apical F fro all stocks. 
##' Input columns should include stock, age.min and age.max 
##' @param recruitment_step_age data frame defining the recruitment age by stock and time step. Default is the minimum age at step 1. Expects columns stock, age and step 
##' @param gd gadget_directory object to optionally set the location of the gadget directory
##' @return list containing the output from Gadget. 
##' @importFrom rlang .data
##' @author Bjarki Thor Elvarsson
##' @export
gadget.fit <- function(wgts = 'WGTS', 
                       main.file = NULL,
                       fleet.predict = NULL,
                       mat.par=NULL, 
                       params.file=NULL,
                       f.age.range=NULL, 
                       fit.folder = 'FIT',
                       printatstart = 1, 
                       steps = 1,
                       recruitment_step_age = NULL,
                       gd = NULL){
  
  print('FUNCTION DEPRECATED use gadget_fit instead')
  
  
  old.dir <- getwd()
  on.exit(setwd(old.dir))
  
  
   if(!is.null(gd)){
    setwd(gd)
  } else {
    gd <-  gadget.variant.dir('.')
  }
  
  if(!is.null(f.age.range) & !('data.frame' %in% class(f.age.range))){
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
    dir.create(fit.folder,showWarnings = FALSE)
    params <- if(!is.null(params.file)) read.gadget.parameters(params.file) else NULL
    lik <- read.gadget.likelihood(main$likelihoodfiles)
  }
  
  
  print('Reading input data')
  lik.dat <- read.gadget.data(lik)
  
  stocks <- 
    main$stockfiles %>% 
    purrr::map(~read.gadget.file(path='.',file_name = .,file_type = 'stock',recursive = FALSE)) 
  names(stocks) <- stocks %>% purrr::map(1) %>% purrr::map('stockname') %>% unlist()
  
  ## model output, i.e printfiles
  make.gadget.printfile(main.file = main.file,
                        file = sprintf('%s/printfile.fit',wgts),
                        gd = list(dir='.',output = sprintf('%s/out.fit',wgts),
                               aggfiles = sprintf('%s/print.aggfiles',wgts)),
                        recruitment_step_age = recruitment_step_age,
                        printatstart = printatstart,
                        steps = steps)
  on.exit(unlink(sprintf('%s/out.fit',wgts),recursive = TRUE))
  
  main$printfiles <- sprintf('%s/printfile.fit',wgts)
  write.gadget.main(main,file = sprintf('%s/main.print',wgts))
  
  print("Running Gadget")
  callGadget(s=1,
             i = params.file,
             main = sprintf('%s/main.print',wgts),
             o = sprintf('%s/SS.print',wgts))
  
  print("Reading output files") 
  out <- read.printfiles(sprintf('%s/out.fit',wgts))
  SS <- tryCatch(read.gadget.lik.out(sprintf('%s/SS.print',wgts)),
                 error = function(e) 'SS could not be read')
  #stocks <- read.gadget.stockfiles(main$stockfiles)
  
  print('Gathering results')

  stock.recruitment <- 
    out[sprintf('%s.recruitment',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::select(.data$stock,.data$year,.data$area,.data$step, recruitment=.data$number)
  
  stock.full <-
    out[sprintf('%s.full',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::mutate(length=as.numeric(gsub('len','',.data$length))) 
  
  stock.std <- 
    out[sprintf('%s.std',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') 
  
  if(sum(grepl('prey$',names(out)))>0){
  
    stock.prey <- 
      out[sprintf('%s.prey',names(stocks))] %>% 
      purrr::set_names(.,names(stocks)) %>% 
      dplyr::bind_rows(.id='stock') 
  } else {
    stock.prey <- 
      tibble::tibble(year=NA_real_, step=NA_real_, area=NA_character_, stock = NA_character_,
                     age=NA_real_,biomass_consumed=NA_real_,number_consumed=NA_real_, mortality = NA_real_)
    
  } 
  if(sum(grepl('prey',names(out)))>0){
    predator.prey <- 
      out[grepl('.+\\.prey\\..+',names(out))] %>% 
      purrr::set_names(.,names(.)) %>% 
      purrr::keep(~'number_consumed' %in% names(.)) %>% 
      dplyr::bind_rows(.id='stock') %>% 
      tidyr::separate(.data$stock,c('prey','predator'),sep='\\.prey\\.') %>% 
      dplyr::group_by(.data$year,.data$step,.data$prey,.data$predator) %>% 
      dplyr::mutate(suit = .data$mortality/max(.data$mortality),
                    suit = ifelse(is.finite(.data$suit),.data$suit,0),
                    length = gsub('len', '', .data$length) %>% 
                      as.numeric())
  } else {
    predator.prey <- 
      tibble::tibble(year=NA_real_, step=NA_real_, area=NA_character_,
                     predator = NA_character_, 
                     prey = NA_character_, 
                     length = NA_real_, 
                     suit = NA_real_,
                     biomass_consumed = NA_real_, mortality = NA_real_)
    
  }
  
  
  fleet.catches <- 
    predator.prey %>% 
    dplyr::group_by(.data$year,.data$area,.data$predator,.data$prey) %>% 
    dplyr::summarise(amount = sum(.data$biomass_consumed)) %>% 
    dplyr::rename(fleet = .data$predator, stock = .data$prey)
  
  fleet.info <- 
    stock.full %>%
    dplyr::left_join(predator.prey %>% 
                       dplyr::select(.data$year,
                                     .data$step, 
                                     .data$area,
                                     fleet = .data$predator, 
                                     stock = .data$prey, 
                                     .data$length, 
                                     .data$suit),
                     by = c("stock", "year", "step", "area", "length")) %>%
    dplyr::group_by(.data$year,.data$step,.data$area,.data$fleet) %>%
    dplyr::summarise(harv.bio = sum(.data$suit*.data$number*.data$mean_weight)) %>%
    dplyr::left_join(fleet.catches %>% 
                       dplyr::group_by(.data$year,.data$fleet,.data$area) %>% 
                       dplyr::summarise(amount=sum(.data$amount)),
                     by = c("year", "area", "fleet")) %>%
    dplyr::group_by(.data$year,.data$step,.data$area,.data$fleet) %>%
    dplyr::mutate(amount = ifelse(is.na(.data$amount),0,.data$amount),
                  harv.rate = .data$amount/.data$harv.bio)
  
  

  if(!is.null(fleet.predict)){
    d <- 
      predator.prey %>% 
      dplyr::filter(.data$predator %in% fleet.predict$fleet)
  } else {
    d <- predator.prey
  }
  
  harv.suit <- 
    d %>% 
    dplyr::group_by(.data$year,.data$step,.data$prey,.data$length) %>% 
    dplyr::filter(.data$biomass_consumed > 0) %>% 
    dplyr::summarise(suit = sum(.data$biomass_consumed*.data$suit)/sum(.data$biomass_consumed)) %>% 
    dplyr::rename(stock = .data$prey)
  
  print('Merging input and output')
  ## merge data and estimates
  if('surveyindices' %in% names(lik.dat$dat)){
    sidat <- 
      out[names(lik.dat$dat$surveyindices)] %>% 
      purrr::set_names(.,names(.)) %>%
      dplyr::bind_rows(.id='name') %>% 
      dplyr::left_join(lik$surveyindices %>% 
                         dplyr::select(.data$name,.data$stocknames,.data$sitype,.data$fittype), 
                       by='name') %>% 
      dplyr::bind_rows(tibble::tibble(length=NA,
                                         age=NA,
                                         survey = NA,
                                         fleet = NA)) %>% 
      dplyr::mutate(age = ifelse(.data$sitype == 'ages',.data$label,.data$age),
                    length = ifelse(.data$sitype %in% c('lengths','fleets'),.data$label,.data$length),
                    fleet = ifelse(.data$sitype == 'effort',.data$label,.data$fleet),
                    survey = ifelse(.data$sitype == 'acoustic',.data$label,.data$survey)) %>% 
      dplyr::left_join(lik.dat$dat$surveyindices %>% 
                         purrr::set_names(.,names(.)) %>% 
                         dplyr::bind_rows(.id='name') %>% 
                         dplyr::as_tibble() %>% 
                         dplyr::rename(observed=.data$number) %>% 
                         dplyr::bind_rows(tibble::tibble(name = NA, year = NA, step = NA, 
                                                         area = NA,length = NA,age = NA,
                                                         fleet = NA,survey = NA,
                                                         upper = NA, lower = NA)) %>% 
                         dplyr::filter(!is.na(.data$year)),
                       by = c("name", "year", "step", "area", "age", "length","fleet","survey")) %>% 
      dplyr::mutate(length = ifelse(.data$sitype %in% c('lengths','fleets'),
                                    paste(.data$lower,.data$upper,sep=' - '),
                                    .data$length)) %>% 
      dplyr::mutate(predict = ifelse(grepl('loglinearfit',tolower(.data$fittype)),
                                     exp(.data$intercept)*.data$number^.data$slope,
                                     .data$intercept + .data$slope*.data$number)) %>% 
      dplyr::filter(!is.na(.data$name))
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
      tibble::as_tibble()
    
    catchdist.fleets <-
      lik.dat$dat$catchdistribution %>% 
      purrr::set_names(.,names(.)) %>%
      purrr::map(. %>% dplyr::mutate(age = as.character(.data$age))) %>%
      dplyr::bind_rows(.id='name') %>%  
      dplyr::right_join(out[dat.names] %>%
                          purrr::set_names(.,dat.names) %>%
                          purrr::map(. %>% dplyr::mutate(age = as.character(.data$age))) %>%
                          dplyr::bind_rows(.id='name') %>% 
                          dplyr::left_join(aggs,by=c('name','length')) ,
                        by=c('name','length', 'year',
                             'step', 'area','age','upper','lower')) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(.data$name,.data$year, .data$step,  .data$area) %>%
      dplyr::mutate(total.catch = sum(.data$number.x,na.rm=TRUE),
                    total.pred = sum(.data$number.y,na.rm=TRUE),
                    observed = .data$number.x/sum(.data$number.x,na.rm=TRUE),
                    predicted = .data$number.y/sum(.data$number.y,na.rm=TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(.data$name,.data$length,.data$age) %>%
      dplyr::mutate(upper = as.double(max(ifelse(is.na(.data$upper),0.0,
                                                 .data$upper))),
                    lower = as.double(max(ifelse(is.na(.data$lower),0.0,
                                                 .data$lower))),
                    avg.length = as.numeric((.data$lower+.data$upper)/2),
                    residuals = as.numeric(.data$observed - .data$predicted)) %>% 
      dplyr::inner_join(lik$catchdistribution %>% 
                          dplyr::select(.data$name,.data$fleetnames,.data$stocknames),
                        by = 'name')
  } else {
    catchdist.fleets <- NULL
  }
  
  
  if(sum(grepl('.std',names(out),fixed = TRUE))>0){
    
    if(is.null(f.age.range)){
      f.age.range <- 
        stock.prey %>% 
        dplyr::group_by(.data$stock) %>% 
        dplyr::summarise(age.min = max(.data$age),age.max=max(.data$age))
    }
    
    
    f.by.year <- 
      stock.prey %>% 
      dplyr::left_join(f.age.range,by="stock") %>% 
      dplyr::group_by(.data$stock,.data$year,.data$area) %>%
      dplyr::summarise(catch=sum(.data$biomass_consumed),
                       num.catch=sum(.data$number_consumed),
                       F=mean(.data$mortality[.data$age>=.data$age.min&.data$age<=.data$age.max]))
    
    res.by.year <- 
      stock.full %>% 
      dplyr::filter(.data$step %in% steps) %>%
      dplyr::left_join(harv.suit,
                       by = c("stock", "year", "step", "length")) %>% 
      dplyr::group_by(.data$stock,.data$year,.data$area,.data$step) %>%
      dplyr::summarise(total.number = sum(.data$number),
                       total.biomass = sum(.data$number*.data$mean_weight),
                       harv.biomass = sum(.data$number*.data$suit*.data$mean_weight),
                       ssb = sum(.data$mean_weight*logit(mat.par[1],
                                                   mat.par[2],
                                                   .data$length)*
                                   .data$number)) %>% 
      dplyr::left_join(f.by.year %>%
                         dplyr::mutate(area = .data$area),
                       by = c("stock","year","area")) %>% 
      dplyr::left_join(stock.recruitment %>% 
                         dplyr::group_by(.data$stock,.data$year,.data$area) %>% 
                         dplyr::summarise(recruitment = sum(.data$recruitment)),
                       by = c('stock','year','area')) %>% 
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
      dplyr::as_tibble()
    
    stockdist <-
      lik.dat$dat$stockdistribution %>% 
      purrr::set_names(.,names(.)) %>%
      dplyr::bind_rows(.id='name') %>% 
      dplyr::right_join(out[dat.names] %>%
                          purrr::set_names(.,dat.names) %>% 
                          dplyr::bind_rows(.id='name') %>% 
                          dplyr::left_join(aggs,by=c('name','length')),
                        by=c('name','length', 'year',
                             'step', 'area','age',
                             'stock','upper','lower'),
                        suffix = c('.y','.x')) %>% 
      dplyr::group_by(.data$name, .data$year, .data$step, 
                      .data$area, .data$age, .data$length) %>% 
      dplyr::mutate(pred.ratio = .data$number.x/sum(.data$number.x,na.rm=TRUE),
                    obs.ratio = .data$number.y/sum(.data$number.y)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(length = (.data$lower+.data$upper)/2) %>% 
      dplyr::inner_join(lik$stockdistribution %>% 
                          dplyr::select(.data$name,.data$fleetnames,.data$stocknames) %>% 
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
      dplyr::select(-c(.data$prey.lower,.data$prey.upper,
                       .data$lower,.data$upper)) %>% 
      dplyr::right_join(
        out[lik.dat$dat$stomachcontent %>% 
              names()] %>%
          dplyr::bind_rows(.id = 'component') %>% 
          dplyr::left_join(prey.agg) %>% 
          dplyr::left_join(pred.agg),
        by = c('component','predator','prey','year','step','area')) %>% 
      dplyr::group_by(.data$component,.data$year,.data$step,.data$predator) %>%
      dplyr::mutate(observed=.data$ratio/sum(.data$ratio,na.rm=TRUE),
                    predicted=.data$number/sum(.data$number,na.rm=TRUE),
                    prey.length = (.data$prey.lower+.data$prey.upper)/2,
                    pred.length = (.data$lower+.data$upper)/2) %>% 
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
          dplyr::rename(fitted_mean = .data$mean,
                        fitted_number = .data$number) %>% 
          dplyr::left_join(lik.dat$dat$catchstatistics[[x]],
                           by = c("year", "step", "area",  "age")) %>% 
          dplyr::rename(observed_mean = .data$mean,
                        observed_number = .data$number)
        }) %>% 
      dplyr::bind_rows(.id = 'name') %>% 
      dplyr::as_tibble()
  } else {
    catchstatistics <- NULL
  }
  
  
  out <- 
    list(sidat = sidat, resTable = resTable, nesTable = nesTable,
         suitability = predator.prey %>% 
           dplyr::select(.data$year,.data$step,stock=.data$prey,
                         fleet=.data$predator,.data$length,.data$suit),# gss.suit, 
         #stock.growth = stock.growth,
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
setwd(old.dir)
  
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


get_gadget_recruitment <- function(stocks,params){
  stocks %>% 
    purrr::map(function(x){
      if(x$doesrenew$doesrenew){
        
        if('normalparamfile' %in% names(x$doesrenew)){
          key <- 'normalparamfile'
          col.names <- c('year','step','area','age','number','mean','stddev','alpha','beta')
        } else if('normalcondfile' %in% names(x$doesrenew)){
          key <- 'normalcondfile'
          col.names <- c('year','step','area','age','number','mean','stddev','relcond')
        } else {
          key <- 'numberfile'
          col.names <- c('year','step','area','age','length','number','weight')
        }
        utils::capture.output(x$doesrenew[[key]] %>% print()) %>% 
          utils::read.table(text = ., comment.char = ';',sep = '\t',fill = TRUE,stringsAsFactors = FALSE) %>% 
          stats::na.omit() %>% 
          purrr::set_names(.,col.names)
      } else{
        NULL
      }
    }) %>% 
    dplyr::bind_rows(.id = 'stock') %>% 
    dplyr::mutate_at(.vars=dplyr::vars(-.data$stock),
                     ~purrr::map(.,function(x) 
                       tryCatch(parse.gadget.formulae(x) %>% 
                                  eval(params %>% 
                                         dplyr::select(.data$value) %>% 
                                         t() %>% 
                                         purrr::set_names(.,dimnames(.)[[2]]) %>% 
                                         as.list()),
                                error = function(e) 0)))
}


#' Retro fit
#'
#' Compile the fit objects from retrospective runs
#' @param pre Folder containing the retro run
#' @return gadget fit list
#' @export
gadget.retro.fit <- function(pre='RETRO'){
  
  tmp_func <- purrr::lift(bind.gadget.fit,.unnamed = TRUE)
  list.files(pre,pattern = 'R[0-9]+') %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(function(x){
      print(x)
      gadget.fit(main.file = sprintf('%s/%s/main',pre,x),
                 params.file = sprintf('%s/params.retro.%s',pre,gsub('R','',x)),
                 wgts = NULL)
    }) %>% 
    tmp_func()
}

