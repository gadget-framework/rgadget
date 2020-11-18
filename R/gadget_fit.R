gadget_fit <- function(gd, params.in = 'params.in', fit = 'FIT'){
  
  main <- read.gadget.file(gd,attr(gd,'mainfile'), recursive = FALSE)
  attr(main, 'file_name') <- 'main'
  stocks <- 
    main$stock$stockfiles %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(~read.gadget.file(path=gd,file_name = .,
                                 file_type = 'stock',
                                 recursive = FALSE)) 
  
  lik <- 
    main$likelihood$likelihoodfiles %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(~read.gadget.file(path=gd,file_name = .,
                                 file_type = 'likelihood',
                                 recursive = TRUE) %>%  
                 purrr::discard(~.$type %in% c('penalty','understocking','migrationpenalty')))
    
  
  fleets <- 
    main$fleet$fleetfiles %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(~read.gadget.file(path=gd,file_name = .,
                                 file_type = 'fleet',
                                 recursive = TRUE,
                                 missingOkay = TRUE)) 
  
  gv <- 
    gadget.variant.dir(gd, variant_dir = fit)
  
  write.gadget.file(main,gv)
  
  print <- 
    gadgetprintfile('printfile',gv, missingOkay = TRUE) 
  
  ## add the likelihood output to the printfile
  for(comp in lik$likelihood %>% purrr::map('name')){
    print <- 
      print %>% 
      gadget_update('likelihoodprinter',printfile = comp, likelihoodcomponent = comp)
  }
  
  ## add stock printers 
  
  for(stock in names(stocks)){
    print <- 
      print %>% 
      gadget_update('stockstdprinter',
                    printfile = paste(stock,'std', sep = '.'),
                    stockname  = stock) %>% 
      gadget_update('stockprinter',
                    printfile = paste(stock, 'full', sep = '.'),
                    stocknames = stock,
                    area = livesonareas(stocks[[stock]]) %>% purrr::set_names(.,.) %>% as.list(),
                    age = list(allages = age_range(stocks[[stock]])),
                    len = tibble::tibble(lower = head(length_range(stocks[[stock]]),-1),
                                         upper = tail(length_range(stocks[[stock]]),-1)) %>% 
                      dplyr::mutate(label = as.ordered(.data$lower)) %>%
                      split(.$label) %>% 
                       purrr::set_names(.,paste0('len',names(.))) %>% 
                      purrr::map(dplyr::select, -.data$label) %>% 
                      purrr::map(unlist)) %>% 
      gadget_update('stockprinter',
                    printfile = paste(stock, 'recruitment', sep = '.'),
                    stocknames = stock,
                    area = livesonareas(stocks[[stock]]) %>% purrr::set_names(.,.) %>% as.list(),
                    age = list(recage = stocks[[stock]][[1]]$minage),
                    len = list(alllen = length_range(stocks[[stock]])))
  }
  
  prey.subset <- 
    stocks %>% purrr::keep(~.$iseaten$iseaten == 1) %>% names()
  
  pred.subset <- 
    stocks %>% purrr::keep(~.$doeseat$doeseat == 1) %>% names()
  
  for(prey in prey.subset){
    ## get the fishing mortality by age
    if(length(fleets) > 0){
      print <- 
        print %>% 
        gadget_update('predatorpreyprinter',
                      printfile = sprintf('%s.prey',prey),
                      predatornames = fleets[[1]] %>% purrr::map(1),
                      preynames = prey,
                      area = livesonareas(stocks[[prey]]) %>% purrr::set_names(.,.) %>% as.list(),
                      age = age_range(stocks[[prey]]) %>% purrr::set_names(.,.) %>% as.list(),
                      len =  list(alllen = length_range(stocks[[prey]]))) 
      
    }
    for(predator in c(pred.subset,fleets[[1]] %>% purrr::map(1))){
      ## calculate the total consumption by predator, predator and size. 
      # Gives selection and suitability 
      print <- 
        print %>% 
        gadget_update('predatorpreyprinter',
                      printfile = sprintf('%s.prey.%s', prey, predator),
                      predatornames = predator,
                      preynames = prey,
                      area = livesonareas(stocks[[prey]]) %>% purrr::set_names(.,.) %>% as.list(),
                      age = list(allages = age_range(stocks[[prey]])),
                      len = tibble::tibble(lower = head(length_range(stocks[[prey]]),-1),
                                           upper = tail(length_range(stocks[[prey]]),-1)) %>% 
                        dplyr::mutate(label = as.ordered(.data$lower))  %>%
                        dplyr::arrange(.data$lower) %>% 
                        split(.$label) %>% 
                        purrr::set_names(.,paste0('len',names(.))) %>% 
                        purrr::map(dplyr::select, -.data$label) %>% 
                        purrr::map(unlist)) 

    }
  }
  
  ## finalize and save 
  write.gadget.file(print,gv)
  
  ## call gadget
  gv <- gadget_evaluate(gv,params.in = params.in, params.out = tempfile())
  
  ## read in the output
  out <- gadgetprintfile('printfile',gv)
  
  
  out %>% 
    purrr::map('printfile') %>% 
    purrr::set_names(.,purrr::map(.,~attr(.,'file_name')) %>% gsub('out/','',.)) %>% 
    purrr::map(1) %>% 
    purrr::map(tibble::as_tibble)
  
  
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
  
  return(out)
  
}


age_range <- function(stock){
  stock[[1]]$minage:stock[[1]]$maxage
}

length_range <- function(stock){
  seq(stock[[1]]$minlength,
      stock[[1]]$maxlength, 
      by = stock[[1]]$dl)
}



livesonareas <- function(stock){
  stock[[1]]$livesonareas 
}
