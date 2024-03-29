#' Gadget fit 
#'
#' @param gd gadget.variant.dir
#' @param params.in input parameters
#' @param fit location of the fit folder
#' @param f.age.range age range for F
#' @param weighted.mortality Method for calculating annual mortality. TRUE for a weighted mean, FALSE for an unweighted mean. This is only applicable if f.age.range is specified. Defaults to FALSE.
#' @param rec.at.age Age at which recruitment occurs, integer value that applies across stocks.
#' @param rec.steps Step(s) at which recruitment occurs, defaults to "all" & applies to all stocks. Note that recruitment is printed at the start of the timestep. 
#' @param steps (unused)
#'
#' @return list
#' @export
gadget_fit <- function(gd, 
                       params.in = attr(gd,'params_in'), 
                       fit = 'FIT', 
                       f.age.range = NULL, 
                       weighted.mortality = FALSE,
                       rec.age = NULL,
                       rec.steps = NULL,
                       steps = 1){
  
  fit <- variant_strip_path(gd,fit)
  main <- read.gadget.file(gd,attr(gd,'mainfile'), recursive = FALSE)
  attr(main, 'file_name') <- 'main'
  stocks <- 
    main$stock$stockfiles %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(~read.gadget.file(path=gd,file_name = .,
                                 file_type = 'stock',
                                 recursive = FALSE)) 
  
  ## ensure that the stock objects are correctly as people have the option of 
  ## giving the stock file an arbitrary name
  
  names(stocks) <- 
    stocks %>% 
    purrr::map(~.[[1]]['stockname']) %>% 
    purrr::flatten()
  
  lik <- 
    main$likelihood$likelihoodfiles %>% 
    purrr::set_names(.,.) %>% 
    purrr::map(~read.gadget.file(path=gd,file_name = .,
                                 file_type = 'likelihood',
                                 recursive = TRUE) %>%  
                 purrr::discard(~.$type %in% c('penalty','understocking','migrationpenalty'))) %>% 
    purrr::flatten(.) 
  
  lik <- 
    lik %>% 
    purrr::set_names(.,lik %>% purrr::map('name')) 
  
  lik.dat <- 
    lik %>% 
    purrr::map(lik_to_tibble) 
  
  
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
  for(comp in (lik %>% purrr::map('name'))){
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
                    len = tibble::tibble(lower = utils::head(length_range(stocks[[stock]]),-1),
                                         upper = utils::tail(length_range(stocks[[stock]]),-1)) %>% 
                      dplyr::mutate(label = as.ordered(.data$lower)) %>%
                      split(.$label) %>% 
                      purrr::set_names(.,paste0('len',names(.))) %>% 
                      purrr::map(dplyr::select, -.data$label) %>% 
                      purrr::map(unlist)) %>% 
      gadget_update('stockprinter',
                    printfile = paste(stock, 'recruitment', sep = '.'),
                    stocknames = stock,
                    area = livesonareas(stocks[[stock]]) %>% purrr::set_names(.,.) %>% as.list(),
                    age = list(recage = ifelse(is.null(rec.age), stocks[[stock]][[1]]$minage, rec.age)),
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
                      len = tibble::tibble(lower = utils::head(length_range(stocks[[prey]]),-1),
                                           upper = utils::tail(length_range(stocks[[prey]]),-1)) %>% 
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
  gv <- gadget_evaluate(gv,params.in = params.in, 
                        params.out = tempfile(), 
                        log = paste(attr(gv,"variant_dir"),'fit.log',sep = '/'))
  
  ## read in the output
  out <- 
    gadgetprintfile('printfile',gv) %>% 
    purrr::set_names(.,purrr::map(.,~attr(.$printfile,'file_name')) %>%
                       gsub('out/','',.)) %>% 
    purrr::map(print_to_tibble)
  
  
  print('Gathering results')
  
  ## Sort recruitment steps
  stock.recruitment <- 
    out[sprintf('%s.recruitment',names(stocks))] %>% 
    purrr::set_names(.,names(stocks)) %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::select(.data$stock,.data$year,.data$area,.data$step, recruitment=.data$number) 
  
  ## Specific steps
  if (!is.null(rec.steps)){
    ## Check
    if (all(rec.steps %in% unique(stock.recruitment$step))){
      stock.recruitment <- 
        stock.recruitment %>% 
        dplyr::filter(step %in% rec.steps)
    }
    else{
      warning(paste0("Output not filtered by recruitment step because specified steps were not found. Available steps: ",
                     paste(unique(stock.recruitment$step), collapse=","),", specified steps: ", paste(rec.steps, collapse=",")))
    }
    
  } 
  
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
  
  
  # 
  # if(!is.null(fleet.predict)){
  #   d <- 
  #     predator.prey %>% 
  #     dplyr::filter(.data$predator %in% fleet.predict$fleet)
  # } else {
  d <- predator.prey
  #  }
  
  harv.suit <- 
    d %>% 
    dplyr::group_by(.data$year,.data$step,.data$prey,.data$length) %>% 
    dplyr::filter(.data$biomass_consumed > 0) %>% 
    dplyr::summarise(suit = sum(.data$biomass_consumed*.data$suit)/sum(.data$biomass_consumed)) %>% 
    dplyr::rename(stock = .data$prey)
  
  print('Merging input and output')
  
  for(comp in names(lik.dat)){
    out[[comp]] <- 
      out[[comp]] %>% 
      dplyr::mutate(area = as.character(.data$area))
    if ('age' %in% names(out[[comp]])) out[[comp]] <- dplyr::mutate(out[[comp]], age = as.character(.data$age))

    if(lik.dat[[comp]]$type[1] == 'surveyindices'){
      if(lik.dat[[comp]]$sitype[1] == 'lengths'){
        out[[comp]] <- 
          out[[comp]] %>% 
          dplyr::rename(length = .data$label)
      } else if(lik.dat[[comp]]$sitype[1] == 'age'){
        out[[comp]] <- 
          out[[comp]] %>% 
          dplyr::rename(age = .data$label)
      } else if(lik.dat[[comp]]$sitype[1] == 'effort'){
        out[[comp]] <- 
          out[[comp]] %>% 
          dplyr::rename(fleet = .data$label)
      } else if(lik.dat[[comp]]$sitype[1] == 'acoustic'){
        out[[comp]] <- 
          out[[comp]] %>% 
          dplyr::rename(survey = .data$label)
      } else if(lik.dat[[comp]]$sitype[1] == 'fleet'){
        out[[comp]] <- 
          out[[comp]] %>% 
          dplyr::rename(length = .data$label)
      }
    }
    
    lik.dat[[comp]] <- 
      lik.dat[[comp]] %>% 
      dplyr::left_join(out[[comp]], 
                       by = intersect(names(out[[comp]]), names(lik.dat[[comp]])))
  }
  
  
  ## merge data and estimates
  if('surveyindices' %in% (lik %>% purrr::map('type'))){
    sidat <- 
      lik.dat %>% 
      purrr::keep(~'surveyindices' %in% .$type) %>% 
      dplyr::bind_rows() %>% 
      dplyr::bind_rows(tibble::tibble(length=NA,
                                      age=NA,
                                      survey = NA,
                                      fleet = NA)) %>% 
      dplyr::mutate(length = ifelse(.data$sitype %in% c('lengths','fleets'),
                                    paste(.data$lower,.data$upper,sep=' - '),
                                    .data$length)) %>% 
      dplyr::mutate(number = .data$predicted, 
                    predicted = ifelse(grepl('loglinearfit',tolower(.data$fittype)),
                                       exp(.data$intercept)*.data$number^.data$slope,
                                       .data$intercept + .data$slope*.data$number)) %>% 
      dplyr::filter(!is.na(.data$name))
  } else {
    sidat <- NULL
  }
  
  
  
  if('catchdistribution' %in% (lik %>% purrr::map('type'))){
    
    catchdist.fleets <-
      lik.dat %>% 
      purrr::keep(~'catchdistribution' %in% .$type) %>% 
      dplyr::bind_rows() %>% 
      dplyr::group_by(.data$name,.data$year, .data$step,  .data$area) %>%
      dplyr::mutate(total.catch = sum(.data$observed,na.rm=TRUE),
                    total.pred = sum(.data$predicted,na.rm=TRUE),
                    obs = .data$observed,
                    pred = .data$predicted,
                    observed = .data$observed/sum(.data$observed,na.rm=TRUE),
                    predicted = .data$predicted/sum(.data$predicted,na.rm=TRUE)) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(.data$name,.data$length,.data$age) %>%
      dplyr::mutate(upper = as.double(max(ifelse(is.na(.data$upper),0.0,
                                                 .data$upper))),
                    lower = as.double(max(ifelse(is.na(.data$lower),0.0,
                                                 .data$lower))),
                    avg.length = as.numeric((.data$lower+.data$upper)/2),
                    residuals = as.numeric(.data$observed - .data$predicted)) 
  } else {
    catchdist.fleets <- NULL
  }
  
  
  if(sum(grepl('.std',names(out),fixed = TRUE))>0){
    
    if(is.null(f.age.range)){
      
      ## No need to re-calculate F as all ages considered
      weighted.mortality <- FALSE
      
      ## Defaults to maximum age for each stock
      f.age.range <- 
        stock.prey %>% 
        dplyr::group_by(.data$stock) %>% 
        dplyr::summarise(age.min = max(.data$age),age.max=max(.data$age))
    }
    
    ## Mean mortality weighted by number in age class
    f.by.year <- 
      stock.prey %>% 
      dplyr::left_join(f.age.range,by="stock") %>% 
      dplyr::group_by(.data$stock,.data$year,.data$area) %>%
      dplyr::summarise(catch=sum(.data$biomass_consumed),
                       num.catch=sum(.data$number_consumed),
                       F=mean(.data$mortality[.data$age>=.data$age.min&.data$age<=.data$age.max]))
    
    ## Adjust mean F to a weighted mean
    if (weighted.mortality){
      
      f.by.year <- 
        f.by.year %>% 
        #    mutate(oldf=F) %>% 
        select(-F) %>% 
        left_join(stock.std %>% 
                    dplyr::select(-c(number_consumed, biomass_consumed)) %>% 
                    dplyr::left_join(stock.prey %>% 
                                       dplyr::select(year,step,age,area,stock,
                                                     number_consumed, biomass_consumed, mortality)) %>%
                    dplyr::left_join(f.age.range, by="stock") %>%
                    tibble::as_tibble() %>% 
                    dplyr::filter(.data$age >= .data$age.min & .data$age <= .data$age.max) %>% 
                    dplyr::group_by(year, step) %>% 
                    dplyr::summarise(F = -log(1 - sum(number_consumed)/sum(number))/0.25)  %>% 
                    dplyr::ungroup() %>% 
                    dplyr::group_by(year) %>% 
                    dplyr::summarise(F = mean(F)))
      
    }
    
    res.by.year <- 
      stock.full %>% 
      dplyr::filter(.data$step %in% steps) %>%
      dplyr::left_join(harv.suit,
                       by = c("stock", "year", "step", "length")) %>% 
      dplyr::group_by(.data$stock,.data$year,.data$area,.data$step) %>%
      dplyr::summarise(total.number = sum(.data$number),
                       total.biomass = sum(.data$number*.data$mean_weight),
                       harv.biomass = sum(.data$number*.data$suit*.data$mean_weight)) %>% 
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
  
  if('stockdistribution' %in% (lik %>% purrr::map('type'))){
    
    stockdist <-
      lik.dat %>% 
      purrr::keep(~'stockdistribution' %in% .$type) %>% 
      dplyr::bind_rows() %>% 
      dplyr::group_by(.data$name, .data$year, .data$step, 
                      .data$area, .data$age, .data$length) %>% 
      dplyr::mutate(pred.ratio = .data$predicted/sum(.data$predicted,na.rm=TRUE),
                    obs.ratio = .data$observed/sum(.data$observed)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(length = (.data$lower+.data$upper)/2) 
    
  } else {
    stockdist <- NULL
  }
  
  
  if('stomachcontent' %in% (lik %>% purrr::map('type'))){
    
    
    stomachcontent <-
      lik.dat %>% 
      purrr::keep(~'stomachcontent' %in% .$type) %>% 
      dplyr::bind_rows() %>% 
      dplyr::group_by(.data$component,.data$year,.data$step,.data$predator) %>%
      dplyr::mutate(observed=.data$ratio/sum(.data$ratio,na.rm=TRUE),
                    predicted=.data$number/sum(.data$number,na.rm=TRUE),
                    prey.length = (.data$prey.lower+.data$prey.upper)/2,
                    pred.length = (.data$lower+.data$upper)/2) %>% 
      dplyr::as_tibble()
    
  } else {
    stomachcontent <- NULL
  }
  
  if('catchstatistics' %in% (lik %>% purrr::map('type'))){
    
    catchstatistics <- 
      lik.dat %>% 
      purrr::keep(~'catchstatistics' %in% .$type) %>% 
      dplyr::bind_rows() 
    
  } else {
    catchstatistics <- NULL
  }
  
  
  out <- 
    list(sidat = sidat, #resTable = resTable, nesTable = nesTable,
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
#         SS = SS,
         stock.full = stock.full, 
         stock.std = stock.std,
         stock.prey = stock.prey,
         fleet.info = fleet.info,
         predator.prey = predator.prey,
#         params = params,
         catchstatistics = catchstatistics)
  class(out) <- c('gadget.fit',class(out))
  save(out,file=sprintf('%s/fit.Rdata',variant_full_path(gd,variant_within_path(gd,fit))))
  
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



lik_to_tibble <- function(comp){
  dat <- 
    comp$datafile[[1]] %>% 
    dplyr::rename(observed = .data$number) %>% 
    dplyr::mutate(name = comp$name,
                  weight = comp$weight,
                  type = comp$type) %>% 
    tibble::as_tibble()
  
  if(comp$type == 'surveyindices'){
    
    dat <- 
      dat %>% 
      dplyr::mutate(sitype = comp$sitype,
                    fittype = comp$fittype,
                    stocknames = paste(comp$stocknames, collapse = '\t'))
    
  } else if(comp$type %in% c('catcdistribution','stockdistribution')){
    
    dat <- 
      dat %>% 
      dplyr::mutate(data_function = comp[["function"]],
                    aggregationlevel = comp$aggregationlevel,
                    overconsumption = comp$overconsumption,
                    epsilon = comp$epsilon,
                    fleetnames = paste(comp$fleetnames, collapse = '\t'))
  }
  
  if(!is.null(comp$areaaggfile)){
    
    dat <- 
      dat %>% 
      dplyr::mutate(area = as.character(.data$area)) %>% 
      dplyr::inner_join(comp$areaaggfile[[1]] %>% {tibble::tibble(area = names(.), area_range = paste(unlist(.), collapse = '\t'))},
                        by = 'area') 
    
  }
  
  if(!is.null(comp$lenaggfile)){
    
    dat <- 
      dat %>% 
      dplyr::inner_join(comp$lenaggfile %>% 
                          utils::capture.output() %>% 
                          paste(collapse = '\n') %>% 
                          readr::read_table2(.,comment = ';',col_names = c('length','lower','upper')),
                        by = 'length')
  }
  
  if(!is.null(comp$ageaggfile)){
    
    dat <- 
      dat %>% 
      dplyr::mutate(age = as.character(.data$age)) %>%
      dplyr::inner_join(comp$ageaggfile[[1]] %>% 
                          {tibble::tibble(age = names(.), 
                                          age_range = purrr::map(.,paste,collapse = '\t') %>% 
                                            unlist())},
                        by = 'age') 
    
  }
  
  return(dat)  
  
}


print_to_tibble <- function(comp){
  dat <- 
    comp$printfile[[1]] %>% 
    dplyr::mutate(name = attr(comp$printfile,'file_name') %>% gsub('out/','',.),
                  print_type = comp$type) %>% 
    dplyr::bind_cols(attr(comp$printfile[[1]],'preamble') %>% purrr::set_names(paste('preamble',1:length(.),sep='_')))
  
  if(comp$type == 'likelihoodprinter'){
    dat <- 
      dat %>% 
      dplyr::rename(predicted = .data$number)
  }
  
  ## digest the postamble, only used for survey indices
  #pos <- grep('Regression information',tmp)
  if(!is.null(attr(comp$printfile[[1]],'postamble'))){
    postamble <- attr(comp$printfile[[1]],'postamble')
    
    pos <- grep('Regression', postamble)
    
    areas <- 
      gsub('Regression information for area ','',postamble[pos]) %>%
      cbind(areas=.,n=diff(c(pos,length(postamble)+1))-1) %>%
      as.data.frame() %>%
      split(.$areas) %>%
      purrr::map_df(function(x) data.frame(areas=rep(x$areas,x$n))) 
    
    regr.txt <- 
      postamble[-c(1:min(pos-1),pos)] %>%
      gsub('; ','',.) %>%
      paste(.,areas$areas)
    
    regr <- utils::read.table(text=regr.txt,stringsAsFactors = FALSE)[c(1,3,5,7,8)] %>% 
      tibble::as_tibble()
    names(regr) <- c('label','intercept','slope','sse','area')
    dat <- dat %>% 
      dplyr::left_join(regr, by = c('area','label')) %>% 
      tibble::as_tibble()
  }
  return(dat)
}




