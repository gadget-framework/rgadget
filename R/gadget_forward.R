gadget_project_time <- function(path='.', num_years = 100, variant_dir = 'PRE'){
  
  if(is.null(attributes(path)$mainfile)){
    print('Location of mainfile not specified, assuming "main"')
    attributes(path)$mainfile <- 'main'
  }
  
  project_path <- gadget.variant.dir(path, variant_dir = variant_dir)
  
  main <- read.gadget.file(path,attributes(path)$mainfile,file_type = 'main')
  main$likelihood$likelihoodfiles <- NULL
  attributes(main)$file_name <- 'main'
  main %>% write.gadget.file(project_path,recursive = FALSE)
  
  attributes(main[[1]]$timefile)$file_name %>% ## should read the raw 
    gadgettime(path) %>% 
    gadget_update(lastyear = .[[1]]$lastyear+num_years) %>% 
    write.gadget.file(project_path)
  
  ## create a timing schedule for the projections and save to file
 schedule <- 
   expand.grid(year = seq(main[[1]]$timefile[[1]]$lastyear,
                         by=1,
                         length.out = num_years+1),
              step = 1:main[[1]]$timefile[[1]]$notimesteps[1],
              area = main[[1]]$areafile[[1]]$areas) %>% 
    dplyr::filter(!(.data$year==main[[1]]$timefile[[1]]$lastyear & 
                      .data$step <= main[[1]]$timefile[[1]]$laststep),
                  !(.data$year==max(.data$year) & .data$step > main[[1]]$timefile[[1]]$laststep)) %>% 
    dplyr::arrange(.data$year,.data$step,.data$area)
 
 schedule %>% 
    readr::write_delim(sprintf('%s/.schedule',paste(project_path,attributes(project_path)$variant_dir,sep='/')))
  
  ## update the area file
  main[[1]]$areafile[[2]] <- 
    main[[1]]$areafile[[2]] %>%
    purrr::set_names(c('year', 'step', 'area', 'mean')) %>% 
    dplyr::bind_rows(schedule %>% 
                       dplyr::mutate(mean=3))   ## this bit could be user defined
  
  main[[1]]$areafile %>% 
    write.gadget.file(project_path) 
  
  return(project_path)
}

gadget_project_stocks <- function(path, imm.file, mat.file, spawn_func = 'hockeystick'){
  
  main <- read.gadget.file(path,attributes(path)$mainfile,file_type = 'main')
  
  imm_stock <- 
    gadgetstock(imm.file, path)
  
  mat_stock <- 
    gadgetstock(mat.file, path)
  
  
  rec_table <- 
    imm_stock$doesrenew %>% 
    purrr::pluck(intersect(names(.), c('normalparamfile','numberfile','normalcondfile'))) %>% 
    .[[1]] %>% 
    tibble::as_tibble()
  
  
  if(imm_stock[[1]]$minage > 0){
    hockey_stock <- 
      gadgetstock(paste('hockeystock',imm_stock[[1]]$stockname,sep='_'),path,missingOkay = TRUE) %>% 
      gadget_update('refweight',data=imm_stock[[1]]$refweightfile[[1]]) %>% 
      gadget_update('stock',
                    minage=0,
                    maxage=imm_stock[[1]]$minage,
                    minlength = imm_stock[[1]]$minlength,
                    maxlength = imm_stock[[1]]$maxlength,
                    dl = imm_stock[[1]]$dl,
                    livesonareas = imm_stock[[1]]$livesonareas) %>%
      gadget_update('naturalmortality',rep(0,.[[1]]$maxage+1)) %>% 
      gadget_update('doesgrow',0) %>% 
      ## to get multiannual recruitment you will need to define multiple hockeystocks or use straying (some other day)
      gadget_update('doesmove',
                    transitionstocksandratios = sprintf('%s 1',imm_stock[[1]]$stockname),
                    transitionstep = rec_table$step %>% unique() %>% utils::head(1)) %>% 
      gadget_update('initialconditions',
                    normalparam = tibble::tibble(age = .[[1]]$minage:.[[1]]$maxage,
                                                 area = .[[1]]$livesonareas,
                                                 age.factor = 0,   
                                                 area.factor = 0,
                                                 mean = .[[1]]$minlength,
                                                 stddev = 1,
                                                 alpha = 0.00001, ## never used 
                                                 beta = 3)) 
    
  } else {
    hockey_stock <- imm_stock
  }
  
  attr(hockey_stock,'file_config')$mainfile_overwrite = TRUE  
  hockey_stock %>% 
    write.gadget.file(path)  
  
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  
  
  ## set up time variable recruitment deviations
  gadgetfile(paste('hockeyrec',imm_stock[[1]]$stockname,sep='.'),
             file_type = 'timevariable',
             components=list(list(paste('hockeyrec',imm_stock[[1]]$stockname,sep='.'),
                                  data = 
                                    tibble::tibble(year = main[[1]]$timefile[[1]]$firstyear,
                                                   step = main[[1]]$timefile[[1]]$firststep,
                                                   value = "0") %>% 
                                    dplyr::bind_rows(expand.grid(year = unique(schedule$year),
                                                                 step = rec_table$step %>% unique()) %>% 
                                                       dplyr::mutate(value = paste0('#',imm_stock[[1]]$stockname,
                                                                                    '.rec.pre.',.data$year,'.',.data$step),
                                                                     year = .data$year - imm_stock[[1]]$minage)) %>% 
                                    dplyr::select(.data$year,.data$step,.data$value) %>%
                                    as.data.frame()))) %>% 
    write.gadget.file(path)
  
  
  #attr(mat_stock,'file_config')$mainfile_overwrite = TRUE
  mat_stock %>% 
    gadget_update('doesspawn',
                  spawnsteps = imm_stock$doesrenew$normalparamfile[[1]]$step %>% unique(), 
                  spawnareas = .[[1]]$livesonareas,
                  firstspawnyear = min(schedule$year)-imm_stock[[1]]$minage,
                  lastspawnyear = max(schedule$year),
                  spawnstocksandratios = list(hockey_stock[[1]]$stockname,1),
                  proportionfunction = 'constant 1',
                  mortalityfunction = 'constant 0',
                  weightlossfunction = 'constant 0',
                  recruitment = list(spawn_func = 'hockeystick',
                                     R = paste(attributes(path)$variant_dir,
                                               paste('hockeyrec',imm_stock[[1]]$stockname,sep='.'),sep='/'),
                                     Blim = sprintf('#%s.blim',mat_stock[[1]]$stockname)),
                  stockparameters =  rec_table %>% ## what about other types of recruitment files?
                    utils::tail(1) %>% 
                    dplyr::select(mean:beta)%>% ## this is very brittle
                    unlist() %>% 
                    purrr::map(to.gadget.formulae) %>% 
                    unlist()) %>% 
    write.gadget.file(path)
  
  imm_stock %>% 
    write.gadget.file(path)
  
  return(path)
}


gadget_project_fleets <- function(path, pre_fleet = 'comm',post_fix='pre',fleet_type='linearfleet') {
  
  #pre_fleets$base_fleet <- pre_fleets$base_fleet%||%pre_fleets$fleet_name
  #pre_fleets$fleet_type <- pre_fleets$fleet_type%||%'totalfleet'
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  main <- read.gadget.file(path,attributes(path)$mainfile,file_type = 'main')
  #pre.fleets <- data_frame(fleet_name = c('lln','bmt','gil'))
  ## collect the model fleets
  fleets <-
    main$fleet$fleetfiles %>% 
    purrr::map(~gadgetfleet(.,path))
  
  ### ugly as hell but works for now
  suits <- 
    fleets %>% 
    purrr::map(~purrr::keep(.,~.[[1]]==pre_fleet)) %>% 
    purrr::flatten() %>% 
    purrr::map('suitability') %>%
    purrr::flatten() %>%
    purrr::map(~purrr::map_if(.,is.call,to.gadget.formulae)) %>% 
    purrr::map(t) %>% 
    purrr::map(paste, collapse = '\t') %>% 
    dplyr::bind_rows(.id='fleet') %>% 
    tidyr::gather("stock","val",-c(1)) %>% 
    dplyr::select(-.data$fleet)
  
  ## check if the projection fleets are defined 
  tmp <- 
    fleets %>% 
    purrr::flatten() %>% 
    purrr::map(~.[[1]]) %>% 
    unlist() %>% 
    setdiff(pre_fleet,.) %>% 
    purrr::map(~stop(sprintf('Projection fleet %s not found',.)))
  
  ## define fleet amounts that are parametrised by year, step, area 
  fleet.amounts <- 
    schedule %>% 
    dplyr::mutate(number= paste('#fleet',pre_fleet,post_fix,.data$year,.data$step,.data$area,sep='.')) %>% 
    structure(area_group = main[[1]]$areafile[[1]]$areas %>% 
                purrr::set_names(.,.))
  
  ## define the projection fleets
  gadgetfleet('fleet.predict',path,missingOkay = TRUE) %>% 
    gadget_update(fleet_type,
                  name = paste0('predict.',pre_fleet),
                  suitability = 
                    suits %>% 
                    tidyr::unite(col,sep='\t') %>% 
                    unlist() %>% 
                    paste(collapse="\n") %>% 
                    sprintf('\n%s',.),
                  data = fleet.amounts) %>% 
    write.gadget.file(path)
  
  return(path)
}

gadget_project_recruitment <- function(path,
                                       stock,
                                       recruitment=NULL,
                                       n_replicates=100,
                                       params.file = 'PRE/params.pre'){
  
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  rec <- 
    recruitment %>% 
    #dplyr::filter(.data$stock == 'stock') %>% 
    dplyr::arrange(.data$year,.data$step) %>% 
    dplyr::group_by(.data$year) %>% 
    dplyr::summarise(recruitment = log(sum(.data$recruitment))) %>%
    tidyr::nest(data = tidyr::everything()) %>% 
    dplyr::mutate(model = purrr::map(data,~stats::lm(utils::head(recruitment,-1)~utils::tail(recruitment,-1),.)),
                  variables = purrr::map(model,~broom::tidy(.) %>% 
                                           dplyr::select(.,.data$term,.data$estimate) %>% 
                                           tidyr::spread(.,"term","estimate")),
                  glances = purrr::map(model,broom::glance)) %>% 
    dplyr::select(-c(.data$data,.data$model)) %>% 
    tidyr::unnest(cols=dplyr::everything()) %>% 
    dplyr::select(a=1,b=2,.data$sigma) %>%
    dplyr::mutate(by=1) %>% 
    dplyr::left_join(tidyr::expand_grid(year=unique(schedule$year),
                                        by=1,
                                        trial=1:n_replicates),
                     by = "by") %>% 
    dplyr::select(-.data$by) %>%
    dplyr::arrange(.data$year,.data$trial) %>% 
    dplyr::mutate(recruitment = stats::arima.sim(dplyr::n(),
                                                 model=list(ar=unique(.data$b)),
                                                 sd=unique(.data$sigma)) + unique(.data$a),
                  recruitment = exp(recruitment)) %>% 
    
    dplyr::mutate(year = sprintf('%s.rec.%s.1',.data$stock,.data$year)) %>% 
    tidyr::spread("year","recruitment") %>%
    dplyr::select(-c('a','b','sigma','trial'))
    
  
    read.gadget.parameters(file = params.file) %>% 
    wide_parameters(value=rec) %>% 
    write.gadget.parameters(file=params.file)
  
    return(path)
}

gadget_project_advice <- function(path,
                                  params.file = 'PRE/params.pre',
                                  harvest_rate = 0.2, 
                                  advice_cv = 0.2, 
                                  advice_rho = 0.6,
                                  pre_fleet='comm',
                                  post_fix = 'pre', 
                                  n_replicates = 100){
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  fleet_parameters <- 
    purrr::map(1:n_replicates,
               function(x)
                 schedule %>% 
                 dplyr::mutate(name = paste('fleet',pre_fleet,post_fix,
                                            .data$year,.data$step,.data$area,
                                            sep = '.'),
                               replicate = x)) %>% 
    dplyr::bind_rows() %>% 
    dplyr::left_join(tidyr::expand_grid(replicate=1:n_replicates,
                                        value = harvest_rate),
                     by = 'replicate')
              
  if(advice_cv > 0){
    fleet_parameters <- 
      fleet_parameters %>% 
      dplyr::mutate(value = value * exp(stats::arima.sim(n = dplyr::n(),
                                                         list(ar=advice_rho),
                                                         sd = advice_cv))) ## bias correction?
  } 
  
  
  
  params <- 
    read.gadget.parameters(params.file) %>% 
    wide_parameters(value=fleet_parameters %>%
                      dplyr::select(-c('year','step','area')) %>% 
                      tidyr::spread('name','value')) %>% 
    write.gadget.parameters(params.file)
  
  return(path)
  
}




if(FALSE){
  fit <- gadget.fit()
  
  gadget_project_time() %>% 
    gadget_project_stocks(imm.file = 'Modelfiles/cod.imm',mat.file = 'Modelfiles/cod.mat') %>% 
    gadget_project_fleets()
  
  
  callGadget(s=1,main = 'PRE/main',i='WGTS/params.final',p='PRE/params.pre',log='tmp')
  
  read.gadget.parameters('PRE/params.pre') %>% 
    init_guess('fleet.+',0.2) %>% 
    init_guess('codimm.rec.+',)
  
}