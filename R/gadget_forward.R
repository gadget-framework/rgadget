#' Gadget projection function
#' 
#' Setup model and parameter files for forward simulations and deterministic projections. 
#' 
#' gadget project recruitment:
#' Sets up the process error for the stock recruitment relationship.
#' The user has a choice of three types, constant, AR and block bootstrap
#' based on a time-series of recruitment
#' 
#' gadget project stocks:
#' Here the user can define the SSB - Rec relationship used in the 
#' projections. It defines link between two stocks, immature and 
#' mature stock and in the case of minage (immstock) > 0 an interim 
#' bookeeping stock from age 0 to minage. 
#' 
#' Limitiations:
#' At present theres is no way to define recruitment into multiple 
#' stocks (ie. via multiple interim stocks). 
#' This function also assumes that there is no SSB - Rec relationship, 
#' since if it is already present the simulation should use that.
#' Multi-annual recruitment is currently not defined. 
#' 
#' @rdname gadget_projections
#' @param path gadget model directory
#' @param num_years number of years 
#' @param variant_dir location of the 
#'
#' @return gadget variant directory 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' fit <- gadget.fit()
#'
#' res <- 
#'  gadget_project_time() %>% 
#'  gadget_project_stocks(imm.file = 'Modelfiles/cod.imm',mat.file = 'Modelfiles/cod.mat') %>% 
#'  gadget_project_fleet(pre_fleet = 'comm') %>% 
#'  gadget_evaluate(params.out = paste(attr(.,'variant_dir'),'params.pre',sep='/'),
#'                  params.in = 'WGTS/params.final') %>% 
#'  gadget_project_recruitment(stock = 'codimm', 
#'                             recruitment = fit$stock.recruitment %>% 
#'                               filter(stock == 'codimm',
#'                                      year > 1980),
#'                             params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>% 
#'  gadget_project_ref_points(ref_points = tibble(codmat.blim = 207727665), 
#'                          params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>% 
#'  gadget_project_advice(pre_fleet = 'comm',
#'                        harvest_rate = 1:100/100, 
#'                        params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>% 
#'  gadget_project_output(imm.file = 'Modelfiles/cod.imm',mat.file = 'Modelfiles/cod.mat',
#'                        pre_fleet = 'comm') %>% 
#'  gadget_evaluate(params.in = paste(attr(.,'variant_dir'),'params.pre',sep='/'))  %>% 
#'  {read.printfiles(paste(attr(.,'variant_dir'),'out',sep='/'))} %>% 
#'  map(mutate, trial=cut(1:length(year),c(0,which(diff(year)<0),1e9),labels = FALSE)) %>% 
#'  set_names(c("catch.F","catch.lw",'codimm.rec','codmat.ssb')) %>% 
#'  map(left_join,tibble(trial=1:10000,harvest_rate = rep(1:100/100,100)))
#'
#'
#' yield_curve <- 
#'  res$catch.lw %>% 
#'  filter(year>2050) %>% 
#'  group_by(trial,harvest_rate,year) %>% 
#'  summarise(c=sum(biomass_consumed)/1e6) %>% 
#'  group_by(harvest_rate) %>% 
#'  summarise(m=median(c),u=quantile(c,0.95),l=quantile(c,0.05)) 
#'
#'
#'
#' ssb_curve <- 
#'  res$codmat.ssb %>% 
#'  filter(year>2050) %>% 
#'  group_by(trial,harvest_rate,year) %>% 
#'  summarise(c=sum(number*mean_weight)/1e6) %>% 
#'  group_by(harvest_rate) %>% 
#'  summarise(m=median(c),u=quantile(c,0.95),l=quantile(c,0.05))
#'
#' f.curve <- 
#'  res$catch.F %>% 
#'  filter(year>2050) %>% 
#'  group_by(trial,harvest_rate,year) %>% 
#'  summarise(c=median(mortality)) %>% 
#'  group_by(harvest_rate) %>% 
#'  summarise(m=median(c),u=quantile(c,0.95),l=quantile(c,0.05)) 
#'
#'
#' blim <- 
#'  fit$res.by.year %>% 
#'  filter(grepl('mat',stock)) %>% 
#'  summarise(b=min(total.biomass)/1e6) %>% 
#'  .$b
#'
#' bpa <- 1.4*blim
#'
#' hr_msy <- 
#'  yield_curve %>% filter(m==max(m)) %>% .$harvest_rate
#'
#' hr_lim <- 
#'  ssb_curve %>% 
#'  filter(m>blim) %>% 
#'  filter(harvest_rate == max(harvest_rate)) %>% 
#'  .$harvest_rate
#'
#'
#' f.msy <-
#'  f.curve %>% 
#'  filter(harvest_rate == hr_msy) %>% 
#'  .$m
#'
#' f.lim <- 
#'  f.curve %>% 
#'  filter(harvest_rate == hr_lim) %>% 
#'  .$m
#'
#' f.pa <- 
#'  f.lim/1.4
#'
#' hr_pa <- 
#'  f.curve %>% 
#'  filter(m < f.pa) %>% 
#'  summarise(hr = max(harvest_rate)) %>%
#'  .$hr
#'
#'
#' library(patchwork)
#'
#' yield_curve %>% 
#'  left_join(f.curve %>% 
#'              select(harvest_rate,F=m)) %>% 
#'  ggplot(aes(F,m)) +
#'  geom_ribbon(aes(ymin=l,ymax=u),fill = 'gold') +  
#'  geom_line() + 
#'  geom_vline(xintercept = min(c(f.msy,f.pa))) + 
#'  geom_vline(xintercept = f.pa,lty=2,col='red') + 
#'  geom_vline(xintercept = f.lim,lwd=1.1,col='red') +
#'  ssb_curve %>% 
#'  left_join(f.curve %>% 
#'              select(harvest_rate,F=m)) %>% 
#'  ggplot(aes(F,m)) + 
#'  geom_ribbon(aes(ymin=l,ymax=u),fill = 'gold') + 
#'  geom_line() + 
#'  geom_vline(xintercept = min(c(f.msy,f.pa))) + 
#'  geom_vline(xintercept = f.pa,lty=2,col='red') + 
#'  geom_vline(xintercept = f.lim,lwd=1.1,col='red') + 
#'  geom_hline(yintercept = blim, col = 'red', lwd = 1.1) + 
#'  geom_hline(yintercept = bpa,col = 'red',lty = 2)
#'  
#'  
#'  ## run prognosis
#'  
#' res <- 
#'  gadget_project_time(num_years = 5, variant_dir = 'PRG') %>% 
#'  gadget_project_stocks(imm.file = 'Modelfiles/cod.imm',mat.file = 'Modelfiles/cod.mat') %>% 
#'  gadget_project_fleet(pre_fleet = 'comm') %>% 
#'  gadget_evaluate(params.out = paste(attr(.,'variant_dir'),'params.pre',sep='/'),
#'                  params.in = 'WGTS/params.final') %>% 
#'  gadget_project_recruitment(stock = 'codimm', 
#'                             recruitment = fit$stock.recruitment %>% 
#'                               filter(stock == 'codimm',
#'                                      year > 1980),
#'                             method = 'constant',
#'                             n_replicates = 1,          
#'                             params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>% 
#'  gadget_project_ref_points(ref_points = tibble(codmat.blim = 207727665), 
#'                          params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>% 
#'  gadget_project_advice(pre_fleet = 'comm',
#'                        harvest_rate = hr_msy, 
#'                        params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/'),
#'                        n_replicates = 1, 
#'                        advice_cv = 0) %>% 
#'  gadget_project_output(imm.file = 'Modelfiles/cod.imm',mat.file = 'Modelfiles/cod.mat',
#'                        pre_fleet = 'comm') %>% 
#'  gadget_evaluate(params.in = paste(attr(.,'variant_dir'),'params.pre',sep='/'))  %>% 
#'  {read.printfiles(paste(attr(.,'variant_dir'),'out',sep='/'))} %>% 
#'  set_names(c("catch.F","catch",'rec','ssb')) 
#'  
#'  
#'  }
#'  
#'  
gadget_project_time <- function(path='.', num_years = 100, 
                                variant_dir = getwd() %>% stringr::str_count('/') %>% 
                                  rep('../',.) %>% paste(collapse = '') %>% paste(tempdir(),sep='')){
  
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
    readr::write_delim(., sprintf('%s/.schedule',paste(project_path,attributes(project_path)$variant_dir,sep='/')))
  
  ## update the area file
  gadgetfile('Modelfiles/area',
             file_type = 'area',
             components = list(list(areas = main[[1]]$areafile[[1]]$areas,
                                    size = main[[1]]$areafile[[1]]$size,
                                    temperature = main[[1]]$areafile %>% 
                                      utils::capture.output() %>% 
                                      readr::read_lines(.) %>% 
                                      .[!grepl('^;',.)] %>% 
                                      readr::read_table2(.,skip = 3, comment = ';', col_names = c('year','step','area','temperature')) %>% 
                                      dplyr::bind_rows(schedule %>% 
                                                         dplyr::mutate(temperature=3))))) %>% ## this is a mess
    write.gadget.file(project_path)

  return(project_path)
}



#' @rdname gadget_projections
#' 
#' @param imm.file location of the immature stock file
#' @param mat.file location of the mature stock file
#' @param proportionfunction proportion suitability
#' @param mortalityfunction mortataliy suitability
#' @param weightlossfunction weightloss suitability
#' @param spawnfunction what spawn function to use (only hockeystick atm)
#'
#' @export
gadget_project_stocks <- function(path, 
                                  imm.file, 
                                  mat.file, 
                                  spawnfunction = 'hockeystick',
                                  proportionfunction = 'constant 1',
                                  mortalityfunction = 'constant 0',
                                  weightlossfunction = 'constant 0'){
  
  main <- read.gadget.file(path,attributes(path)$mainfile,file_type = 'main')
  
  imm_stock <- 
    gadgetstock(imm.file, path)
  
  mat_stock <- 
    gadgetstock(mat.file, path)
  
  
  rec_table <- 
    imm_stock$doesrenew %>% 
    purrr::pluck(intersect(names(.), 
                           c('normalparamfile','numberfile','normalcondfile'))) %>% 
    .[[1]] %>% 
    tibble::as_tibble()
  
  
  if(imm_stock[[1]]$minage > 0){
    hockey_stock <- 
      gadgetstock(paste('hockeystock',imm_stock[[1]]$stockname,sep = '_'),path,missingOkay = TRUE) %>% 
      gadget_update('refweight',data = imm_stock[[1]]$refweightfile[[1]]) %>% 
      gadget_update('stock',
                    minage = 0,
                    maxage = imm_stock[[1]]$minage,
                    minlength = imm_stock[[1]]$minlength,
                    maxlength = imm_stock[[1]]$maxlength,
                    dl = imm_stock[[1]]$dl,
                    livesonareas = imm_stock[[1]]$livesonareas) %>%
      gadget_update('naturalmortality',rep(0,.[[1]]$maxage + 1)) %>% 
      gadget_update('doesgrow',0) %>% 
      ## to get multiannual recruitment you will need to define multiple hockeystocks or use straying (some other day)
      gadget_update('doesmove',
                    transitionstocksandratios = sprintf('%s 1',imm_stock[[1]]$stockname),
                    transitionstep = rec_table$step %>% unique() %>% utils::head(1)) %>% 
      gadget_update('initialconditions',
                    normalparam = tidyr::expand_grid(age = .[[1]]$minage:.[[1]]$maxage,
                                                     area = .[[1]]$livesonareas,
                                                     age.factor = 0,   
                                                     area.factor = 0,
                                                     mean = .[[1]]$minlength,
                                                     stddev = 1,
                                                     alpha = 0.00001, ## never used 
                                                     beta = 3) %>% 
                      dplyr::arrange(.data$area,.data$age)) 
    
  } else {
    hockey_stock <- imm_stock
  }
  
  hockey_stock %>% 
    write.gadget.file(path)  
  
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep = '/')),
                      delim = ' ')
  
  
  
  ## set up time variable recruitment deviations
  gadgetfile(paste('hockeyrec',imm_stock[[1]]$stockname,sep = '.'),
             file_type = 'timevariable',
             components = list(list(paste('hockeyrec',imm_stock[[1]]$stockname,sep = '.'),
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
  
  
  imm_stock %>% 
    write.gadget.file(path)
 ## if the stock already has a SSB - Rec relationship defined we don't need to run this funcion
  mat_stock %>% 
    gadget_update('doesspawn',
                  spawnsteps = imm_stock$doesrenew$normalparamfile[[1]]$step %>% unique(), 
                  spawnareas = .[[1]]$livesonareas,
                  firstspawnyear = min(schedule$year) - imm_stock[[1]]$minage,
                  lastspawnyear = max(schedule$year),
                  spawnstocksandratios = list(hockey_stock[[1]]$stockname,1),
                  proportionfunction = proportionfunction,
                  mortalityfunction = mortalityfunction,
                  weightlossfunction = weightlossfunction,
                  recruitment = list(spawnfunction = spawnfunction,
                                     R = paste(attributes(path)$variant_dir,
                                               paste('hockeyrec', imm_stock[[1]]$stockname,sep = '.'), sep = '/'),
                                     Blim = sprintf('#%s.blim', mat_stock[[1]]$stockname)),
                  stockparameters =  rec_table %>% ## what about other types of recruitment files?
                    utils::tail(1) %>% 
                    dplyr::select(mean:beta) %>% ## this is very brittle
                    unlist() %>% 
                    purrr::map(to.gadget.formulae) %>% 
                    unlist()) %>% 
    write.gadget.file(path)
  
  
  return(path)
}


#' @rdname gadget_projections
#'
#' @param pre_fleet name of the fleet projections are based on (original fleet). Only a single fleet should be implemented here at a time. Multiple fleets require multiple gadget_project_fleet calls.
#' @param post_fix A single label (vector of length 1) used to distinguish the original fleet and the projection fleet (with same parameterisations). Should be the same for all projection fleets.
#' @param fleet_type type of gadget fleet for the projections
#' @param common_mult a string with a base name for a vector of harvest/catch rate multipliers (shared between fleets)
#' @param pre_proportion proportion of the effort taken
#' @param path gadget variant directory
#' @param type type of projection, either "standard" or "prognosis"
#' @param ... addition input to the fleet files
#'
#' @export
gadget_project_fleet <- function(path, pre_fleet = 'comm',
                                  post_fix='pre',
                                  fleet_type='linearfleet',
                                  common_mult = NULL,
                                  pre_proportion = NULL,
                                  type = 'standard',
                                  ...) {
  
  
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  main <- read.gadget.file(path,attributes(path)$mainfile,file_type = 'main')
  #pre.fleets <- tibble::tibble(fleet_name = c('lln','bmt','gil'))
  ## collect the model fleets
  fleets <-
    main$fleet$fleetfiles %>% 
    purrr::map(~gadgetfleet(.,path))

  ## check if the projection fleets are defined 
  fleets %>% 
    purrr::flatten(.x = .) %>% 
    purrr::map(~.[[1]]) %>% 
    unlist() %>% 
    setdiff(pre_fleet,.) %>% 
    purrr::map(~stop(sprintf('Projection fleet %s not found',.)))
  
    
  ### ugly as hell but works for now
  suits <- 
    fleets %>% 
    purrr::map(~purrr::keep(.,~.[[1]]==pre_fleet)) %>% 
    purrr::flatten(.x = .) %>% 
    purrr::map('suitability') %>%
    purrr::flatten(.x = .) %>% 
    purrr::map(~purrr::map_if(.,is.call,to.gadget.formulae)) %>% 
    purrr::map(~paste(.,collapse = '\t')) %>% 
    unlist() %>% 
    paste(names(.),.,sep='\t')
  
  
  ## define fleet amounts that are parametrised by year, step, area 
  if(is.null(common_mult)){
    common_mult <- '#fleet.%s.%s.%s.%s.%s'
  } else if(type == 'prognosis') {
    ## advice error is defined via prognosis likelihood
    common_mult <- '1' 
    fleet_type <- 'totalfleet'
  } else {
    if(is.null(pre_proportion)) pre_proportion <- 1
    common_mult <- paste('(*', pre_proportion, paste0('#fleet.',common_mult, '.%3$s.%4$s.%5$s'), ')')
  }

  
  
  
  fleet.amounts <- 
    schedule %>% 
    dplyr::mutate(number = sprintf(common_mult,pre_fleet,post_fix,.data$year,.data$step,.data$area)) %>% 
    structure(area_group = main[[1]]$areafile[[1]]$areas %>% 
                purrr::set_names(.,.))
  
  ## define the projection fleets
  gadgetfleet('fleet.predict',path,missingOkay = TRUE) %>% 
    gadget_update(fleet_type,
                  name = paste(pre_fleet,post_fix,sep='.'),
                  suitability = 
                    suits %>% 
                    paste(collapse="\n") %>% 
                    sprintf('\n%s',.),
                  ...,
                  data = fleet.amounts) %>% 
    write.gadget.file(path)
  
  return(path)
}


#' @rdname gadget_projections
#'
#' @param stocks names of of the stocks
#' @param pre_fleets vector of fleets on which the projections are based
#' @param post_fix A single label (vector of length 1) used to distinguish the original fleets and the projection fleets (with same parameterisations). Should be the same for all projection fleets.
#' @param path gadget variant director
#' @param mainfile_overwrite should the likelihood be overwritten, defaults to TRUE, but should be set as FALSE for multiple prognosis components (different groups of fleets)
#' @param firsttacyear when does the TAC scheme start
#' @param assessmentstep when is the assessment
#' @param weightoflastyearstac running average TAC..
#' @param ... additional input to the prognosis likelihood component
#'
#' @export
gadget_project_prognosis_likelihood <- function(path,
                                                stocks,
                                                pre_fleets = 'comm',
                                                post_fix = 'pre',
                                                mainfile_overwrite = TRUE,
                                                firsttacyear = NULL,
                                                assessmentstep = 2,
                                                weightoflastyearstac = 0,
                                                ...){
  
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  main <- read.gadget.file(path,attributes(path)$mainfile,file_type = 'main')
  
  
  
  pre.fleet.names <- 
    paste(pre_fleets, post_fix, sep = '.')
  
  fleet_label <- 
    paste0(paste(pre_fleets, collapse = '.'), '.', post_fix)
  
  progn <- 
    gadgetfile('fleet.likelihood',
               file_type = 'likelihood',
               components = list(list('[component]',
                                      name = 'prognosis',
                                      weight = 1,
                                      type = "proglikelihood",
                                      fleetnames = pre.fleet.names,
                                      stocknames = stocks,
                                      ...,
                                      fleetproportions= sprintf('#fleet.prop.%s',pre.fleet.names),
                                      weightoflastyearstac = weightoflastyearstac,
                                      maxchange=4,
                                      functionnumber=1,
                                      firsttacyear = if(is.null(firsttacyear)) min(schedule$year) else firsttacyear,
                                      assessmentstep = assessmentstep,
                                      asserr = gadgetfile(paste('asserr',fleet_label,'timevar',sep = '.'),
                                                          file_type = 'timevariable',
                                                          components=list(list('asserr',
                                                                               data = tibble::tibble(year = main[[1]]$timefile[[1]]$firstyear,
                                                                                                 step = main[[1]]$timefile[[1]]$firststep,
                                                                                                 value = "0") %>% 
                                                                                 dplyr::bind_rows(expand.grid(year = unique(schedule$year),
                                                                                                       step = assessmentstep) %>% 
                                                                                             dplyr::mutate(value = paste0('1#','asserr.',fleet_label,'.',.data$year,'.',.data$step,'.1'))) %>% 
                                                                                 as.data.frame()))),
                                      implerr = gadgetfile(paste('implerr',fleet_label,'timevar',sep = '.'),
                                                           file_type = 'timevariable',
                                                           components=list(list('implerr',
                                                                                data = tibble::tibble(year = main[[1]]$timefile[[1]]$firstyear,
                                                                                                  step = main[[1]]$timefile[[1]]$firststep,
                                                                                                  value = "0") %>% 
                                                                                  dplyr::bind_rows(expand.grid(year = unique(schedule$year),
                                                                                                        step = assessmentstep) %>% 
                                                                                              dplyr::mutate(value = paste0('1#','implerr.',fleet_label,'.',.data$year,'.',.data$step,'.1'))) %>% 
                                                                                  as.data.frame())))))) 
  
  attr(progn,'file_config')$mainfile_overwrite = mainfile_overwrite
  progn %>% 
    write.gadget.file(path)
  
  return(path)
}

#' @rdname gadget_projections
#' 
#' @param stock name of immature stock
#' @param recruitment recruitment time series
#' @param n_replicates number of simulations
#' @param params.file name of the parameter files
#' @param method prediction method, built in options are 'AR', 'bootstrap' or constant
#' @param ... additional arguments for the 
#' @export
gadget_project_recruitment <- function(path,
                                       stock,
                                       recruitment=NULL,
                                       n_replicates=100,
                                       params.file = 'PRE/params.pre',
                                       method = 'AR', 
                                       ...){
  
  schedule <- 
    readr::read_delim(sprintf('%s/.schedule',
                              paste(path,attributes(path)$variant_dir,sep='/')),
                      delim = ' ')
  
  rec_step <- unique(recruitment$step)[1]
  
  if(!('model' %in% names(recruitment))){
    recruitment$model <- 1
  } 
  
  ## remove unwanted columns and sum
  recruitment <- 
    recruitment %>% 
    dplyr::group_by(.data$model,.data$year, .data$step) %>% 
    dplyr::summarise(recruitment = sum(.data$recruitment)) %>% 
    dplyr::arrange(.data$model,.data$year)
 
  
  if(method == 'AR'){
    rec <- 
      recruitment %>% 
      split(.$model) %>% 
      purrr::map(gadget_project_rec_arima,schedule=schedule,n_replicates=n_replicates,...) %>% 
      dplyr::bind_rows(.id="model") 
    
  } else if(method == 'bootstrap'){
    rec <- 
      recruitment %>% 
      split(.$model) %>% 
      purrr::map(gadget_project_rec_bootstrap,schedule=schedule,n_replicates=n_replicates,...) %>% 
      dplyr::bind_rows(.id="model")
    
  } else if(method == 'constant'){
    rec <- 
      recruitment %>% 
      split(.$model) %>% 
      purrr::map(gadget_project_rec_constant,schedule=schedule) %>% 
      dplyr::bind_rows(.id="model")
    
  }else {
    stop('Method not valid, expected constant, AR or bootstrap')
  }
  
  rec <- 
    rec %>% 
    dplyr::mutate(year = sprintf('%s.rec.pre.%s.%s', stock, .data$year, rec_step),
                  rec = .data$rec/1e4) %>% 
    tidyr::pivot_wider(., id_cols = c(.data$trial,.data$model), names_from = .data$year, values_from = .data$rec) %>% 
    dplyr::select(-c('trial','model'))
  
  read.gadget.parameters(file = paste(path, params.file, sep = '/')) %>% 
    wide_parameters(value = rec) %>% 
    write.gadget.parameters(file = paste(path, params.file, sep = '/'))
  
  
  return(path)
}

gadget_project_rec_arima <- function(recruitment,schedule,n_replicates){
  
  mrec <- mean(recruitment$recruitment)
  
  recruitment %>% 
    dplyr::mutate(recruitment = log(recruitment)) %>% 
    stats::lm(head(recruitment,-1)~utils::tail(recruitment,-1),data = .) %>% 
    {list(variables=broom::tidy(.) %>% 
            {tibble::tibble(a = .$estimate[1],
                            b = .$estimate[2])},
          sigma=broom::glance(.) %>% 
            dplyr::select(.data$sigma))} %>%
    dplyr::bind_cols() %>% 
    dplyr::slice(rep(1,length(unique(schedule$year))*n_replicates)) %>% 
    dplyr::bind_cols(tidyr::expand_grid(year=unique(schedule$year),
                                        trial=1:n_replicates)) %>% 
    dplyr::mutate(rec = stats::arima.sim(dplyr::n(),
                                         model=list(ar=unique(.data$b)),
                                         sd=unique(.data$sigma)),
                  rec = mrec*exp(.data$rec)) %>% 
    dplyr::select('trial','year','rec')
  
}

gadget_project_rec_bootstrap <- function(recruitment,schedule,n_replicates,block_size = 7) {
  schedule %>% 
    dplyr::select(.data$year,.data$step) %>% 
    dplyr::distinct() %>% 
    dplyr::filter(.data$step %in% unique(recruitment$step)[1]) %>% 
    dplyr::slice(rep(1:dplyr::n(),n_replicates)) %>%  
    dplyr::mutate(trial = cut(1:length(.data$year),c(0,which(diff(.data$year)<0),1e9),labels = FALSE),
                  rec = tseries::tsbootstrap(recruitment$recruitment,type='block',
                                             nb=dplyr::n(),b=block_size) %>% as.numeric() %>% .[1:dplyr::n()]) %>% 
    dplyr::select('trial','year','rec')
}

gadget_project_rec_constant <- function(recruitment,schedule){
  schedule %>% 
    dplyr::select(.data$year,.data$step) %>% 
    dplyr::distinct() %>% 
    dplyr::filter(.data$step %in% unique(recruitment$step)[1]) %>% 
    dplyr::mutate(trial = 1,
                  rec = exp(mean(log(recruitment$recruitment)))) %>% 
    dplyr::select('trial','year','rec')
}

#' @rdname gadget_projections 
#' @param harvest_rate median harvest rate
#' @param advice_cv assessment error cv
#' @param advice_rho assessment error correlation
#' @param pre_fleet name of the fleet projections are based on (original fleet). Only a single fleet should be implemented here at a time. Multiple fleets require multiple gadget_project_advice calls.
#' @param post_fix A single label (vector of length 1) used to distinguish the original fleet and the projection fleet (with same parameterisations). Should be the same for all projection fleets.
#' @export
gadget_project_advice <- function(path,
                                  params.file = 'PRE/params.pre',
                                  harvest_rate = 0.2, 
                                  advice_cv = 0.2, 
                                  advice_rho = 0.6,
                                  pre_fleet = 'comm',
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
                                        value = harvest_rate) %>% 
                       dplyr::mutate(iter = 1:dplyr::n()),
                     by = 'replicate')
  
  if(advice_cv > 0){
    
    
    advice_mult <- 
      fleet_parameters %>% 
      dplyr::select(.data$year) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(mult = exp(stats::arima.sim(n = dplyr::n(),
                                                list(ar = advice_rho),
                                                sd = advice_cv)))
    fleet_parameters <- 
      fleet_parameters %>% 
      dplyr::left_join(advice_mult, by = 'year') %>% 
      dplyr::mutate(value = .data$value * .data$mult) %>%  ## bias correction?
      dplyr::select(-.data$mult)
  } 
  
  params <- 
    read.gadget.parameters(paste(path, params.file, sep = '/')) %>% 
    wide_parameters() %>% 
    dplyr::slice(rep(1:dplyr::n(),each=length(harvest_rate)*n_replicates/dplyr::n())) %>% 
    wide_parameters(value=fleet_parameters %>%
                      dplyr::select(-c('year','step','area')) %>% 
                      tidyr::spread(.,'name','value') %>% 
                      dplyr::select(-c("replicate","iter"))) %>% 
    write.gadget.parameters(paste(path, params.file, sep = '/'))
  
  return(path)
  
}



#' @rdname gadget_projections
#' @param ref_points tibble with reference points
#' @export
gadget_project_ref_points <- function(path,ref_points,params.file='PRE/params.pre'){
  params <- read.gadget.parameters(paste(path, params.file, sep = '/'))
  
  ref_points %>% 
    split(1:nrow(ref_points)) %>% 
    purrr::map(function(x){
      params %>% 
        wide_parameters(x %>% dplyr::slice(rep(1,nrow(params))))
      }) %>% 
    dplyr::bind_rows() %>% 
    structure(file_format='wide') %>% 
    write.gadget.parameters(paste(path, params.file, sep = '/'))
  
  return(path)
}



#' @rdname gadget_projections
#' @param pre_fleets vector of fleets on which the projections are based
#' @param f_age_range F age range, specified in the format a1:a2
#' @param rec_age age of recruitment (as reported)
#' @param print_block file suffix for the printfile
#' @export
gadget_project_output <- function(path, imm.file, mat.file,
                                  pre_fleets = 'comm', 
                                  post_fix = 'pre',
                                  f_age_range = NULL,
                                  rec_age = NULL,
                                  print_block = '1'){
  
  pre.fleet.names <- paste(pre_fleets, post_fix, sep = '.')
  
  imm_stock <- 
    gadgetstock(imm.file, path)
  
  mat_stock <- 
    gadgetstock(mat.file, path)
  
  
  print <- 
    gadgetprintfile(sprintf('pre.print.%s',print_block),path,missingOkay = TRUE) %>% 
    gadget_update('stockprinter',
                  stocknames = mat_stock[[1]]$stockname,
                  area = mat_stock[[1]]$livesonareas %>% purrr::set_names(.,paste0('area',.)) %>% as.list(),
                  age = list(allages = mat_stock[[1]]$minage:mat_stock[[1]]$maxage),
                  len = list(alllen = c(mat_stock[[1]]$minlength,mat_stock[[1]]$maxlength)),
                  printfile = sprintf('%s.ssb',mat_stock[[1]]$stockname),
                  printatstart = 1,
                  yearsandsteps = 'all 1') %>% 
    gadget_update('stockprinter',
                  stocknames = imm_stock[[1]]$stockname,
                  area = imm_stock[[1]]$livesonareas %>% purrr::set_names(.,paste0('area',.)) %>% as.list(),
                  age = list(rec_age = if(is.null(rec_age)) imm_stock[[1]]$minage else rec_age),
                  len = list(alllen = c(imm_stock[[1]]$minlength,imm_stock[[1]]$maxlength)),
                  printfile = sprintf('%s.rec',imm_stock[[1]]$stockname),
                  printatstart = 1,
                  yearsandsteps = 'all all') %>% 
    gadget_update('predatorpreyprinter',
                  predatornames = pre.fleet.names,
                  preynames = unique(c(imm_stock[[1]]$stockname,mat_stock[[1]]$stockname)),
                  area = c(imm_stock[[1]]$livesonareas,mat_stock[[1]]$livesonareas) %>% unique() %>% 
                    purrr::set_names(.,paste0('area',.)) %>% as.list(),
                  age = list(allages = imm_stock[[1]]$minage:mat_stock[[1]]$maxage),
                  len = list(alllen = c(imm_stock[[1]]$minlength,mat_stock[[1]]$maxlength)),
                  printfile = sprintf('%s.%s.catch.lw',imm_stock[[1]]$stockname,mat_stock[[1]]$stockname),
                  yearsandsteps = 'all all') %>% 
    gadget_update('predatorpreyprinter',
                  predatornames = pre.fleet.names,
                  preynames = unique(c(imm_stock[[1]]$stockname,mat_stock[[1]]$stockname)),
                  area = c(imm_stock[[1]]$livesonareas,mat_stock[[1]]$livesonareas) %>% unique() %>% 
                    purrr::set_names(.,paste0('area',.)) %>% as.list(),
                  age = list(Fages =  if(is.null(f_age_range)) mat_stock[[1]]$maxage else paste(f_age_range, collapse = ' ')),
                  len = list(alllen = c(imm_stock[[1]]$minlength,mat_stock[[1]]$maxlength)),
                  printfile = sprintf('%s.%s.catch.F',imm_stock[[1]]$stockname,mat_stock[[1]]$stockname),
                  yearsandsteps = 'all all')
  write.gadget.file(print, path)
  return(path)
}

