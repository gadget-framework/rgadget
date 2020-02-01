
#' Gadget forward 
#'
#' This function implements a crude forward simulation for a Gadget model.
#' NOTE: the function currently assumes at least one recruiting stock. 
#' -- details to come--
#'
#' @param years Number of years to predictc
#' @param params.file parameter file to base the prediction upon
#' @param main.file main file for the model
#' @param num.trials number of projection repliactes
#' @param fleets data frame with at least two columns, fleet and ratio, 
#' which are the names of the fleets and the ratio of the harvestable biomass they consume.
#' @param effort proportion of the harvestable biomass taken per year. Note that this relates to 
#' fishing mortality of fully recruited through the relation F=-log(1-E)
#' @param temperature (optional) data.frame containing temperature by year, step and area in the projections
#' @param rec.scalar scaling schedule for recruitment going forward. Data frame with year, stock and rec.scalar
#' @param check.previous Should previous results be loaded? Defaults to FALSE
#' @param save.results Should the results be saved? Defaults to TRUE
#' @param stochastic Should the projection be stochastic (default) or deterministic (assuming the average three year recruitment)?
#' @param rec.window What timeperiod should be used to estimate the distribution of recruits.
#' @param gd gadget directory
#' @param custom.print filename of customise printfile
#' @param method what method should be used to generate the projected recruitment, defaults to AR1
#' @param ref.years what years should be used as the basis for the projections
#' @param prj.func if method is "custom" then the user can supply a function that generates the recruitment
#' @param ... passed to prj.func
#' @importFrom rlang .data
#' @return list of simulation results
#' @export
gadget.forward <- function(years = 20,params.file = 'params.out',
                           main.file = 'main', num.trials = 10,
                           fleets = data.frame(fleet='comm',ratio = 1),
                           effort = 0.2,
                           temperature = NULL,
                           rec.scalar = NULL,
                           check.previous = FALSE,
                           save.results = TRUE,
                           stochastic = TRUE,
                           rec.window = NULL,
                           gd=list(dir='.',rel.dir='PRE'),
                           method = 'AR1',
                           ref.years=NULL,
                           custom.print=NULL,
                           prj.func = NULL,
                           ...){
  ## helper function
  readoutput <- function(x){
    tmp <- readLines(x)
    file.remove(x)
    preamble <- tmp[grepl(';',tmp)]
    body <- tmp[!grepl(';',tmp)]
    
    header <- 
      preamble[grepl('year step area',preamble)] %>% 
      gsub('; (*)','\\1',.) %>% 
      gsub('\\[|\\]','',.) %>% 
      stringr::str_split(' ') %>% 
      unlist() 
    
    body %>% 
      paste(collapse='\n') %>% 
      utils::read.table(text=.,
                 col.names = header,fill=TRUE,
                 stringsAsFactors = FALSE) %>% 
      dplyr::mutate(trial=cut(1:length(.data$year),
                              c(0,which(diff(.data$year)<0),1e9),
                              labels = FALSE)-1) %>% 
      tibble::as_tibble() 
  }
  
  
  ## TODO fix stocks that are spawning
  pre <- paste(gd$dir,gd$rel.dir,sep='/') 
  
  if(check.previous){
    if(file.exists(sprintf('%s/out.Rdata',pre))){
      load(sprintf('%s/out.Rdata',pre))
      return(out)
    }
  }
  
  dir.create(pre,showWarnings = FALSE, recursive = TRUE)
  dir.create(sprintf('%s/Aggfiles',pre), showWarnings = FALSE)
  
  
  ## read in model files
  main <- 
    read.gadget.main(file=main.file)
#    read.gadget.file(gd$dir,main.file,file_type = 'main',recursive = TRUE) 
#  main$likelihood$likelihoodfiles <- NULL
  
  stocks <-
    read.gadget.stockfiles(main$stockfiles)
#    main$stock$stockfiles %>% 
    
    #    purrr::map(~read.gadget.file(gd$dir,.,file_type = 'stock',recursive = TRUE))
  time <- 
    read.gadget.time(main$timefile)
  area <- 
    read.gadget.area(main$areafile)
  
  fleet <- 
    read.gadget.fleet(main$fleetfiles)
#    main$fleet$fleetfiles %>% 
#    purrr::map(~read.gadget.file(gd$dir,.,file_type = 'fleet',recursive = TRUE))
  
  all.fleets <- 
    paste(fleet$fleet$fleet,collapse = ' ')
  params <-
    read.gadget.parameters(params.file)
  rec <- 
    get.gadget.recruitment(stocks,params,collapse = FALSE) %>% 
    stats::na.omit() 
  
  if(is.null(ref.years)){
    ref.years <- 
      min(rec$year)
  }
  
  # rec.step.ratio <- 
  #   rec %>% 
  #   dplyr::group_by(stock,step) %>% 
  #   dplyr::filter(year %in% ref.years) %>% 
  #   dplyr::summarise(rec.ratio = sum(recruitment)) %>% 
  #   dplyr::mutate(rec.ratio = rec.ratio/sum(rec.ratio))
  
  rec <- 
    rec %>% 
#    dplyr::group_by(stock,year) %>% 
#    dplyr::summarise(recruitment = sum(recruitment)) %>% 
    dplyr::arrange(.data$stock,.data$year,.data$step)
  
  
  ## Write agg files
  plyr::l_ply(stocks,
              function(x){
                writeAggfiles(x,folder=sprintf('%s/Aggfiles',pre))
              })
  
  ## adapt model to include predictions
  sim.begin <- 
    time$lastyear + 1
  rec <- 
    rec %>% 
    dplyr::filter(.data$year < sim.begin)
  
  if(nrow(rec) == 0)
    stop('No recruitment info found')
  
  time$lastyear <- sim.begin + years - 1
  write.gadget.time(time,file = sprintf('%s/time.pre',pre))
  main$timefile <- sprintf('%s/time.pre',pre)
  
  time.grid <- 
    expand.grid(year = time$firstyear:time$lastyear,
                step = 1:length(time$notimesteps),
                area = area$areas)
  
  ## hmm check this at some point
  if(is.null(temperature)){
    area$temperature <- 
      dplyr::mutate(time.grid,
                    temperature = 5)
  } else {
    area$temperature <- 
      area$temperature %>% 
      dplyr::bind_rows(temperature) %>% 
      dplyr::mutate(area = as.numeric(.data$area))
    
    num.missing <- 
      dplyr::anti_join(time.grid %>% 
                         dplyr::mutate(area = as.numeric(.data$area)), 
                       area$temperature) %>% 
      dplyr::summarise(n=dplyr::n())
    if(num.missing$n>0){
      stop('Error temperature data mismatch')
    }
  }
  main$areafile <- sprintf('%s/area',pre)
  write.gadget.area(area,file=sprintf('%s/area',pre))
  
  ## fleet setup 
  fleet <-
    fleet %>%
    purrr::map(~dplyr::filter(.,.data$fleet %in% fleets$fleet))
    
  fleet$fleet <- 
    dplyr::mutate(fleet$fleet,
                  fleet = sprintf('%s.pre',.data$fleet),
                  multiplicative = '#rgadget.effort',   #effort,
                  amount = sprintf('%s/fleet.pre', pre),
                  type = 'linearfleet')
  
  fleet$prey <- 
    dplyr::mutate(fleet$prey,
                  fleet = sprintf('%s.pre',.data$fleet))
  
  fleet.predict <- 
    time.grid %>% 
    dplyr::filter((.data$year >= sim.begin | 
                     (.data$year==(sim.begin-1) & .data$step > time$laststep)) &
                    .data$area %in% fleet$fleet$livesonareas) 
  
  if('year' %in% names(fleets) | 'step' %in% names(fleets)){
    fleet.predict <- 
      fleet.predict %>% 
      dplyr::left_join(fleets)
  } else {
    fleet.predict <- 
      fleets %>% 
      split(.$fleet) %>% 
      purrr::map(~cbind(fleet.predict,.)) %>% 
      dplyr::bind_rows() 
  }
  
  fleet.predict <- 
    fleet.predict %>% 
    dplyr::mutate(fleet = paste(.data$fleet,'pre',sep='.'))
  
  write.gadget.table(dplyr::arrange(fleet.predict[c('year','step','area','fleet','ratio')],
                                    .data$year,.data$step,.data$area),
                     file=sprintf('%s/fleet.pre',pre),
                     col.names=FALSE,row.names=FALSE,
                     quote = FALSE)
  
  main$fleetfiles <- c(main$fleetfiles,sprintf('%s/fleet', pre))
  write.gadget.fleet(fleet,file=sprintf('%s/fleet', pre))
  
  
  
  ## recruitment 
  
  if(!is.null(rec.window)){
    if(length(rec.window)==1){
      tmp <- 
        rec %>%
        dplyr::ungroup() %>% 
        dplyr::filter(as.numeric(.data$year) < rec.window)
    } else if( 'data.frame' %in% class(rec.window)){
      tmp <- 
        rec %>%
        dplyr::ungroup() %>% 
        dplyr::left_join(rec.window,by = 'stock') %>% 
        dplyr::filter(as.numeric(.data$year) < .data$upper,
                      as.numeric(.data$year) > .data$lower)
    } else {
      tmp <- 
        rec %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(as.numeric(.data$year) <= max(rec.window) & 
                        as.numeric(.data$year) >= min(rec.window))
    }
  } else {
    tmp <- 
      rec %>% 
      dplyr::ungroup()
  }
  
  ## todo: consider covariates in recruitment
  
  if(stochastic){
    if(tolower(method) == 'bootstrap'){
      prj.rec <-
        tmp %>% 
        dplyr::group_by(.data$stock,.data$year) %>% 
        dplyr::summarise(recruitment = sum(.data$recruitment)) %>% 
        split(.$stock) %>% 
        purrr::map(~dplyr::select(.,.data$recruitment) %>% 
                     dplyr::slice(plyr::rlply(ceiling(num.trials*years/nrow(tmp)),
                                              c(sample(rec.window[2]:rec.window[3]-rec.window[1]+1,
                                                       replace = TRUE),
                                                sample(rec.window[1]:rec.window[2]-rec.window[1]+1,
                                                       replace = TRUE))) %>%                                           
                                    unlist())) %>%
        purrr::map(~dplyr::slice(.,1:(num.trials*years))) %>% 
        purrr::map(~tibble::tibble(year = rep((sim.begin):(sim.begin+years-1),num.trials),
                                   trial = rep(1:num.trials,each=years),
                                   recruitment = .$recruitment)) %>% 
        dplyr::bind_rows(.id='stock') %>% 
        dplyr::select(.data$stock,.data$year,.data$trial,.data$recruitment) %>% 
        dplyr::mutate(step = (rec$step[rec$year == min(ref.years)])[1])
      
    } else if(tolower(method) == 'ar1'){
      ## fit an AR model to the fitted recruiment
      prj.rec <- 
        tmp %>% 
        dplyr::group_by(.data$stock,.data$year) %>% 
        dplyr::summarise(recruitment = sum(.data$recruitment)) %>% 
        split(.$stock) %>% 
        purrr::map(~stats::lm(utils::head(.$recruitment,-1)~utils::tail(.$recruitment,-1))) %>%
        purrr::map(~dplyr::bind_cols(broom::glance(.),
                                     as.data.frame(t(broom::tidy(.)$estimate)))) %>% 
        purrr::map(~dplyr::rename(.,a=.data$V1,b=.data$V2)) %>% 
        purrr::map(~data.frame(year = rep((sim.begin):(sim.begin+years-1),num.trials),
                               trial = rep(1:num.trials,each=years),
                               rec = pmax(arima.sim(years*num.trials,model=list(ar=.$b),sd=.$sigma) + .$a,0))) %>% 
        dplyr::bind_rows(.id='stock')  %>% 
        dplyr::mutate(rec = ifelse(is.na(.data$rec),.data$x,.data$rec)) %>% 
        dplyr::select(.data$stock,.data$year,.data$trial,recruitment = .data$rec) %>% 
        dplyr::mutate(step = (rec$step[rec$year == min(ref.years)])[1])
      
      ## project next n years
    } else if(tolower(method) == 'custom'){
      if(is.function(prj.func)){
        prj.rec <- 
          tmp %>% 
          prj.func(...) 
        if(!('data.frame' %in% class(prj.rec)))
          stop('prj.func does not return a data.frame')
        if(!(c('stock','year','trial','recruitment','step') %in% names(prj.rec)))
          stop('prj.func does include columns stock, year, step, trial and recruitment')
      } else {
        stop('No projection function supplied')    
      }
    }else {
      stop('Invalid projection method')
    }
  } else {
    prj.rec <- 
      tmp %>% 
      dplyr::group_by(.data$stock) %>% 
      dplyr::summarise(recruitment = mean(.data$recruitment)) %>% 
      dplyr::left_join(expand.grid(stock = stocks %>% 
                                     purrr::map(getStockNames) %>% 
                                     unlist,
                                   year = (sim.begin):(sim.begin+years-1),
                                   trial = 1:num.trials) %>% 
                         dplyr::arrange(.data$stock,.data$year,.data$trial),
                       by = 'stock') %>% 
      dplyr::mutate(step = min(rec$step[rec$year == min(ref.years)]))
  }
  
  if(num.trials == 1 & length(effort)==1){
    prj.rec %>% 
      dplyr::mutate(switch = paste(.data$stock,'rec',.data$year,.data$step,sep='.'),
                    lower = 0,
                    upper = .data$recruitment + 1,
                    optimise = 0) %>% 
      dplyr::select(.data$switch,value=.data$recruitment,.data$lower,.data$upper,.data$optimise) %>% 
      dplyr::bind_rows(params,
                       data.frame(switch = 'rgadget.effort',
                                  value = effort,
                                  lower = 0.0001,
                                  upper = 100, 
                                  optimise = 0,
                                  stringsAsFactors = FALSE)) %>% 
      write.gadget.parameters(file=sprintf('%s/params.forward', pre))
  } else {
    params %>% 
      dplyr::select(.data$switch,.data$value) %>% 
      tidyr::spread(.data$switch,.data$value) %>% 
      dplyr::slice(rep(1,num.trials*length(effort))) %>% 
      dplyr::bind_cols(prj.rec %>% 
                         dplyr::mutate(switch = paste(.data$stock,'rec',.data$year,.data$step,sep='.')) %>% 
                         dplyr::select(.data$trial,.data$switch,.data$recruitment) %>% 
                         tidyr::spread(.data$switch,.data$recruitment) %>% 
                         dplyr::select(-.data$trial) %>% 
                         dplyr::slice(rep(1:num.trials,each=length(effort))) %>% 
                         dplyr::mutate(rgadget.effort=rep(effort,num.trials))) %>% 
      structure(file_format = 'wide') %>% 
      write.gadget.parameters(file=sprintf('%s/params.forward', pre))
  }
  
  
  ## add recruitment scalar
  if(is.null(rec.scalar)){
    prj.rec <- 
      prj.rec %>% 
      dplyr::mutate(rec.scalar=1)
  } else {
    if(sum(rec.scalar$stock %in% prj.rec$stock)==0)
      warning('No stocks found in rec.scalar')
    prj.rec <- 
      prj.rec %>% 
      dplyr::left_join(rec.scalar %>% 
                         dplyr::select(.data$stock, .data$year, .data$rec.scalar)) %>% 
      dplyr::mutate(rec.scalar = ifelse(is.na(.data$rec.scalar),1,.data$rec.scalar))
  }
  ## end fix
  
  ## create the output files
  print.txt <-
    paste('[component]',
          'type             stockprinter',
          'stocknames       %1$s',
          'areaaggfile      %2$s/Aggfiles/%1$s.area.agg',
          'ageaggfile       %2$s/Aggfiles/%1$s.allages.agg',
          'lenaggfile       %2$s/Aggfiles/%1$s.len.agg',
          'printfile        %2$s/out/%1$s.lw',
          'printatstart     1',
          'yearsandsteps    all 1',
          sep = '\n')
  
  catch.print <-
    paste('[component]',
          'type\t\tpredatorpreyprinter',
          'predatornames\t\t%3$s',
          'preynames\t\t%1$s',
          'areaaggfile      %2$s/Aggfiles/%1$s.area.agg',
          'ageaggfile       %2$s/Aggfiles/%1$s.allages.agg',
          'lenaggfile       %2$s/Aggfiles/%1$s.alllen.agg',
          'printfile        %2$s/out/catch.%1$s.lw',
          'yearsandsteps    all all',
          sep = '\n')
  
  if(!is.null(custom.print)){
    custom.print <- 
      readLines(custom.print) %>% 
      gsub('printfile[ \t]+([A-Za-z0-9]+)',sprintf('printfile\t%s/%s/\\1',pre,'out'),.) %>% 
      paste(., collapse="\n ")
    
  } else {NULL}
  
  printfile <-
    paste(
      custom.print,
      ';',
      paste(sprintf(catch.print, unique(fleet$prey$stock), pre,
                    paste(all.fleets, paste(fleet$fleet$fleet,collapse=' '))),
            collapse='\n'),
      paste(sprintf(print.txt,unique(fleet$prey$stock),
                    pre),
            collapse = '\n'),
      ';',
      '[component]',
      'type\tlikelihoodsummaryprinter',
      'printfile\t.jnk',
      sep = '\n')
  
  catch.files <- sprintf('catch.%s.lw',unique(fleet$prey$stock))
  print.files <- sprintf('%s.lw',unique(fleet$prey$stock))
  
  
  dir.create(sprintf('%s/out/',pre),showWarnings = FALSE, recursive = TRUE)
  
  main$printfiles <- sprintf('%s/printfile',pre)
  write.unix(printfile,f = sprintf('%s/printfile',pre))
  
  main$likelihoodfiles <- ';'
  
  plyr::llply(stocks,function(x){
    tmp <- 
      prj.rec %>% 
      dplyr::filter(.data$stock == x@stockname,.data$trial == 1) #%>% 
      #dplyr::left_join(rec.step.ratio,by=c('stock'))
    
    if(x@doesrenew==1){
      x@renewal.data <-
        x@renewal.data %>% 
        dplyr::arrange(.data$year,.data$step) %>% 
        dplyr::filter(.data$year < sim.begin) %>% 
        dplyr::bind_rows(x@renewal.data %>% 
                           dplyr::filter(.data$year == min(ref.years)) %>%
                           dplyr::mutate(n = dplyr::n()) %>% 
                           dplyr::slice(rep(1:.data$n[1],length(unique(tmp$year)))) %>% 
                           dplyr::mutate(year=rep(as.character(tmp$year),each = .data$n[1]),
                                         number = sprintf('(* (* 0.0001 #%s.rec.%s.%s ) %s)',
                                                          x@stockname,.data$year,.data$step, 
                                                          tmp$rec.scalar)) %>% #*tmp$rec.ratio)) %>% 
                           dplyr::select_(.dots = names(x@renewal.data))) %>% 
        as.data.frame()
    }
    gadget_dir_write(gd,x)
  })
  
  main$stockfiles <- paste(sprintf('%s/%s',pre,
                                   plyr::laply(stocks,function(x) x@stockname)),
                           collapse = ' ')
  
  
  write.gadget.main(main,file=sprintf('%s/main.pre',pre))
  
  
  callGadget(s = 1, i = sprintf('%s/params.forward',pre),
             main = sprintf('%s/main.pre',pre))
  
  time <- methods::new('gadget-time',
                       firstyear = time$firstyear,
                       firststep = time$firststep,
                       lastyear = time$lastyear,
                       laststep = time$laststep,
                       notimesteps = time$notimesteps)

  out <- 
    list.files(paste(pre,'out',sep='/')) %>%
    purrr::set_names(paste(paste(pre,'out',sep='/'),.,sep='/'),.) %>% 
    purrr::map(readoutput) %>% 
    purrr::map(~.x %>% 
                 dplyr::left_join(tibble::tibble(trial = 0:(num.trials*length(effort)-1),
                                                 effort = rep(effort,num.trials)),
                                  by = 'trial'))
    
  catch <- 
    out[catch.files] %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::mutate(stock = gsub('catch.(.+).lw','\\1',.data$stock))
  lw <- 
    out[print.files] %>% 
    dplyr::bind_rows(.id='stock') %>% 
    dplyr::mutate(stock = gsub('(^.+).lw','\\1',.data$stock))
  out <- out[!(names(out) %in% c(catch.files,print.files))]
  out <- 
    list(custom = out,
         catch = catch,
         lw = lw,
         recruitment = prj.rec %>% 
           tibble::as_tibble(),
         num.trials = num.trials,
         stochastic = stochastic,
         sim.begin = sim.begin)
  class(out) <- c('gadget.forward',class(out))
  if(save.results){
    save(out,file = sprintf('%s/out.Rdata',pre))
  }
  return(out)
}



plot.gadget.forward <- function(gadfor,type='catch',quotayear=FALSE){
  if(type=='catch'){
    ggplot2::ggplot(gadfor$catch %>% 
                      dplyr::group_by(.data$year,.data$effort,.data$trial) %>% 
                      dplyr::summarise(catch=sum(.data$biomass.consumed)/1e6),
                 ggplot2::aes(.data$year,.data$catch,col=.data$effort,lty=.data$trial)) +
      ggplot2::geom_rect(ggplot2::aes(ymin=-Inf,ymax=Inf,
                                      xmin=gadfor$sim.begin,xmax=Inf),
                         fill='gray',col='white')+
      ggplot2::geom_line()+ 
      ggplot2::labs(y="Catch (in '000 tons)", x='Year')     
  } else if(type=='ssb'){
    ggplot2::ggplot(gadfor$lw %>% 
                      dplyr::group_by(.data$year) %>% 
                      dplyr::summarise(ssb = sum(.data$ssb)/1e6),
                    ggplot2::aes(.data$year,.data$catch,col=.data$effort,lty=.data$trial)) +
      ggplot2::geom_bar(stat='identity') + 
      ggplot2::labs(y="SSB (in '000 tons)", x='Year')     
  } else if(type=='rec'){
    ggplot2::ggplot(gadfor$recruitment %>% 
                      dplyr::group_by(.data$year) %>% 
                      dplyr::summarise(catch=sum(.data$catch)),
                    ggplot2::aes(.data$year,.data$catch,col=.data$effort,lty=.data$trial)) +
      ggplot2::geom_bar(stat='identity') + 
      ggplot2::labs(y="Recruitment (in millions)", x='Year')     
  }
}

