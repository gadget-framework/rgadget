##' This function reads in the gadget output files defined in
##' printfiles. This is a quick and dirty implementation that has been
##' designed to read a few nice examples, so it may work for some instances
##' and it may not. It assumes that the last line containing a comment ';'
##' is the line describing the column names and is the last comment line.
##' @title Read gadget printfiles
##' @param path a character string with the name of the folder containing the printfiles
##' @param suppress logical should warnings of missing files be suppressed
##' @return a list containing the data that has been read in named after the files found in path.
##' @export
read.printfiles <- function(path='.',suppress=FALSE){
  
  
  
  read.printfile <- function(file){
    #    file <- paste(path,file,sep='/')
    tmp <- readLines(file)
    
    preamble <- tmp[grepl(';',tmp)]
    body <- tmp[!grepl(';',tmp)]
    header <- 
      preamble[grepl('year step area',preamble)] %>% 
      gsub('; (*)','\\1',.) %>% 
      gsub('\\[|\\]','',.) %>% 
      stringr::str_split(' ') %>% 
      unlist() 
    
    if(is.null(header)){
      warning('Old style printfile detected, update Gadget to the most recent version')
      header <- 
        preamble[grepl('year-step-area',preamble)] %>% 
        gsub('; (*)','\\1',.) %>% 
        stringr::str_split('-') %>% 
        unlist() %>% 
        gsub(' ','_',.)
    }
    
    
    data <- body %>% 
      paste(collapse='\n') %>% 
      paste('\n',sep='') %>% 
      readr::read_table2(file = .,
                         col_names = FALSE,
                         guess_max = length(body))  
    
    if(is.null(data))
      return(NULL)
    if(ncol(data) > length(header)){
      if(!suppress)
        warning(sprintf('Error in read.printfile -- Header could not be read from file %s',file))
    } else {
      names(data) <- header[1:ncol(data)]
    }
    pos <- grep('Regression information',tmp)
    if(length(pos)!=0){
      areas <- 
        gsub('; Regression information for area ','',tmp[pos]) %>%
        cbind(areas=.,n=diff(c(pos,length(tmp)+1))-1) %>%
        as.data.frame() %>%
        split(.$areas) %>%
        purrr::map_df(function(x) data.frame(areas=rep(x$areas,x$n))) 
      
      regr.txt <- 
        tmp[-c(1:min(pos-1),pos)] %>%
        gsub('; ','',.) %>%
        paste(.,areas$areas)
      
      regr <- utils::read.table(text=regr.txt,stringsAsFactors = FALSE)[c(1,3,5,7,8)] %>% 
        tibble::as_tibble()
      names(regr) <- c('label','intercept','slope','sse','area')
      data <- data %>% 
        dplyr::left_join(regr, by = c('area','label'))
    }
    return(data)
  }
  printfiles <- 
    fs::dir_ls(path) %>% 
    purrr::map(read.printfile) %>% 
    purrr::set_names(.,gsub(path,'',names(.)) %>% gsub('/','',.,fixed = TRUE))
  class(printfiles) <- c('gadgetOut','list')
  return(printfiles)
}
##' \code{read.gadget.likelihood} reads the likelihood (input) file for gadget. The format of
##' the likelihood file is described in gadget's user manual.
##' @title Old style gadget file input and output (mostly deprecated)
##' @rdname gadgetFileIO 
##' @param files a vector of character strings containing the names of the likelihood files
##' @return object of class gadget.likelihood, i.e. a list containing the various likelihood components
##' @author Bjarki Þór Elvarsson
##' @export
read.gadget.likelihood <- function(files='likelihood'){
  lik <- NULL
  for(file in files){
    lik <- c(lik,sub(' +$','',gsub('\t',' ',readLines(file))))
  }
  lik <- stringr::str_trim(lik)
  lik <- lik[lik!='']
  lik <- lik[!grepl(';',substring(lik,1,1))]
  lik <- sapply(strsplit(lik,';'),function(x) sub(' +$','',x[1]))
  
  
  comp.loc <- grep('component',lik)
  name.loc <- comp.loc+3
  weights <- NULL
  common <- c('name','weight','type','datafile','areaaggfile','lenaggfile',
              'ageaggfile','sitype','function','preyaggfile')
  tmp.func <- function(comp){
    loc <- grep(paste('[ \t]',tolower(comp),sep=''),tolower(lik[name.loc]))
    if(sum(loc)==0){
      return(NULL)
    } else {
      
      dat <- plyr::ldply(loc, function(dd){
        if(dd < length(comp.loc)) {
          restr <- (comp.loc[dd] + 1):(comp.loc[dd+1]-1)
        } else {
          restr <- 1:length(lik) > comp.loc[dd]
        }
        
        tmp <- sapply(strsplit(sapply(strsplit(lik[restr],'[ \t]'),
                                      function(x) {
                                        paste(x[!(x==''|x=='\t')],
                                              collapse=' ')
                                      }),' '),
                      function(x) as.character(x))
        if(class(tmp)!='list'){
          names.tmp <- utils::head(tmp,1)
          tmp <- as.data.frame(tmp,stringsAsFactors=FALSE)[2,]
          names(tmp) <- names.tmp
          row.names(tmp) <- tmp$name
          tmp$type <- tolower(tmp$type)
        } else {
          names.tmp <- sapply(tmp,function(x) x[1])
          tmp <- sapply(tmp,function(x) paste(x[-1], collapse='\t'))
          names(tmp) <- names.tmp
          tmp <- as.data.frame(t(tmp),stringsAsFactors=FALSE)
          tmp$type <- tolower(tmp$type)
        }
        return(tmp)
      })
      if(is.null(weights)){
        weights <<- dat[intersect(common, unique(c(names(weights),names(dat))))]
      } else {
        weights <<-
          dplyr::bind_rows(dat, weights)[intersect(common, unique(c(names(weights),
                                                                    names(dat))))]
      }
      dat$weight <- NULL
      return(dat)
    }
  }
  
  likelihood <- list(penalty = tmp.func('penalty'),
                     understocking = tmp.func('understocking'),
                     migrationpenalty = tmp.func('migrationpenalty'),
                     surveyindices = tmp.func('surveyindices'),
                     catchdistribution = tmp.func('catchdistribution'),
                     catchstatistics = tmp.func('catchstatistics'),
                     surveydistribution = tmp.func('surveydistribution'),
                     stockdistribution = tmp.func('stockdistribution'),
                     stomachcontent = tmp.func('stomachcontent'),
                     recaptures = tmp.func('recaptures'),
                     recstatistics = tmp.func('recstatistics'),
                     catchinkilos = tmp.func('catchinkilos')
  )
  likelihood$weights <- weights
  row.names(likelihood$weights) <- weights$name
  likelihood$weights$weight <- as.numeric(weights$weight)
  likelihood <- likelihood[c('weights',unique(likelihood$weights$type))]
  class(likelihood) <- c(class(likelihood),'gadget.likelihood')
  return(likelihood)
}
##' \code{write.gadget.likelihood} writes a likelihood object to file
##' @rdname gadgetFileIO
##' @param lik object of class gadget.likelihood
##' @param file name of the likelihood file
##' @param data.folder location of data folder (if changed)
##' @param bs.sample (for bootstrap), appends the appropriate replicate number to data file
##' @return character string corresponding to the likelihood file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.likelihood <- function(lik,file='likelihood',
                                    data.folder=NULL, bs.sample=NULL){
  lik.text <- sprintf('; Likelihood file - created in Rgadget\n; %s - %s',
                      file, Sys.Date())
  weights <- lik$weights[c('name','weight')]
  lik$weights <- NULL
  for(comp in lik){
    if(!is.null(data.folder)){
      comp$datafile <- paste(data.folder,comp$datafile,sep='/')
    }
    ## reorder surveyindices columns
    if('surveyindices' %in% comp$type){
      comp <-
        comp[intersect(c('name','type','datafile','sitype','biomass',
                         'areaaggfile','ageaggfile','lenaggfile','surveynames',
                         'fleetnames','stocknames','fittype','slope',
                         'intercept'),
                       names(comp))]
    }
    ## same with catchdistribution
    if('catchdistribution' %in% comp$type){
      comp <-
        comp[intersect(c('name','type','datafile','function','aggregationlevel',
                         'overconsumption','epsilon','areaaggfile','ageaggfile',
                         'lenaggfile','fleetnames','stocknames'),
                       names(comp))]
    }
    ## and with catchstatistics
    if('catchstatistics' %in% comp$type){
      comp <-
        comp[intersect(c('name','type','datafile','function','areaaggfile',
                         'lenaggfile','ageaggfile','fleetnames','stocknames'),
                       names(comp))]
    }
    
    
    comp <- stats::na.omit(reshape2::melt(merge(weights,comp,by='name',sort=FALSE),
                                          id.vars = 'name'))
    comp.text <- plyr::ddply(comp,'name',function(x){
      paste('[component]',
            sprintf('name\t\t%s',x$name[1]),
            paste(x$variable,x$value, sep = '\t\t',
                  collapse = '\n'),
            ';', sep = '\n')
    })
    
    lik.text <- paste(lik.text,
                      paste(comp.text$V1,
                            collapse='\n'),
                      sep='\n')
  }
  if(!is.null(bs.sample))
    write.unix(sprintf(lik.text,bs.sample),f=file)
  else
    write.unix(lik.text,f=file)
  invisible(lik.text)
}

##' \code{get.gadget.likelihood} retrives selected parts of the likelihood object
##' @rdname gadgetFileIO
##' @param likelihood likelihood object
##' @param comp selected likelihood components
##' @param inverse (logical) should inverse selection be applied
##' @return likelihood object
##' @author Bjarki Thor Elvarsson
get.gadget.likelihood <- function(likelihood,comp,inverse=FALSE){
  if(inverse)
    weights <- dplyr::filter(likelihood$weights,!(.data$name %in% comp))
  else
    weights <- dplyr::filter(likelihood$weights,.data$name %in% comp)
  tmp <-
    within(list(),
           for(type in weights$type){
             restr <- likelihood[[type]][['name']] %in% comp
             if(inverse)
               restr <- !restr
             assign(type,
                    likelihood[[type]][restr,])
           }
    )
  tmp$restr <- NULL
  tmp$type <- NULL
  tmp$weights <- weights
  class(tmp) <- c('gadget.likelihood',class(tmp))
  return(tmp)
}


##' \code{read.gadget.file} reads gadget's main file
##' @rdname gadgetFileIO
##' @param file main file location
##' @return object of class gadget.main
##' @author Bjarki Þór Elvarsson
read.gadget.main <- function(file='main'){
  if(!file.exists(file)) {
    stop('Main file not found')
  }
  main <- sub(' +$','',readLines(file))
  if(length(main) == 0)
    stop(sprintf('Error in read.gadget.main, file %s is empty',file))
  main <- main[main!='']
  main <- main[!grepl(';',substring(main,1,1))]
  main <- sapply(strsplit(main,';'),function(x) x[1])
  main <- clear.spaces(main)
  tmp <- sapply(main[sapply(main,length)!=1],function(x) x[2:length(x)])
  names(tmp) <-  sapply(main[sapply(main,length)!=1],function(x) x[1])
  main <- as.list(tmp)
  class(main) <- c('gadget.main',class(main))
  return(main)
}


##' \code{write.gadget.main} writes gadget.main object to file
##' @rdname gadgetFileIO
##' @param main gadget.main object
##' @param file name of main file
##' @return text of the main file (if desired)
##' @author Bjarki Þór Elvarsson
write.gadget.main <- function(main,file='main'){
  main.text <- sprintf('; main file for gadget - created in Rgadget\n; %s - %s',
                       file,date())
  if(is.null(main$printfiles)){
    main$printfiles <- '; no printfile supplied'
  }
  main.text <-
    paste(main.text,
          paste('timefile',main$timefile),
          paste('areafile',main$areafile),
          paste('printfiles',paste(main$printfiles,collapse='\t')),
          '[stock]',
          paste('stockfiles',paste(main$stockfiles,collapse='\t')),
          ifelse(is.null(main$tagfiles), #| main$tagfiles == '',
                 '[tagging]',
                 paste('[tagging]\ntagfiles',paste(main$tagfiles,
                                                   collapse='\t'))),
          ifelse(is.null(main$otherfoodfiles), #| main$otherfoodfiles == '',
                 '[otherfood]',
                 paste('[otherfood]\notherfoodfiles',
                       paste(main$otherfoodfiles,collapse='\t'))),
          ifelse(is.null(main$fleetfiles), # | main$likelihoodfiles == '',
                 '[fleet]',
                 paste('[fleet]\nfleetfiles',
                       paste(main$fleetfiles,collapse='\t'))),
          ifelse(is.null(main$likelihoodfiles),'[likelihood]',
                 paste('[likelihood]\nlikelihoodfiles',
                       paste(main$likelihoodfiles,collapse='\t'))),
          sep='\n')
  write.unix(main.text,f=file)
  invisible(main.text)
}

##' \code{clear.spaces} clears tab and spaces from a string and return a list or a matrix of values
##' @rdname gadgetFileIO
##' @param text string
##' @return list or matrix containing the (non-empty) values from the string
##' @author Bjarki Þór Elvarsson
clear.spaces <- function(text){
  sapply(strsplit(sapply(strsplit(text,'[ \t]'),
                         function(x) {
                           paste(x[!(x==''|x=='\t')],
                                 collapse=' ')
                         }),' '),
         function(x) x)
}



##' @title Make gadget printfile
##'
##' @param main.file location of the main file
##' @param printatstart print at the end or start of a step
##' @param steps print steps
##' @param recruitment_step_age data frame with stock and recruitment age
##' @param gd location of the gadget model
##' @param file name of resulting printfile
##'
##' @return gadget.mainfile object
##' @author Bjarki Thor Elvarsson
make.gadget.printfile <- function(main.file='main',
                                  file='printfile',
                                  printatstart = 1,
                                  steps = 1,
                                  recruitment_step_age = NULL,
                                  gd = list(dir='.',output = 'out',aggfiles = 'print.aggfiles')){
  
  main <- read.gadget.file(gd$dir,main.file,file_type = 'main')
  
  if(length(main$likelihood$likelihoodfiles)>0){
    lik <- 
      main$likelihood$likelihoodfiles %>% 
      purrr::map(~read.gadget.file(gd$dir,.,
                                   file_type = 'likelihood',
                                   recursive = FALSE))
  } else {
    lik <- NULL
  }
  stocks <- 
    main$stock$stockfiles %>% 
    purrr::map(~read.gadget.file(path=gd$dir,file_name = .,file_type = 'stock',recursive = FALSE)) 
  
  names(stocks) <- stocks %>% purrr::map(1) %>% purrr::map('stockname') %>% unlist()
  
  if(is.null(recruitment_step_age)){
    recruitment_step_age <- 
      stocks %>% 
      purrr::map(1) %>% 
      ## begin hack
      purrr::map(~purrr::set_names(.,tolower(names(.)))) %>% 
      ## end hack
      purrr::map('minage') %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(step=1) %>% 
      tidyr::gather("stock","age",-"step") 
    
  }
  
  fleets <- 
    main$fleet$fleetfiles %>% 
    purrr::map(~read.gadget.file(gd$dir,.,recursive = FALSE)) %>% 
    purrr::flatten()
  
  names(fleets) <- 
    fleets %>% 
    purrr::map(1) %>% 
    unlist() %>% 
    as.character()
  
  header <- sprintf('; gadget printfile, created in %s',Sys.Date())
  lik.summary <- 
    paste('[component]',
          'type\tlikelihoodsummaryprinter',
          sprintf('printfile\t%s/likelihoodsummary', gd$output),
          ';',sep='\n')
  
  
  lik.template <-
    paste('[component]',
          'type\tlikelihoodprinter',
          'likelihood\t%1$s',
          sprintf('printfile\t%s/%%1$s',gd$output),
          ';', sep='\n')
  
  
  stock.std <-
    paste('[component]',
          'type\tstockstdprinter',
          'stockname\t%1$s',
          sprintf('printfile\t%s/%%1$s.std',gd$output),
          sprintf('printatstart %s', printatstart),
          sprintf('yearsandsteps\tall\t%s', steps),
          ';',
          sep='\n')
  
  stock.full <-
    paste('[component]',
          'type\tstockprinter',
          'stocknames\t%1$s',
          sprintf('areaaggfile\t%s/%%1$s.area.agg',gd$aggfiles),
          sprintf('ageaggfile\t%s/%%1$s.allages.agg',gd$aggfiles),
          sprintf('lenaggfile\t%s/%%1$s.len.agg',gd$aggfiles),
          sprintf('printfile\t%s/%%1$s.full',gd$output),
          sprintf('printatstart\t%s', printatstart),
          sprintf('yearsandsteps\tall\t%s', steps),
          ';',
          sep='\n')
  
  predator <-
    paste('[component]',
          'type\tpredatorpreyprinter',
          'predatornames\t%2$s',
          'preynames\t%1$s',
          sprintf('areaaggfile\t%s/%%1$s.area.agg',gd$aggfiles),
          sprintf('ageaggfile\t%s/%%1$s.age.agg',gd$aggfiles),
          sprintf('lenaggfile\t%s/%%1$s.alllen.agg',gd$aggfiles),
          sprintf('printfile\t%s/%%1$s.prey',gd$output),
          'yearsandsteps\tall all',
          ';',
          sep = '\n')
  
  predator.prey <-
    paste('[component]',
          'type\tpredatorpreyprinter',
          'predatornames\t%2$s',
          'preynames\t%1$s',
          sprintf('areaaggfile\t%s/%%1$s.area.agg',gd$aggfiles),
          sprintf('ageaggfile\t%s/%%1$s.allages.agg',gd$aggfiles),
          sprintf('lenaggfile\t%s/%%1$s.len.agg',gd$aggfiles),
          sprintf('printfile\t%s/%%1$s.prey.%%2$s',gd$output),
          'yearsandsteps\tall all',
          ';',
          sep = '\n')
  
  recruitment.print <- 
    paste('[component]',
          'type\tstockprinter',
          'stocknames\t%s',
          sprintf('areaaggfile\t%s/%%1$s.area.agg',gd$aggfiles),
          sprintf('ageaggfile\t%s/%%1$s.rec.age.agg',gd$aggfiles),
          sprintf('lenaggfile\t%s/%%1$s.alllen.agg',gd$aggfiles),
          sprintf('printfile\t%s/%%1$s.recruitment',gd$output),
          'printatstart\t0',
          'yearsandsteps\tall %s',
          ';',
          sep = '\n')
  
  prey.subset <- 
    stocks %>%  purrr::keep(~.$iseaten$iseaten == 1) %>% names()
  
  pred_prey_table <- expand.grid(preys = prey.subset,
                                 predators = c(names(fleets),
                                               stocks %>% 
                                                 purrr::keep(~.$doeseat$doeseat == 1) %>% 
                                                 names()), 
                                 stringsAsFactors = FALSE)
  
  
  dir.create(gd$output, showWarnings = FALSE)
  dir.create(gd$aggfiles, showWarnings = FALSE)
  
  
  stocks %>% 
    purrr::map(function(x){
      ## hack begins
      names(x[[1]]) <- tolower(names(x[[1]]))
      ## hack ends
      lengths <- seq(x[[1]]$minlength,x[[1]]$maxlength,by = x[[1]]$dl)
      lenAgg <- data.frame(length = paste('len',utils::tail(lengths,-1),
                                          sep = ''),
                           min = utils::head(lengths,-1),
                           max = utils::tail(lengths,-1))
      
      agg.head <-
        paste(sprintf('; aggregation file for %s created using rgadget at %s',
                      x[[1]]$stockname,Sys.Date()),
              paste(c('; ',names(lenAgg)),collapse = '\t'),
              sep = '\n')
      write.unix(agg.head,f = sprintf('%s/%s.len.agg',gd$aggfiles,
                                      x[[1]]$stockname))
      
      write.gadget.table(lenAgg,
                         file = sprintf('%s/%s.len.agg',gd$aggfiles,
                                        x[[1]]$stockname),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      
      ## all length agg file
      alllenAgg <- data.frame(length = 'alllen',
                              min = min(lengths),
                              max = max(lengths))
      write.unix(agg.head,f = sprintf('%s/%s.alllen.agg',gd$aggfiles,
                                      x[[1]]$stockname))
      write.gadget.table(alllenAgg,
                         file = sprintf('%s/%s.alllen.agg',gd$aggfiles,x[[1]]$stockname),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      
      ## age agg file
      ageAgg <- data.frame(label = x[[1]]$minage:x[[1]]$maxage,
                           age = x[[1]]$minage:x[[1]]$maxage)
      write.unix(agg.head,f = sprintf('%s/%s.age.agg',gd$aggfiles,x[[1]]$stockname))
      write.gadget.table(ageAgg,
                         file = sprintf('%s/%s.age.agg',gd$aggfiles,x[[1]]$stockname),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      
      ## recruitment age agg file
      rec.ageAgg <- 
        recruitment_step_age %>% 
        dplyr::filter(.data$stock == x[[1]]$stockname) %>% 
        dplyr::mutate(label = .data$age) %>% 
        dplyr::select(.data$label,.data$age)
      
      if(nrow(rec.ageAgg)>0){  
        write.unix(agg.head,f = sprintf('%s/%s.rec.age.agg',gd$aggfiles,x[[1]]$stockname))
        write.gadget.table(rec.ageAgg,
                           file = sprintf('%s/%s.rec.age.agg',gd$aggfiles,x[[1]]$stockname),
                           col.names=FALSE,append=TRUE,
                           quote=FALSE,sep='\t',row.names=FALSE)
      }
      
      ## allages.agg
      allagesAgg <- data.frame(label = 'allages',
                               age = paste(x[[1]]$minage:x[[1]]$maxage,
                                           collapse = '\t'))
      write.unix(agg.head,f = sprintf('%s/%s.allages.agg',gd$aggfiles,
                                      x[[1]]$stockname))
      write.gadget.table(allagesAgg,
                         file = sprintf('%s/%s.allages.agg',gd$aggfiles,x[[1]]$stockname),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      ## Area agg file
      areaAgg <- data.frame(label=paste('area',
                                        x[[1]]$livesonareas,
                                        sep = ''),
                            area = x[[1]]$livesonareas)
      write.unix(agg.head,f = sprintf('%s/%s.area.agg',gd$aggfiles,
                                      x[[1]]$stockname))
      write.gadget.table(areaAgg,
                         file = sprintf('%s/%s.area.agg',gd$aggfiles,x[[1]]$stockname),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      
      
      
      
    })-> foo
  
  
  if(length(lik)>0){
    txt <- 
      lik %>%
      purrr::flatten() %>% 
      purrr::discard(~.$type %in% c('understocking','penalty',
                                    'migrationpenalty')) %>% 
      purrr::map('name') %>% 
      unlist() %>%
      purrr::map(~sprintf(lik.template,.)) %>% 
      unlist()
    
    
  } else {
    txt <- ';'
    lik.summary <- ';'
  }
  
  write.unix(paste(header,
                   lik.summary,
                   paste(txt,collapse='\n'),
                   paste(sprintf(stock.std,plyr::laply(stocks,
                                                       function(x) x[[1]]$stockname)),
                         collapse='\n'),
                   paste(sprintf(stock.full,plyr::laply(stocks,
                                                        function(x) x[[1]]$stockname)),
                         collapse='\n'),
                   ifelse(length(fleets)>0,
                          paste(sprintf(predator,prey.subset,
                                        paste(names(fleets),collapse = ' ')),
                                collapse='\n'),
                          ';\n'),
                   paste(sprintf(predator.prey,
                                 pred_prey_table$preys,
                                 pred_prey_table$predators),
                         collapse='\n'),
                   paste(sprintf(recruitment.print,recruitment_step_age$stock,
                                 recruitment_step_age$step),
                         collapse = '\n'),
                   ';',
                   sep='\n'),
             f=file)
}



##' Produce diagnostics
##' @title Gadget results
##' @param grouping --defunct--
##' @param final --defunct--
##' @param wgts location of the folder whith results from the
##' iterative refweighting
##' @param normalize (logical) should the resulting table be normalized
##' @return table with SS
##' @author Bjarki Thor Elvarsson
read.gadget.results <- function(grouping=list(),
                                final=list(final='final'),
                                wgts='WGTS',
                                normalize = FALSE
){
  
  read.gadget.SS <- function(file='lik.out'){
    lik.out <- readLines(file)
    SS <- as.numeric(clear.spaces(strsplit(lik.out[length(lik.out)],
                                           '\t\t')[[1]][2]))
    return(SS)
  }
  likelihood <- read.gadget.likelihood(sprintf('%s/likelihood.final',wgts))
  grouping <- read.gadget.grouping(lik=likelihood,wgts=wgts)
  comp.tmp <- dplyr::filter(likelihood$weights,
                            !(.data$type %in% c('penalty','understocking',
                                                'migrationpenalty','catchinkilos'))&
                              !(.data$name %in% unlist(grouping)))$name
  comp <- within(grouping,
                 for(item in comp.tmp){
                   assign(item, item)
                 })
  comp$item <- NULL
  res <-
    dplyr::bind_rows(plyr::ldply(comp,
                                 function(x)
                                   read.gadget.SS(paste(wgts,
                                                        paste('lik',
                                                              paste(x,collapse='.'),
                                                              sep='.'),sep='/'))),
                     plyr::ldply(final,
                                 function(x)
                                   read.gadget.SS(paste(wgts,
                                                        paste('lik',
                                                              paste(x,collapse='.'),
                                                              sep='.'),sep='/'))))
  names(res)[-1] <- likelihood$weights$name
  rownames(res) <- res$.id
  if(normalize){
    for(group in names(grouping)){
      for(comp in grouping[[group]]){
        res[,comp] <- res[,comp]/res[group,comp]
      }
    }
  }
  
  return(res)
}


##' @rdname gadgetFileIO
##' @description \code{read.gadget.data} reads data used by the various components
##' @param likelihood object of class gadget.likelihood
##' @param debug should debug information be printed
##' @param year_range limit the years read in
##' @return list of dataframes and degress of freedom
##' @author Bjarki Þór Elvarsson
##' @export
read.gadget.data <- function(likelihood,debug=FALSE,year_range=NULL){
  read.agg <- function(x, first = FALSE){      
    if(first){
      return(sapply(strsplit(readLines(x),'[\t ]'),function(x) x[1]))
    }  else {
      return(utils::read.table(x,stringsAsFactors=FALSE,comment.char=';'))
    }
  }
  
  read.preyagg <- function(x){
    tmp <- readLines(x)
    loc <- grep('lengths',tmp)
    tmp2 <- utils::read.table(text=tmp[grepl('lengths',tmp)])
    tmp2$V1 <- clear.spaces(tmp[loc-2])
    return(tmp2)
  }
  
  read.func <- function(x){
    if(debug){
      print(sprintf('reading datafile %s',x$datafile))
    }
    dat <- tryCatch(utils::read.table(x$datafile,comment.char=';',stringsAsFactors = FALSE),
                    error = function(x) NULL)
    
    
    area.agg <- tryCatch(read.agg(x$areaaggfile, first = TRUE),
                         warning = function(x) NULL,
                         error = function(x) NULL)
    age.agg <- tryCatch(read.agg(x$ageaggfile, first = TRUE),
                        warning = function(x) NULL,
                        error = function(x) NULL)
    len.agg <- tryCatch(read.agg(x$lenaggfile),
                        warning = function(x) NULL,
                        error = function(x) NULL)
    
    prey.agg <- tryCatch(read.preyagg(x$preyaggfile),
                         warning = function(x) NULL,
                         error = function(x) NULL)
    
    
    if(x$type=='catchdistribution'){
      names(dat) <- c('year','step','area','age','length','number')
    }
    if(x$type=='catchstatistics'){
      if(x[['function']] %in%
         c('lengthcalcstddev','weightnostddev','lengthnostddev'))
        names(dat) <- c('year','step','area','age','number','mean')
      if(x[['function']] %in% c('lengthgivenstddev','weightgivenstddev',
                                'lengthgivenvar'))
        names(dat) <- c('year','step','area','age','number','mean','stddev')
      if(x[['function']] %in% c('weightgivenstddevlen'))
        names(dat) <- c('year','step','area','age','number','mean','stddev') ## tempfix: gadget output is wrong
      #names(dat) <- c('year','step','area','length','number','mean','stddev')
      
    }
    if(x$type=='stockdistribution'){
      names(dat) <- c('year','step','area','stock','age','length','number')
    }
    if(x$type=='surveyindices'){
      if(x$sitype %in% c('lengths','fleets') )
        names(dat) <- c('year','step','area','length','number')
      if(x$sitype=='ages')
        names(dat) <- c('year','step','area','age','number')
      if(x$sitype=='acoustic')
        names(dat) <- c('year','step','area','survey','number')
      if(x$sitype=='effort')
        names(dat) <- c('year','step','area','fleet','number')
    }
    if(x$type == 'surveydistribution'){
      names(dat) <- c('year','step','area','age','length','number')
    }
    if(x$type=='stomachcontent'){
      if(ncol(dat)==6) {
        names(dat) <- c('year','step','area','predator','prey','ratio')
      } else if(ncol(dat) == 7){
        names(dat) <- c('year','step','area','predator','prey','ratio','std_dev')
      } else if(ncol(dat) == 5){
        names(dat) <- c('year','step','area','predator','ratio')
      }
    }
    if(x$type=='recaptures'){
      names(dat) <- c('tagid','year','step','area','length','number')
    }
    if(x$type=='recstatistics'){
      if(x[['function']]=='lengthgivenstddev')
        names(dat) <- c('tagid','year','step','area','number','mean','stddev')
      else
        names(dat) <- c('tagid','year','step','area','number','mean')
    }
    if(x$type=='catchinkilos'){
      if(ncol(dat)==4) #x$aggregationlevel==1)
        names(dat) <- c('year','area','fleet','biomass')
      else
        names(dat) <- c('year','step','area','fleet','biomass')
    }
    
    restr.area <- (dat$area %in% area.agg)
    if(length(restr.area)==0)
      restr.area <- TRUE
    restr.age <- (dat$age %in% age.agg)
    if(length(restr.age)==0)
      restr.age <- TRUE
    restr.len <- (dat$length %in% len.agg[,1])
    if(length(restr.len)==0)
      restr.len <- TRUE
    dat <- dat[restr.area&restr.age&restr.len,]
    if('length' %in% names(dat)){
      names(len.agg)[1:3] <- c('length','lower','upper')
      dat <- merge(dat,len.agg,all.x=TRUE)
    }
    if('predator' %in% names(dat)){
      names(len.agg)[1:3] <- c('predator','lower','upper')
      dat <- merge(dat,len.agg,all.x=TRUE)
    }
    if('prey' %in% names(dat)){
      names(prey.agg)[1:3] <- c('prey','prey.lower','prey.upper')
      dat <- merge(dat,prey.agg,all.x=TRUE)
    }
    
    if(!is.null(year_range)){
      dat <- 
        dat %>% 
        dplyr::filter(.data$year %in% year_range)
    }
    
    attr(dat,'len.agg') <- len.agg
    attr(dat,'pred.agg') <- len.agg
    attr(dat,'age.agg') <- age.agg
    attr(dat,'prey.agg') <- prey.agg
    attr(dat,'area.agg') <- area.agg
    return(dat)
  }
  
  if(length(likelihood$weights$weight) == 0)
    return(list(dat=list(),df=list()))
  
  lik.dat <- plyr::dlply(subset(likelihood$weights,
                                !(likelihood$weights$type %in% c('penalty', 'understocking',
                                                                 'migrationpenalty','proglikelihood'))),
                         'type',
                         function(x) plyr::dlply(x,'name',read.func))
  
  df <- lapply(lik.dat,function(x)
    sapply(x,function(x){
      x <- stats::na.omit(x)
      tmp <- 0
      if(length(intersect(c('lower','upper'),names(x)))>0){
        tmp <- 2
      }
      nrow(x[x[,ncol(x)-tmp]>0,])
    }))
  gadDat <- list(dat=lik.dat,df=df)
  class(gadDat) <- c('gadgetData','list')
  return(gadDat)
}






##' Read in the gadget likelihood output.
##' @title Read gadget lik.out
##' @param file string containing the name of the file
##' @param suppress logical, should file errors be suppressed
##' @return a list containing the swicthes (names of variable), weigths
##' (l?kelihood components) and data (dataframe with the parameter values,
##' likelihood component values and the final score.
##' @author Bjarki Thor Elvarsson, Hoskuldur Bjornsson
##' @export
read.gadget.lik.out <- function(file='lik.out',suppress=FALSE){
  if(!file.exists(file)){
    return(NULL)
  }
  lik <-  tryCatch(readLines(file),
                   error = function(e){
                     if(!suppress)
                       print(sprintf('file corrupted -- %s', file))
                     return(NULL)
                   })
  
  i <- grep("Listing of the switches",lik)
  i1 <- grep("Listing of the likelihood components",lik)
  i2 <- grep("Listing of the output from the likelihood",lik)
  
  if(is.null(i)|is.null(i1)|is.null(i2)){
    warning(sprintf('file %s is corrupt',file))
    return(NULL)
  }
  
  switches <- tryCatch(lapply(strsplit(lik[(i+1):(i1-2)],'\t'),unique),
                       error = function(e){
                         if(!suppress)
                           print(sprintf('file corrupted -- %s', file))
                         return(NULL)
                       })
  if(is.null(switches)){
    return(NULL)
  }
  names(switches) <- sapply(switches,function(x) x[1])
  switches <- lapply(switches,function(x) x[-1])
  
  weights <- t(sapply(strsplit(lik[(i1+3):(i2-2)],'\t'),function(x) x))
  weights <- as.data.frame(weights,stringsAsFactors=FALSE)
  weights$V2 <- as.numeric(weights$V2)
  weights$V3 <- as.numeric(weights$V3)
  names(weights) <- c('Component','Type','Weight')
  
  data <- utils::read.table(file,skip=(i2+1))
  names(data) <- c('iteration',names(switches),weights$Component,'score')
  attr(data,'Likelihood components') <- weights$Component
  attr(data,'Parameters') <- names(switches)
  lik.out <- list(switches=switches,weights=weights,data=data)
  class(lik.out) <- c('gadget.lik.out',class(lik.out))
  return(lik.out)
}



##' \code{strip.comments} is a helper function created to clear out all comments (indicated by ';') and
##' unwanted spaces from gadget input and output files.
##' @rdname gadgetFileIO
##' @param file location of the gadget input file
##' @return list containing the lines from the file stripped of unwanted text.
##' @author Bjarki Thor Elvarsson
strip.comments <- function(file='main'){
  tmp <- unlist(plyr::llply(file,readLines))
  main <- sub('\t+$',' ',tmp)
  main <- gsub("^\\s+|\\s+$", "", tmp) #sub(' +$','',main)
  comments <- main[grepl(';',substring(main,1,1))]
  main <- main[!grepl(';',substring(main,1,1))]
  main <- gsub('(','( ',main,fixed=TRUE)
  main <- gsub(')',' )',main,fixed=TRUE)
  main <- main[main!='']
  main <- sapply(strsplit(main,';'),function(x) x[1])
  main <- clear.spaces(main)
  #  attr(main,'comments') <- comments
  return(main)
}









##' @rdname gadgetFitHelpers
##' @description \code{read.gadget.wgts} reads the output from iterative weighting likelihood output
##'
##' @param params.file base parameter file
##' @param wgts location of the reweighting folder
##' @param likelihood likelihood file
##' @param lik.pre strings matching the likelihood output
##' @param params.pre strings matching the parameter estimates
##' @param parallel should the files be read in parallel
##'
##' @return data.frame with parameter estimates and likelihood output from the iterative reweighting folder.
read.gadget.wgts <- function(params.file = 'params.in',
                             wgts = 'WGTS',
                             likelihood = 'likelihood',
                             lik.pre = 'lik.',
                             params.pre = 'params.',
                             parallel=FALSE){
  
  
  params.in <- read.gadget.parameters(params.file)
  bs.lik <- read.gadget.likelihood(likelihood)
  files <- unique(list.files(wgts))
  
  lik.pre.g <- paste('^',gsub('.','[^a-zA-Z]',lik.pre,fixed=TRUE),sep='')
  params.pre.g <- paste('^',gsub('.','[^a-zA-Z]',params.pre,fixed=TRUE),sep='')
  
  liks <- unique(files[grep(lik.pre.g,files)])
  comps <- gsub(lik.pre.g,'',liks)
  
  tmp.func <- function(path){
    read.gadget.SS <- function(file='lik.out'){
      lik.out <- read.gadget.lik.out(file)
      
      if(is.null(lik.out)){
        SS <- as.data.frame(t(rep(NA,length(bs.lik$weights$weight))))
        names(SS) <- bs.lik$weights$name
      } else {
        SS <- lik.out$data[intersect(bs.lik$weights$name,names(lik.out$data))]
      }
      return(SS)
    }
    path.f <- list.files(path)
    liks <- path.f[grep(lik.pre,path.f)]
    params <- path.f[grep(params.pre.g,path.f)]
    plyr::ldply(intersect(comps,unique(c(gsub(params.pre.g,'',params),
                                         'init'))),
                function(x){
                  if(x=='init')
                    tmp <- params.in
                  else
                    tmp <- read.gadget.parameters(sprintf('%s/%s%s',
                                                          path,params.pre,x))
                  if(is.null(tmp)){
                    tmp <- params.in
                    tmp$value <- NA*tmp$value
                    ss <- as.data.frame(t(rep(NA,length(bs.lik$weights$weight))))
                    names(ss) <- bs.lik$weights$name
                  } else {
                    ss <- read.gadget.SS(sprintf('%s/%s%s',path,lik.pre,x))
                  }
                  optim  <- plyr::ldply(attributes(tmp)$optim.info,
                                        function(x) cbind(fake.id=1,x))
                  optim <- stats::reshape(optim,idvar='fake.id',
                                          timevar='.id',direction='wide')
                  optim$fake.id <- NULL
                  dtmp <- cbind(bs.data=utils::tail(unlist(strsplit(path,'/')),1),
                                comp=x,
                                t(tmp['value']),
                                ss,
                                optim)
                  
                  return(dtmp)
                }
    )
  }
  dparam <- plyr::ldply(wgts,tmp.func,.parallel=parallel)
  attr(dparam,'init.param') <- params.in
  return(dparam)
}













##' @rdname gadgetFitHelpers
##' @description \code{read.gadget.grouping} reads the the likelihood grouping from the WGTS folder
##' @param lik old style gadget likelihood object
##' @param wgts logcation of the WGTS folder
##' @return list of wgts groupings
read.gadget.grouping <- function(lik = read.gadget.likelihood(),
                                 wgts = 'WGTS'){
  lik.tmp <- subset(lik$weights,
                    !(lik$weights$type %in% c('penalty','understocking',
                                              'migrationpenalty','catchinkilos')))
  
  tmp <- 
    tryCatch(
      plyr::ldply(lik.tmp$name,
                  function(x){
                    text <- gsub('params.','',
                                 grep('params',list.files(wgts),
                                      value = TRUE))
                    x1 <- gsub('.','\\.',x,fixed=TRUE)
                    x1 <- paste('([[:punct:]]|^)',x1,'([[:punct:]]|$)',
                                sep='')
                    pos <- grep(x1,text)
                    data.frame(name = x,
                               pos = pos,
                               ord = regexpr(x,text[pos])[1],
                               stringsAsFactors=FALSE)
                  }),
      error=function(e) stop('Error when reading likelihood grouping from WGTS, some components are missing. 
                           Has iterative reweighting been completed?'))
  tmp <- dplyr::arrange(tmp,.data$pos,.data$ord)
  grouping <- plyr::dlply(tmp,~pos,function(x) as.character(x$name))
  names(grouping) <- unlist(plyr::llply(grouping,function(x) paste(x,collapse='.')))
  attributes(grouping)$split_labels <- NULL
  return(grouping)
}




write.unix <- function(x,f,append=FALSE,...){
  f <- file(f,open=ifelse(append,'ab','wb'))
  write(x,file=f,...)
  close(f)
}

write.gadget.table <- function(x,file='',append=FALSE,...){
  f <- file(file,open=ifelse(append,'ab','wb'))
  utils::write.table(x,file=f,eol='\n',...)
  close(f)
}
