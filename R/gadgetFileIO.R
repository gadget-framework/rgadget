
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
    if(length(tmp) == 0){
      if(!suppress)
        warning(sprintf('Warning in read.printfile -- %s is of length 0',file))
      return(NULL)
    }
    header <- 
      max(grep(';',tmp[1:5])) %>% 
      tmp[.] %>% 
      gsub('; (*)','\\1',.) %>%
      gsub(' ','.',.) %>% 
      gsub('\\]|\\[','',.) %>% 
      gsub('-',' ',.) %>% 
      scan(text = .,what = 'character',quiet = TRUE)
    
    data <- tryCatch(read.table(file,comment.char=';',header=FALSE,
                                stringsAsFactors = FALSE),
                     error = function(e){
                       if(!suppress)
                         print(sprintf('file corrupted -- %s', file))
                       return(NULL)
                     })
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
        map(function(x) data.frame(areas=rep(x$areas,x$n))) %>%
        bind_rows()
      
      regr.txt <- 
        tmp[-c(1:min(pos-1),pos)] %>%
        gsub('; ','',.) %>%
        paste(.,areas$areas)
      
      regr <- read.table(text=regr.txt,stringsAsFactors = FALSE)[c(1,3,5,7,8)]
      names(regr) <- c('label','intercept','slope','sse','area')
      data <- merge(data,regr)
  #    data <- mutate(data,
  #                   predict = exp(intercept)*number^slope) ## 1000 hmm
    }
    # pos <- grep('; Standard output file for the stock',tmp)
    # if(length(pos) != 0){
    #   step1 <- data[c('year','step','age','area','number')]
    #   next1 <- mutate(subset(step1,age>1),year=year-1,age=as.integer(age-1))
    #   names(next1)[5] <- 'num.after.harv'
    #   tmp <- merge(step1,next1)
    #   tmp$Z <- log(tmp$number) - log(tmp$num.after.harv)
    #   data <- merge(data,tmp[c('year','step','age','area','Z')],all.x=TRUE)
    # }

    return(data)
  }
  out.files <- list.files(path=path,
                          full.names=TRUE,recursive=TRUE)

  printfiles <- plyr::llply(out.files,read.printfile)
  names(printfiles) <- gsub('/','',gsub(path.expand(path),'',
                                        out.files),fixed=TRUE)
  class(printfiles) <- c('gadgetOut','list')
  return(printfiles)
}
##' \code{read.gadget.likelihood} reads the likelihood (input) file for gadget. The format of
##' the likelihood file is described in gadget's user manual.
##' @title Old style gadget file input and output (mostly deprecated)
##' @rdname gadgetFileIO 
##' @param files a vector of character strings containing the names of the likelihood files
##' @return object of class gadget.likelihood, i.e. a list containing the various likelihood components
##' @author Bjarki ??r Elvarsson
##' @export
read.gadget.likelihood <- function(files='likelihood'){
  lik <- NULL
  for(file in files){
    lik <- c(lik,sub(' +$','',gsub('\t',' ',readLines(file))))
  }
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
          names.tmp <- head(tmp,1)
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
##' @author Bjarki ??r Elvarsson
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
        
    
    comp <- na.omit(reshape2::melt(merge(weights,comp,by='name',sort=FALSE),
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

##' \code{merge.gadget.likelihood} merges to likelihood objects in one
##' @rdname gadgetFileIO
##' @param lik1 likelihood object
##' @param lik2 likelihood object
##' @return merged gadget likelihood object
##' @author Bjarki Thor Elvarsson
merge.gadget.likelihood <- function(lik1,lik2){
  tmp <- within(list(),
                for(comp in unique(c(names(lik1),names(lik2)))){
                  assign(comp,
                         unique(dplyr::bind_rows(lik1[[comp]],lik2[[comp]])))
                })
  class(tmp) <- c('gadget.likelihood',class(tmp))
  tmp$comp <- NULL
  return(tmp)
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
    weights <- subset(likelihood$weights,!(name %in% comp))
  else
    weights <- subset(likelihood$weights,name %in% comp)
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
##' @author Bjarki ??r Elvarsson
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
##' @author Bjarki ??r Elvarsson
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
          ifelse(is.null(main$likelihoodfiles), # | main$likelihoodfiles == '',
                 '[fleet]',
                 paste('[fleet]\nfleetfiles',
                       paste(main$fleetfiles,collapse='\t'))),
          '[likelihood]',
          paste('likelihoodfiles',
                paste(main$likelihoodfiles,collapse='\t')),
          sep='\n')
  write.unix(main.text,f=file)
  invisible(main.text)
}

##' \code{clear.spaces} clears tab and spaces from a string and return a list or a matrix of values
##' @rdname gadgetFileIO
##' @param text string
##' @return list or matrix containing the (non-empty) values from the string
##' @author Bjarki ??r Elvarsson
clear.spaces <- function(text){
  sapply(strsplit(sapply(strsplit(text,'[ \t]'),
                         function(x) {
                           paste(x[!(x==''|x=='\t')],
                                 collapse=' ')
                         }),' '),
         function(x) x)
}


##' \code{read.gadget.parameters} reads a gadget parameter file
##' @title Gadget parameters
##' @rdname gadget.parameters
##' @param file parameter file
##' @return dataframe
##' @author Bjarki  Thor Elvarsson
##' @export
read.gadget.parameters <- function(file='params.in'){

  params <- tryCatch(read.table(file,header=TRUE,
                       comment.char=';',
                       stringsAsFactors=FALSE),
                     error = function(e){
                       print(sprintf('Error in read.gadget.parameters -- %s cannot be read', file))
                       return(NULL)
                     })
  if(is.null(params))
    return(params)
  row.names(params) <- params$switch
  ## digg through the data written in the header
  header <- readLines(file)
  header <- header[grepl(';',substring(header,1,1))]

  num.func <- function(pre){
    post <- ' function evaluations'
    num <- as.numeric(gsub(post,'',gsub(pre,'',header[grepl(pre,header)])))
    num <- ifelse(length(num) == 0,NA,num)
    return(num)
  }

  ## Number of function evaluations
  sim.func.str <- '; Simulated Annealing algorithm ran for '
  sim.pos <- grep(sim.func.str,header)

  hook.func.str <- '; Hooke & Jeeves algorithm ran for '
  hook.pos <- grep(hook.func.str,header)

  bfgs.func.str <- '; BFGS algorithm ran for '
  bfgs.pos <- grep(bfgs.func.str,header)

  ## final likelihood values from each component
  lik.func <- function(i){
    lik <- as.numeric(gsub('; and stopped when the likelihood value was ','',
                           header[i]))
    lik <- ifelse(length(lik) == 0,NA,lik)
    return(lik)
  }

  ## convergence
  conv.func <- function(i){
    error <- '; because an error occured during the optimisation'
    converged <- '; because the convergence criteria were met'
    maxiter <-
      '; because the maximum number of function evaluations was reached'
    msg <- ifelse(header[i]==error,'Error in optimisation',
                  ifelse(header[i]==converged,'Convergence criteria were met',
                         ifelse(header[i]==maxiter,
                                'Maximum number of iterations',
                                'No information')))
    ifelse(length(msg)==0,NA,msg)
  }

  safe_dates <- 
    quietly(~gsub('; Gadget version .+ running on [A-Za-z].+','',.) %>% 
             lubridate::parse_date_time('%a %b! %d! %H!:%M!:%S! %Y!'))
  
  date <- safe_dates(header[1]) %>% .$result
    

  tmp <- list(simann=data.frame(numFunc=num.func(sim.func.str),
                lik.val=lik.func(sim.pos+1),
                convergence=conv.func(sim.pos+2),
                stringsAsFactors=FALSE),
              hooke=data.frame(numFunc=num.func(hook.func.str),
                lik.val=lik.func(hook.pos+1),
                convergence=conv.func(hook.pos+2),
                stringsAsFactors=FALSE),
              bfgs=data.frame(numFunc=num.func(bfgs.func.str),
                lik.val=lik.func(bfgs.pos+1),
                convergence=conv.func(bfgs.pos+2),
                stringsAsFactors=FALSE))
  class(params) <- c('gadget.parameters',class(params))
  attr(params,'optim.info') <- tmp
  attr(params,'data') <- date 
  return(params)
}

##' \code{write.gadget.parameters} writes gadget input parameters
##' @rdname gadget.parameters 
##' @param params params dataframe
##' @param file a string naming the file to write to
##' @param columns should a conventional column based parameter file be written out or
##' should a line based parameter (used when evaluating gadget on a matrix) be used.
##' @return a string containing the text of the params file (if desired)
##' @author Bjarki ??r Elvarsson
##' @export
write.gadget.parameters <- function(params,file='params.out',columns=TRUE){
  input.text <-
    paste("; input file for the gadget model",
          "; created automatically from Rgadget",
          sprintf('; %s - %s',file,date()),
          paste(names(params),collapse='\t'),
          sep='\n')

  if(!columns)
    write.unix(paste(c('switches',names(params)),collapse='\t'),f=file)
  else
    write.unix(input.text,file)
  write.gadget.table(params,file=file,
              quote=FALSE, row.names=FALSE, col.names=FALSE,
              append=TRUE, sep="\t")
             

}


##' @title Make gadget printfile
##' @param main location of main file
##' @param output location of output folder
##' @param aggfiles location of aggfiles
##' @param file name of resulting printfile
##' @return gadget.mainfile object
##' @author Bjarki Thor Elvarsson
make.gadget.printfile <- function(main='main',output='out',
                                  aggfiles='print.aggfiles',
                                  file='printfile',
								  printatstart = 1,
								  steps = 1){
    
    main <- read.gadget.main(main)
    lik <- read.gadget.likelihood(main$likelihoodfiles)
    stocks <- read.gadget.stockfiles(main$stockfiles)
    fleets <- read.gadget.fleet(main$fleetfiles)
    
    header <-
        paste(sprintf('; gadget printfile, created in %s',Sys.Date()),
              '[component]',
              'type\tlikelihoodsummaryprinter',
              sprintf('printfile\t%s/likelihoodsummary', output),
              ';',sep='\n')
    
    
    lik.template <-
        paste('[component]',
              'type\tlikelihoodprinter',
              'likelihood\t%1$s',
              sprintf('printfile\t%s/%%1$s',output),
              ';', sep='\n')
    
    
    stock.std <-
        paste('[component]',
              'type\tstockstdprinter',
              'stockname\t%1$s',
              sprintf('printfile\t%s/%%1$s.std',output),
              sprintf('printatstart %s', printatstart),
              sprintf('yearsandsteps\tall\t%s', steps),sep='\n')
    
    stock.full <-
        paste('[component]',
              'type\tstockprinter',
              'stocknames\t%1$s',
              sprintf('areaaggfile\t%s/%%1$s.area.agg',aggfiles),
              sprintf('ageaggfile\t%s/%%1$s.allages.agg',aggfiles),
              sprintf('lenaggfile\t%s/%%1$s.len.agg',aggfiles),
              sprintf('printfile\t%s/%%1$s.full',output),
              sprintf('printatstart\t%s', printatstart),
              sprintf('yearsandsteps\tall\t%s', steps),sep='\n')
    
    predator <-
        paste('[component]',
              'type\tpredatorpreyprinter',
              'predatornames\t%2$s',
              'preynames\t%1$s',
              sprintf('areaaggfile\t%s/%%1$s.area.agg',aggfiles),
              sprintf('ageaggfile\t%s/%%1$s.age.agg',aggfiles),
              sprintf('lenaggfile\t%s/%%1$s.alllen.agg',aggfiles),
              sprintf('printfile\t%s/%%1$s.prey',output),
              'yearsandsteps\tall all',
              sep = '\n')
    
    predator.prey <-
        paste('[component]',
              'type\tpredatorpreyprinter',
              'predatornames\t%2$s',
              'preynames\t%1$s',
              sprintf('areaaggfile\t%s/%%1$s.area.agg',aggfiles),
              sprintf('ageaggfile\t%s/%%1$s.allages.agg',aggfiles),
              sprintf('lenaggfile\t%s/%%1$s.len.agg',aggfiles),
              sprintf('printfile\t%s/%%1$s.prey.%%2$s',output),
              'yearsandsteps\tall all',
              sep = '\n')
    
    prey.subset <- stocks[which(lapply(stocks, function(x) x@iseaten) == 1)]
    
    tmp <- expand.grid(preys = names(prey.subset),
                       predators = c(fleets$fleet$fleet, stocks %>% 
                                       purrr::set_names(.,names(.))%>% 
                                       purrr::keep(~isPredator(.)==1) %>% 
                                       unlist() %>% 
                                       names()))
    
    
    dir.create(aggfiles, showWarnings = FALSE)
    dir.create(output, showWarnings = FALSE)
    
    plyr::l_ply(stocks,
          function(x){
              writeAggfiles(x,folder=aggfiles)
          })
    
    txt <- sprintf(lik.template,
                   subset(lik$weights,
                          !(type %in% c('understocking','penalty',
                                        'migrationpenalty')))[['name']])
    write.unix(paste(header,paste(txt,collapse='\n'),
                     paste(sprintf(stock.std,plyr::laply(stocks,
                                                   function(x) x@stockname)),
                           collapse='\n'),
                     paste(sprintf(stock.full,plyr::laply(stocks,
                                                    function(x) x@stockname)),
                           collapse='\n'),
                     paste(sprintf(predator,plyr::laply(prey.subset,
                                                  function(x) x@stockname),
                                   paste(fleets$fleet$fleet,collapse = ' ')),
                           collapse='\n'),
                     paste(sprintf(predator.prey,tmp$preys,tmp$predators),
                           collapse='\n'),
                     ';',
                     sep='\n'),
               f=file)
}

##' Read gadget printfile
##' @title Read gadget printfile
##' @param file string containing the path to the printfile
##' @return list of the prinfile components.
##' @author Bjarki Thor Elvarsson
read.gadget.printfile <- function(file='printfile'){
  printfile <- strip.comments(file)
  comp.loc <- grep('component',printfile)
  name.loc <- grep('printfile',printfile)
  name.print <- sapply(printfile[grep('printfile',printfile)],function(x) x[2])
  name.print <- sapply(strsplit(name.print,'/'),function(x) tail(x,1))
  diff.comp <- diff(c(comp.loc,length(printfile)+1))-1
#  type.loc <- grep('type',printfile)
#  types <- unique(sapply(printfile[type.loc],function(x) x[2]))
#  print.types <- llply(types,function(x) grep(x,printfile))
#  names(print.types) <- types

  tmp.func <- function(restr){
    names.tmp <- sapply(printfile[restr],
                        function(x) x[1])
    tmp <- lapply(sapply(printfile[restr],
                         function(x) x[-1]),unlist)
    names(tmp) <- names.tmp
    return(tmp)
  }

  print <- plyr::llply(1:length(comp.loc),
                 function(x) tmp.func(comp.loc[x]+1:diff.comp[x]))
  names(print) <- name.print
  return(print)
}
##' Write the gadget prinfile to file, optionally changing the output directory
##' of the printfile components.
##' @title Write Gadget printfile
##' @param print printfile object
##' @param file string containing the desired location of the printfile
##' @param output.dir where should the output from the prinfile components be written, defaults to 'out'.
##' @return (invisible) text of the printfile if desired.
##' @author Bjarki Thor Elvarsson
write.gadget.printfile <- function(print,file='prinfile',output.dir='out'){
  print.text <- sprintf('; Printfile for gadget, created by Rgadget\n; %s',file)
  for(name in names(print)){
    tmp <- print[name][[name]]
    tmp[['printfile']] <- paste(output.dir,name,sep='/')
    print.text <- paste(print.text,
                        ';\n[component]',
                        paste(names(tmp),sapply(tmp,function(x) paste(x,collapse='\t')),
                              sep='\t',collapse='\n'),
                        sep='\n')
  }
  write.unix(print.text,file)
  invisible(print.text)
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
  comp.tmp <- subset(likelihood$weights,
                     !(type %in% c('penalty','understocking',
                                   'migrationpenalty','catchinkilos'))&
                     !(name %in% unlist(grouping)))$name
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
##' @return list of dataframes and degress of freedom
##' @author Bjarki ??r Elvarsson
##' @export
read.gadget.data <- function(likelihood,debug=FALSE,year_range=NULL){
  read.agg <- function(x, first = FALSE){      
      if(first){
          return(sapply(strsplit(readLines(x),'[\t ]'),function(x) x[1]))
      }  else {
          return(read.table(x,stringsAsFactors=FALSE,comment.char=';'))
      }
  }

  read.preyagg <- function(x){
    tmp <- readLines(x)
    loc <- grep('lengths',tmp)
    tmp2 <- read.table(text=tmp[grepl('lengths',tmp)])
    tmp2$V1 <- clear.spaces(tmp[loc-2])
    return(tmp2)
  }

  read.func <- function(x){
      if(debug){
          print(sprintf('reading datafile %s',x$datafile))
      }
      dat <- tryCatch(read.table(x$datafile,comment.char=';',stringsAsFactors = FALSE),
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
        dplyr::filter(year %in% year_range)
    }
    
    attr(dat,'len.agg') <- len.agg
    attr(dat,'pred.agg') <- len.agg
    attr(dat,'age.agg') <- age.agg
    attr(dat,'prey.agg') <- prey.agg
    attr(dat,'area.agg') <- area.agg
    return(dat)
  }

  lik.dat <- plyr::dlply(subset(likelihood$weights,
                          !(type %in% c('penalty', 'understocking',
                                        'migrationpenalty'))),
                   'type',
                   function(x) plyr::dlply(x,'name',read.func))

  df <- lapply(lik.dat,function(x)
              sapply(x,function(x){
                x <- na.omit(x)
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

##' @description \code{read.gadget.optinfor} reads optinfo parameters from file
##' @rdname gadget.optinfo
##' @title Gadget optimiser settings
##' @param file location of the optinfofile
##' @return optinfo object
##' @author Bjarki ??r Elvarsson
##' @export
read.gadget.optinfo <- function(file='optinfofile'){
  optinfo <- readLines(file)
  optinfo <- na.omit(sapply(strsplit(optinfo,';'),function(x) x[1]))
  simann <- (1:length(optinfo))[(optinfo == '[simann]')]
  hooke <- (1:length(optinfo))[(optinfo == '[hooke]')]
  bfgs <- (1:length(optinfo))[(optinfo == '[bfgs]')]

  vars <- c(simann-1,hooke-1,bfgs-1,length(optinfo))
  simann.end <- min(vars[vars>simann])
  hooke.end <-  min(vars[vars>hooke])
  bfgs.end <- min(vars[vars>bfgs])
  tmp.func <- function(start,end){
    x <-  as.data.frame(clear.spaces(optinfo[start:end]),
                        stringsAsFactors=FALSE)
    names(x) <- x[1,]
    x <- x[2,]
    return(x)
  }
  optinfo <- list(simann = tmp.func(simann+1,simann.end),
                  hooke = tmp.func(hooke+1,hooke.end),
                  bfgs = tmp.func(bfgs+1,bfgs.end))
  class(optinfo) <- c('gadget.optinfo',class(optinfo))
  return(optinfo)
}

##' @description \code{write.gadget.optinfo} write optinfo to file
##' @rdname gadget.optinfo
##' @param optinfo optinfo object
##' @param file file
##' @param location location
##' @return text of the optinfofile (if desired)
##' @author Bjarki ??r Elvarsson
write.gadget.optinfo<-function(optinfo,file='optinfofile'){
  opt.text <-
    paste("; optimisation file for gadget",
          "; created in R-gadget",
          sprint('; %s - %s',file,date()),
          sep='\n')
  for(comp in names(optinfo)){
    opt.text <-
      paste(opt.text,
            sprintf('[%s]',comp),
            paste(names(optinfo[[comp]]),
                  optinfo[[comp]],
                  sep='\t\t',collapse='\n'),
            sep='\n')
  }
  write.unix(opt.text,f=file)
  invisible(opt.text)
}



##' Read in the gadget likelihood output.
##' @title Read gadget lik.out
##' @param file string containing the name of the file
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

  data <- read.table(file,skip=(i2+1))
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


##' Read in a Gadget model to a gadget-main object DEPRECATED
##' @title read gadget main
##' @param main.file location of the main file
##' @return mainfile object
##' @author Bjarki Thor Elvarsson
read.gadget.model <- function(main.file='main',model.name='Gadget-model'){
  main <- read.gadget.main(main.file)
  time <- read.gadget.time(main$timefile)
  time <- new('gadget-time',
              firstyear = time$firstyear,
              firststep = time$firststep,
              lastyear = time$lastyear,
              laststep = time$laststep,
              notimesteps = time$notimesteps)

  area <- read.gadget.area(main$areafile)
  area <- new('gadget-area',
              areas = as.numeric(area$areas),
              size = as.numeric(area$size),
              temperature = area$temperature)
  fleets <- read.gadget.fleet(main$fleetfiles)
  fleets <- plyr::dlply(fleets$fleet,~fleet,
                  function(x){
                    fleetdat <-
                      subset(read.gadget.table(x$amount[1]),
                             V4 == x$fleet)
                    if(x$type[1]!='linearfleet'){
                      names(fleetdat) <-
                        c('year','step','area','fleetname','amount')
                    } else{
                      names(fleetdat) <-
                        c('year','step','area','fleetname','Fy')

                    }
                    suitability <- merge(x['fleet'],fleets$prey)
                    new('gadget-fleet',
                    name = x$fleet[1],
                    type = x$type[1],
                    multiplicative = x$multiplicative[1],
                    amount = fleetdat,
                    suitability=suitability)
                  })


  gadget.model <-
    new('gadget-main',
        model.name = model.name,
        time = time,
        area = area,
        print = ifelse(is.null(main$printfile),list(),
          read.gadget.printfile(main$printfile)),
        stocks = read.gadget.stockfiles(main$stockfiles),
        tags = new('gadget-tagging'),
        otherfood = ifelse(is.null(main$otherfoodfiles),list(),
                           read.gadget.otherfood(main$otherfoodfiles)),
        fleets = fleets,
        likelihood = as.list(read.gadget.likelihood(main$likelihoodfiles))
        )
  return(gadget.model)
}


##' @rdname gadgetFileIO
##' @description \code{read.gadget.stockfiles} reads in Gadget stockfiles DEPRECATED
##' @param stock.files location of stock file
##' @return list of gadget-stock objects
read.gadget.stockfiles <- function(stock.files){
  tmp.func <- function(sf){
    stock <- strip.comments(sf)

    growth.loc <- grep('doesgrow', stock, ignore.case = TRUE)
    mort.loc <- grep('naturalmortality', stock, ignore.case = TRUE)
    init.loc <- grep('initialconditions', stock, ignore.case = TRUE)
    pred.loc <- init.loc - 1
    if(tolower(stock[init.loc+1])=='numbers'){
      init.loc <- init.loc + 1
    }
#    initfile.loc <- #grep('normalcondfile',stock, ignore.case =TRUE)
#      c(grep('normalcondfile',stock, ignore.case =TRUE),
#        grep('normalparamfile',stock, ignore.case =TRUE),
#        grep('numberfile',stock, ignore.case =TRUE))
    eat.loc <- grep('doeseat', stock, ignore.case = TRUE)
    migrate.loc <- grep('doesmigrate', stock, ignore.case = TRUE)
    initfile.loc <- migrate.loc -1
    mature.loc <- grep('doesmature', stock, ignore.case = TRUE)
    move.loc <- grep('doesmove', stock, ignore.case = TRUE)
    renew.loc <- grep('doesrenew', stock, ignore.case = TRUE)
    spawn.loc <- grep('doesspawn', stock, ignore.case = TRUE)
    stray.loc <- grep('doesstray', stock, ignore.case = TRUE)

    growth.info <- function(tmp){
      if(length(tmp)==1)
        tmp <- new('gadget-growth')
      else {
        names.tmp <- sapply(tmp,function(x) x[1])
        tmp <- plyr::llply(tmp,function(x) paste(x[-1],collapse=' '))
        names(tmp) <- names.tmp

        if(is.null(tmp$growthparameters))
          tmp$growthparameters <- vector()
        if(is.null(tmp$growthfunction))
          tmp$growthfunction <- vector()
        if(is.null(tmp$wgrowthfunction))
          tmp$wgrowthfunction <- vector()
        if(is.null(tmp$lgrowthfunction))
          tmp$lgrowthfunction <- vector()
        if(is.null(tmp$yeareffect))
          tmp$yeareffect <- vector()
        if(is.null(tmp$stepeffect))
          tmp$stepeffect <- vector()
        if(is.null(tmp$areaeffect))
          tmp$areaeffect <- vector()
        if(tolower(tmp$growthfunction) == 'lengthvbsimple'){
          tmp <- new('gadget-growth',
                     growthfunction = tmp$growthfunction,
                     ## growthfunction parameters
                     growthparameters = tmp$growthparameters,
#                   wgrowthparameters = tmp$wgrowthparameters,
#                   lgrowthparameters = tmp$lgrowthparameters,
#                   yeareffect = tmp$yeareffect,
#                   stepeffect = tmp$stepeffect,
#                   areaeffect = tmp$areaeffect,
                   ## growth implementation
                   beta = tmp$beta,
                   maxlengthgroupgrowth = tmp$maxlengthgroupgrowth)
        } else {
          tmp <- new('gadget-growth',
                     growthfunction = tmp$growthfunction,
                     ## growthfunction parameters
                     growthparameters = tmp$growthparameters,
                     wgrowthparameters = tmp$wgrowthparameters,
                     lgrowthparameters = tmp$lgrowthparameters,
                     ##                   yeareffect = tmp$yeareffect,
                     ##                   stepeffect = tmp$stepeffect,
                     ##                   areaeffect = tmp$areaeffect,
                     ## growth implementation
                     beta = tmp$beta,
                     maxlengthgroupgrowth = tmp$maxlengthgroupgrowth)
        }
      }
      return(tmp)
    }

    prey.info <- function(tmp){
      if(length(tmp)==1){
        tmp <- new('gadget-prey')
      } else {
        tmp <- new('gadget-prey',
                   name = stock[[1]][2],
                   preylengths = read.table(tmp[[2]][2],comment.char=';'),
                   energycontent = ifelse(length(tmp)==3,as.numeric(tmp[[3]][2]),
                     1))
      }
      return(tmp)
    }

    pred.info <- function(tmp){
      if(length(tmp)==1){
        tmp <- new('gadget-predator')
      } else {
        suit.loc <- grep('suitability',tmp)
        pref.loc <- grep('preference',tmp)
        maxcon.loc <- grep('maxconsumption',tmp)
        half.loc <- grep('halffeedingvalue',tmp)
        suit <- plyr::ldply((suit.loc+1):(pref.loc-1),
                      function(x){
                        c(stock = tmp[[x]][1],
                          suitability = paste(tmp[[x]][-1],collapse=' '))
                      })
        pref <- plyr::ldply((pref.loc+1):(maxcon.loc-1),
                      function(x){
                        c(stock = tmp[[x]][1],
                          preference = paste(tmp[[x]][-1],collapse=' '))
                      })
        tmp <- new('gadget-predator',
                   suitability = suit,
                   preference = pref,
                   maxconsumption = paste(tmp[[maxcon.loc]][-1], collapse = ' '),
                   halffeedingvalue = paste(tmp[[half.loc]][2], collapse = ' '))
      }
      return(tmp)
    }

    initialdata <- read.gadget.table(stock[[initfile.loc]][2])
    if(length(names(initialdata)) == 7){
      names(initialdata) <- c('age','area','age.factor','area.factor',
                              'mean','stddev','relcond')
    } else if (length(names(initialdata)) == 8){
      names(initialdata) <- c('age','area','age.factor','area.factor',
                              'mean','stddev','alpha','beta')
    } else if (length(names(initialdata)) == 5){
      names(initialdata) <- c('area','age','length','number','weight')
    }
    renewal.data <-
      tryCatch(read.gadget.table(stock[[renew.loc+3]][2]),
               error=function(x){
                 tryCatch(read.gadget.table(stock[[renew.loc+4]][2]),
                          error=function(x) data.frame(text='No renewal data'))
               })
    if(length(names(renewal.data)) == 8){
      names(renewal.data) <- c('year','step','area','age','number',
                               'mean','stddev','relcond')
    } else if (length(names(renewal.data)) == 9){
      names(renewal.data) <- c('year','step','area','age','number',
                               'mean','stddev','alpha','beta')
    } else if (length(names(renewal.data)) == 6){
      names(renewal.data) <- c('year','area','age','length','number','weight')
    }
    doesmature <- as.numeric(stock[[mature.loc]][2])
    if(doesmature == 1){
      maturity.function <- tolower(stock[[mature.loc+1]][2])
      maturity.file <- strip.comments(stock[[mature.loc+2]][2])
      maturestocksandratios <- (maturity.file[[1]][-1])
      coefficients <- (maturity.file[[2]][-1])
      maturitylengths <- ''
      maturitysteps <- ''
      if(tolower(maturity.function) == 'fixedlength'){
          maturitylengths <- (maturity.file[[3]][-1])
      } else if(tolower(maturity.function) != 'continuous'){
          maturitysteps <- (maturity.file[[3]][-1])
      } 
  
    } else {
      maturity.function <- ''
      maturestocksandratios <- ''
      coefficients <- ''
      maturitylengths <- ''
      maturitysteps <- ''
  }
    doesmove <- as.numeric(stock[[move.loc]][2])
    if(doesmove == 1){
        transitionstocksandratios <- (stock[[move.loc+1]][-1])
        transitionstep <- as.numeric(stock[[move.loc+2]][-1])
    } else {
      transitionstocksandratios <- ''
      transitionstep <- 0
    }

    doesmigrate <- as.numeric(stock[[migrate.loc]][2])
    if(doesmigrate == 1){
        yearstep <- read.table(stock[[migrate.loc+1]][-1],
                               comment.char = ';',
                               stringsAsFactors=FALSE)
        tmp <- strip.comments(stock[[migrate.loc+2]][-1])
        mat.loc <- grep('[migrationmatrix]',tmp,fixed=TRUE)
        tyler <- diff(mat.loc)[1]
	if(length(tyler) == 0){
 	   tyler <- length(tmp)-1
	}
        migrationratio <-
          plyr::llply(mat.loc,  ## all puns intended;)
                function(x){
                  plyr::laply((x+2):(x+tyler-1),
                        function(y){merge.formula(tmp[[y]])})
                })
        names(migrationratio) <- plyr::laply(mat.loc,function(x){ tmp[[x+1]][2]})
    } else {
      migrationratio <- list()
#      transitionstocksandratios <- ''
      yearstep <- data.frame()
    }

    doesspawn <- as.numeric(stock[[spawn.loc]][2])
    if(doesspawn == 1){
      tmp <- strip.comments(stock[[spawn.loc+1]][2])
      ssar <- tmp[[5]][-1]
      nssar <- 2*(1:(length(ssar)/2))-1
      ssar <- data.frame(stock=ssar[nssar],ratio=ssar[1+nssar])
      stockparameters <- as.data.frame(t(merge.formula(tmp[[10]][-1])))
      names(stockparameters) <- c('mean','std.dev','alpha','beta')
      spawning <-
        new('gadget-spawning',
            spawnsteps = as.numeric(tmp[[1]][-1]),
            spawnareas = as.numeric(tmp[[2]][-1]),
            firstspawnyear = as.numeric(tmp[[3]][-1]),
            lastspawnyear = as.numeric(tmp[[4]][-1]),
            spawnstocksandratio = ssar,
            proportionfunction = merge.formula(tmp[[6]][-1]),
            mortalityfunction = merge.formula(tmp[[7]][-1]),
            weightlossfunction = merge.formula(tmp[[8]][-1]),
            recruitment = merge.formula(tmp[[9]][-1]),
            stockparameters = stockparameters)


    } else {
      spawning <- new('gadget-spawning')
    }




    st <-
      new('gadget-stock',
          stockname = stock[[1]][2],
          livesonareas = as.numeric(stock[[2]][-1]),
          minage = as.numeric(stock[[3]][2]),
          maxage = as.numeric(stock[[4]][2]),
          minlength = as.numeric(stock[[5]][2]),
          maxlength = as.numeric(stock[[6]][2]),
          dl = as.numeric(stock[[7]][2]),
          refweight = read.table(stock[[8]][2],comment.char=';'),
          growthandeatlengths = read.table(stock[[9]][2],comment.char=';'),
          doesgrow = as.numeric(stock[[growth.loc]][2]),
          growth = growth.info(stock[growth.loc:(mort.loc-1)]),
          naturalmortality = merge.formula(stock[[mort.loc]][-1]),
          iseaten = as.numeric(stock[[mort.loc+1]][2]),
          preyinfo = prey.info(stock[(mort.loc+1):(eat.loc-1)]),
          doeseat = as.numeric(stock[[eat.loc]][2]),
          predator = pred.info(stock[eat.loc:(pred.loc)]),
          initialconditions = list(minage = stock[[init.loc + 1]][2],
            maxage = stock[[init.loc + 2]][2],
            minlength = stock[[init.loc + 3]][2],
            maxlength = stock[[init.loc + 4]][2],
            dl = ifelse(tolower(stock[[init.loc + 5]][1])=='dl',
              stock[[init.loc + 5]][2],as.numeric(stock[[7]][2])),
            sdev = ifelse(tolower(stock[[init.loc + 6]][1])=='sdev',
              stock[[init.loc + 6]][2], 1)),
          initialdata = initialdata,
          doesmigrate = as.numeric(stock[[migrate.loc]][2]),
          yearstep = yearstep,
          migrationratio = migrationratio,
          doesmature =  as.numeric(stock[[mature.loc]][2]),
          maturityfunction = maturity.function,
          maturestocksandratios = maturestocksandratios,
          coefficients = coefficients,
          maturitysteps = maturitysteps,
          maturitylengths = maturitylengths,
          doesmove = as.numeric(stock[[move.loc]][2]),
          transitionstocksandratios = transitionstocksandratios,
          transitionstep = transitionstep,
          doesrenew =  as.numeric(stock[[renew.loc]][2]),
          renewal = list(
            minlength = ifelse(as.numeric(stock[[renew.loc]][2]) == 0,
              '',
              as.numeric(stock[[renew.loc + 1]][2])),
            maxlength = ifelse(as.numeric(stock[[renew.loc]][2]) == 0,
              '',
              as.numeric(stock[[renew.loc + 2]][2]))),
          renewal.data = renewal.data,
          doesspawn = as.numeric(stock[[spawn.loc]][2]),
          spawning = spawning,
          doesstray = ifelse(length(stray.loc)==0,
            0,as.numeric(stock[[stray.loc]][2]))
          )

    return(st)
  }
  stocks <- plyr::llply(stock.files,tmp.func)
  names(stocks) <- plyr::laply(stocks,getStockNames)
  return(stocks)
}

##' @rdname gadgetFileIO
##' @description \code{read.gadget.area} reads the gadget area file
##' @param area.file location of the area file
##' @return areafile object
##' @author Bjarki Thor Elvarsson
read.gadget.area <- function(area.file='area'){
  area <- strip.comments(area.file)
  areas <- area[[1]][-1]
  size <- area[[2]][-1]
  temperature <-
    as.data.frame(t(sapply(area[-c(1:3)],function(x) as.numeric(x))))
  names(temperature) <- c('year','step','area','temperature')
  area <- list(areas=areas,size=size,temperature=temperature)
  class(area) <- c('gadget.area',class(area))
  return(area)
}


##' @rdname gadgetFileIO
##' @description \code{write.gadget.area} writes a gadget area file
##' @param area data frame with area and temperature
##' @param file location of the area file
##' @return nothing
##' @author Bjarki Thor Elvarsson
write.gadget.area <- function(area,file='area'){
  header <- sprintf('; time file created in Rgadget\n; %s - %s',file,date())
  area.file <-
    paste(header,
          paste('areas',paste(area$areas,collapse=' '),sep='\t'),
          paste('size',paste(area$size,collapse=' '),sep='\t'),
          'temperature',
          '; year - step - area - temperature',
          sep='\n')
  write.unix(area.file,f=file)
  write.gadget.table(area$temperature,file=file,col.names=FALSE,append=TRUE,
                     quote=FALSE,sep='\t',row.names=FALSE)
}

##' \code{read.gadget.time} reads gadget time file
##' @rdname gadgetFileIO
##' @param time.file location of the file
##' @return timefile objects
read.gadget.time <- function(time.file='time'){
  time <- strip.comments(time.file)
  time.names <- sapply(time,function(x) x[1])
  time <- sapply(time,function(x) as.numeric(x[-1]))
  names(time) <- time.names
  if(sum(time$notimesteps[-1])!=12)
    warning('Error in timefile - notimesteps does not sum to 12')
  if(length(time$notimesteps[-1])!=time$notimesteps[1])
    warning('Error in timefile - notimesteps does not contain the right number of timesteps')
  time$notimesteps <- time$notimesteps[-1]
  class(time) <- c('gadget.time',class(time))
  return(time)
}

##' \code{write.gadget.time} writes gadget time file
##' @rdname gadgetFileIO
##' @param time data.frame with time
##' @param file location of the time file
##' @return nothing
write.gadget.time <- function(time,file='time'){
  header <- sprintf('; time file created in Rgadget\n; %s - %s',file,date())
  time.file <-
    paste(header,
          paste('firstyear',time$firstyear,sep='\t'),
          paste('firststep',time$firststep,sep='\t'),
          paste('lastyear',time$lastyear,sep='\t'),
          paste('laststep',time$laststep,sep='\t'),
          paste('notimesteps',
                paste(length(time$notimesteps),
                      paste(time$notimesteps,collapse=' ')),
                sep='\t'),
          sep='\n')
  write.unix(time.file,f=file)
}




##' @rdname gadgetFitHelpers
##' @description \code{read.gadget.wgts} reads the output from iterative weighting likelihood output
##' @param params.file base parameter file
##' @param wgts location of the reweighting folder
##' @param likelihood likelihood file
##' @param lik.pre strings matching the likelihood output
##' @param params.pre strings matching the parameter estimates
##' @param parallel should the files be read in parallel
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
            optim <- reshape(optim,idvar='fake.id',
                             timevar='.id',direction='wide')
            optim$fake.id <- NULL
            dtmp <- cbind(bs.data=tail(unlist(strsplit(path,'/')),1),
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
##' @description \code{merge.formula} merges txt to gadget.formula
##' @param txt gadget formula text
##' @return txt where the formula has been properly compiled
merge.formula <- function(txt){
  openP <- grep('(',txt,fixed=TRUE)
  closeP <- grep(')',txt,fixed=TRUE)
  if(length(openP) + length(closeP) == 0)
    return(txt)

  if(length(openP) != length(closeP))
    stop('numbers of paranthesis dont match in gadget formula')

  braces <- data.frame(begin=openP,end=closeP,group=openP)
  for(i in 1:length(openP)){
    braces$end[i] <- closeP[i]
    braces$begin[i] <- openP[max(which(openP < closeP[i]))]
    openP[max(which(openP < closeP[i]))] <- length(txt)
  }
  braces <- arrange(braces, begin)
  for(i in 1:length(openP)){
    braces$group[braces$end<braces$end[i] & braces$end>braces$begin[i]] <-
      braces$group[i]
  }

  braces <- plyr::ddply(braces,'group',function(x) head(x,1))
  for(i in length(braces$group):1){
    txt[braces$begin[i]] <- paste(txt[braces$begin[i]:braces$end[i]],
                                  collapse=' ')
    txt <- txt[-c((braces$begin[i]+1):braces$end[i])]
  }
  return(txt)
}


##' @title gadget.fit helper functions
##' @description \code{eval.gadget.formula} Evaluate gadget formulas, which are in reverse polish notation, ie
##' '(* x y)' which is equivalent to 'x*y'. The evaluation supports the following
##' symbols '*','/','+','-','exp','log','sqrt'. The evaluation uses a gadget
##' parameter object for its evaluation.
##' @rdname gadgetFitHelpers
##' @param gad.for gadget formula
##' @param par gadget parameters object
##' @return a vector of evaluated gadget formulas
##' @author Bjarki Thor Elvarsson
eval.gadget.formula <- function(gad.for,par=data.frame()){
  tmp <- strsplit(gsub(')',' )',gsub('(','',gad.for,fixed=TRUE)),' ')
  plyr::ldply(tmp,
        function(x){
          x <- x[!x=='']
          x[x=='-'] <- "'-'("
          x[x=='+'] <- "'+'("
          par.ind <- grep('#',x,fixed=TRUE)
          x <- gsub("*","prod(",x,fixed=TRUE)
          x <- gsub("/","'/'(",x,fixed=TRUE)
          x <- gsub("+ ","sum(",x,fixed=TRUE)
          x <- gsub("- ","'-'(",x,fixed=TRUE)
          x <- gsub('exp','exp(',x,fixed = TRUE)
          x <- gsub('log','log(',x,fixed = TRUE)
          x <- gsub('sqrt','sqrt(',x,fixed = TRUE)
          ## remove initial values
          x <- gsub('[0-9]+.[0-9]+#|[0-9]+#','#',x)
          x[par.ind] <- par[gsub('#','',x[par.ind],fixed=TRUE),'value']
          x <- gsub(',)',')',gsub('(,','(',paste(x,collapse=','),fixed=TRUE),
                    fixed=TRUE)
          return(eval(parse(text=x)))
        })
}

##' \code{read.gadget.table} reads gadget tables
##' @rdname gadgetFileIO
##' @param file path to file
##' @param header logical, should the header be read from the file
##' @return data.frame 
read.gadget.table <- function(file,header=FALSE){
  dat <- strip.comments(file)
  if(class(dat) == 'list')
    gad.tab <- plyr::ldply(dat,merge.formula)
  else {
    gad.tab <- plyr::adply(dat,2,merge.formula)
    gad.tab$X1 <- NULL
  }
  if(header){
    comments <- attr(dat,'comments')
    header <- tail(comments,1)
    ## unfinised business
  }

  return(gad.tab)
}



##' @rdname gadgetFileIO
##' @description \code{read.gadget.fleet} read gadget fleet
##' @param fleet.file location of the fleet file
##' @return fleet file object
read.gadget.fleet <- function(fleet.file='fleet'){
  fleet <- strip.comments(fleet.file)
  comp.loc <- grep('fleetcomponent|component',fleet)
  suit.loc <- grep('suitability',fleet)
  fleet.dat <-
    data.frame(fleet = plyr::laply(fleet[comp.loc+1],function(x) x[2]),
               type = plyr::laply(fleet[comp.loc+1],function(x) x[1]),
               livesonareas = plyr::laply(fleet[comp.loc+2],
                 function(x) paste(x[-1],collapse=' ')),
               multiplicative = plyr::laply(fleet[comp.loc+3],
                 function(x) as.numeric(x[2])),
               amount =  plyr::laply(fleet[c(comp.loc[-1]-1,
                 length(fleet))],
                 function(x) x[2]),
               stringsAsFactors=FALSE
               )
  diff.suit <- data.frame(fleet=plyr::laply(fleet[comp.loc+1],function(x) x[2]),
                          begin=suit.loc+1,
                          end=c(comp.loc[-1]-2,length(fleet)-1))
  prey <- plyr::ddply(diff.suit,'fleet',
                function(x){
                  plyr::ldply(fleet[x$begin:x$end],
                        function(x)
                        c(stock=x[1],suitability=x[3],
                          params=paste(tail(x,-3),collapse=' ')))

                })
  return(list(fleet=fleet.dat,prey=prey))
}

##' \code{write.gadget.fleet} writes gadget fleet DEPRECATED
##' @rdname gadgetFileIO
##' @param fleet a gadget.fleet 
##' @param file location of the file
##' @return nothing
write.gadget.fleet <- function(fleet,file='fleet'){
  base.text <-
    paste('[fleetcomponent]',
          '%s\t%s',
          'livesonareas\t%s',
          'multiplicative\t%s',
          'suitability',
          '%s',
          'amount\t%s',
          sep='\n')

  suit.text <- plyr::ddply(fleet$prey,'fleet',
                     function(x){
                       c(suitability=paste(x$stock,'function',
                           x$suitability,x$params,
                           sep='\t', collapse='\n'))
                     })
  tmp <- merge(fleet$fleet,suit.text,by='fleet')
  tmp$suitability <- ifelse(tmp$type=='quotafleet',
                            paste(tmp$suitability,
                                  sprintf('quotafunction\t%s\nbiomasslevel\t%s\nquotalevel\t%s\nselectstocks\t%s',
                                          tmp$quotafunction,tmp$biomasslevel,
                                          tmp$quotalevel,
                                          tmp$selectstocks),
                                  sep='\n'),
                            tmp$suitability)

  write.unix(sprintf(base.text,tmp$type,tmp$fleet,tmp$livesonareas,
                     tmp$multiplicative,tmp$suitability, tmp$amount),
             f=file)

}



##' \code{get.gadget.growth} get gadget growth
##' @rdname gadgetFitHelpers
##' @param stocks gadget.stock
##' @param params gadget.parms
##' @param dt delta t
##' @param age.based logical
##' @param recl recruitment length
##' @return growth matrix
get.gadget.growth <- function(stocks,params,dt=0.25,age.based=FALSE,recl=FALSE){
  plyr::ldply(stocks,function(x){
    txt.split <- merge.formula(unlist(strsplit(x@growth@growthparameters,' ')))
    txt.split <- c(txt.split,x@growth@beta,x@growth@maxlengthgroupgrowth)
    suit.par <- eval.gadget.formula(txt.split,params)$V1
    lt <- getLengthGroups(x)
    if(age.based){
      age <- x@minage:x@maxage
      if (recl) {
          gadget_growth_eqn <- strsplit(x@initialdata$mean, ' ')
          age_var <- lapply(1:(length(gadget_growth_eqn)-1), function(x) {
              setdiff(gadget_growth_eqn[[x]], gadget_growth_eqn[[x + 1]])
          })
          age_var <- c(age_var, list(NULL))
          find_age_param <- lapply(seq_along(gadget_growth_eqn), function(x) {
              which(gadget_growth_eqn[[x]] == age_var[[x]])
          })[[1]]
          gadget_growth_eqn <- gadget_growth_eqn[[1]]
          growth_switches <- grep("#", gadget_growth_eqn)
          growth_switch_vals <- eval.gadget.formula(gadget_growth_eqn[growth_switches],
                                                    params)$V1
          gadget_growth_eqn[growth_switches] <- growth_switch_vals
          gadget_growth_eqn[find_age_param] <- "#age"
          gadget_growth_eqn <- paste(gadget_growth_eqn, collapse = " ")
          growth <- parse.gadget.formulae(gadget_growth_eqn)
          data.frame(stock = x@stockname, age = age, length = eval(growth))
      } else{
            data.frame(stock=x@stockname,age=age,
                    length=suit.par[1]*(1-exp(-suit.par[2]*age)))
      }
    } else {
      reshape2::melt(growthprob(lt,suit.par[5],suit.par[1],suit.par[2],dt,
                      suit.par[6],max(diff(lt))),
           varnames = c('lfrom','lto','lgrowth'))
    }
  })
}

##' @rdname gadgetFitHelpers
##' @description \code{get.gadget.recruitment} gets gadget recruitment
##' @param stocks gadget.stock
##' @param params gadget.params
##' @return recruitment by year
get.gadget.recruitment <- function(stocks,params,collapse=TRUE){
  plyr::ldply(stocks, function(x){
    if(x@doesrenew == 1){
      tmp <- 
        x@renewal.data %>% 
        dplyr::mutate(stock = x@stockname,
                      recruitment = 1e4*unlist(eval.gadget.formula(number,params))) %>% 
        dplyr::select(stock,year,step,area,recruitment) %>% 
        na.omit() 
      if(collapse){
        tmp %>% 
		  # area added to accomodate merge with res.by.year in gadget.fit() - pfrater
          dplyr::group_by(stock,year, area) %>% 
          dplyr::summarise(recruitment=sum(recruitment)) %>% 
          as.data.frame()
      } else{
        tmp
      }
    } else {
      data.frame(stock = x@stockname,year=NA,area=NA,recruitment=NA)
    }
  })
}

##' \code{get.gadget.catches} gets gadget catches
##' @rdname gadgetFitHelpers
##' @param fleets gadget.fleet
##' @param params gadget.params
##' @return catches by year + step
get.gadget.catches <- function(fleets,params){
  tmp <- plyr::ddply(fleets$fleet,~fleet,
               function(x){
                 subset(read.gadget.table(x$amount),
                        V4 == x$fleet)
               })
  tmp$fleet <- NULL
  names(tmp) <- c('year','step','area','fleet','catch')
  tmp <- merge(merge(fleets$prey[c('fleet','stock')],tmp),
               fleets$fleet[c('fleet','type')])
  tmp$catch <- unlist(eval.gadget.formula(tmp$catch,params))
  return(tmp)
}

##' @rdname gadgetFitHelpers
##' @description \code{read.gadget.grouping} reads the the likelihood grouping from the WGTS folder
##' @param lik old style gadget likelihood object
##' @param wgts logcation of the WGTS folder
##' @return list of wgts groupings
read.gadget.grouping <- function(lik = read.gadget.likelihood(),
                                 wgts = 'WGTS'){
  lik.tmp <- subset(lik$weights,
                    !(type %in% c('penalty','understocking',
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
  tmp <- arrange(tmp,pos,ord)
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
    write.table(x,file=f,eol='\n',...)
    close(f)
}
