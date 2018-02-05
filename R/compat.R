#' Fix data headers in Gadget input files
#'
#' Recent revisions of RGadget introduced the gadgetfile class, which is a fairly flexible S3 class to interact with Gadget input files. 
#' To ensure that data object are read properly, i.e. as data.frame, the following header needs to be inserted to indicate that the following 
#' sections indicate data\cr
#' \cr
#' \code{; -- data --} \cr
#' \code{; col1 col2 ...}\cr
#' \cr
#' This function attempts to translate older gadget models, i.e. those not developed using Rgadget, to the new format 
#' @param main.file name of the main file
#' @param path location of the gadget model directory
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' fix_headers()
#' }
fix_headers <- function(main.file = 'main',path='.'){
  main <- read.gadget.file(path,main.file,file_type = 'main',recursive = TRUE)
  ## fix the areafile
  attributes(main[[1]]$areafile[[1]][4][[1]])$preamble <- 
    list('-- data --', "year\tstep\tarea\ttemperature")
  main[[1]]$areafile %>% write.gadget.file(path)
  
  st <-
    main$stock$stockfiles %>% 
    map(~read.gadget.file(file_name = ., file_type = 'stock', path = path,recursive = TRUE)) %>% 
    map(function(x){
      x$initialconditions <- c(x$initialconditions,x$numbers)
      x$numbers <- NULL
      tmp <- x$initialconditions %>% keep(~'gadgetfile' %in% class(.)) %>% names()
      if(tmp == 'normalparamfile'){
        attributes(x$initialconditions[[tmp]][[1]])$preamble <- 
          c('-- data --',paste(c('age', 'area','age.factor','area.factor', 'mean', 'stddev', 'alpha','beta'),collapse = '\t'))
      } 
      if(tmp == 'normalcondfile'){
        attributes(x$initialconditions[[tmp]][[1]])$preamble <- 
          c('-- data --',paste(c( 'age', 'area','age.factor','area.factor', 'mean', 'stddev', 'relcond'),collapse = '\t'))
      } 
      if(tmp == 'numberfile'){
        attributes(x$initialconditions[[tmp]][[1]])$preamble <- 
          c('-- data --',paste(c('age', 'area', 'length', 'number', 'weight'),collapse = '\t'))
      }
      if(x$doesrenew$doesrenew == 1){
        tmp <- x$doesrenew %>% keep(~'gadgetfile' %in% class(.)) %>% names()
        
        if(tmp == 'normalparamfile'){
          attributes(x$doesrenew[[tmp]][[1]])$preamble <- 
            c('-- data --',paste(c('year', 'step', 'area', 'age', 'number', 'mean', 'stddev', 'alpha','beta'),collapse = '\t'))
        } 
        if(tmp == 'normalcondfile'){
          attributes(x$doesrenew[[tmp]][[1]])$preamble <- 
            c('-- data --',paste(c('year', 'step', 'area', 'age', 'number', 'mean', 'stddev', 'relcond'),collapse = '\t'))
        } 
      }
      x
    }) %>% 
    map(~write.gadget.file(.,path))
  profvis({
  lik <- 
    main$likelihood %>% 
    map(~read.gadget.file(file_name = ., file_type = 'likelihood', path = path,recursive = TRUE)) 
  })
    lik %>% 
    map(~map(.,function(x){
      if(x$type=='catchdistribution'){
        attributes(x$datafile[[1]])$preamble  <- 
          c('-- data --',paste(c('year','step','area','age','length','number'),collapse = '\t'))
      }
      if(x$type=='catchstatistics'){
        if(x[['function']] %in% c('lengthcalcstddev','weightnostddev','lengthnostddev'))
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --', paste(c('year','step','area','age','number','mean'), collapse = '\t'))
        if(x[['function']] %in% c('lengthgivenstddev','weightgivenstddev', 'lengthgivenvar'))
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','age','number','mean','stddev'),collapse = '\t'))
        if(x[['function']] %in% c('weightgivenstddevlen'))
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','age','number','mean','stddev'),collapse = '\t')) 
        
      }
      if(x$type=='stockdistribution'){
        attributes(x$datafile[[1]])$preamble  <- 
          c('-- data --',paste(c('year','step','area','stock','age','length','number'),collapse = '\t'))
      }
      if(x$type=='surveyindices'){
        if(x$sitype %in% c('lengths','fleets') )
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','length','number'), collapse = '\t'))
        if(x$sitype=='ages')
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','age','number'), collapse = '\t'))
        if(x$sitype=='acoustic')
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','survey','number'), collapse = '\t'))
        if(x$sitype=='effort')
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','fleet','number'), collapse = '\t'))
      }
      if(x$type == 'surveydistribution'){
        attributes(x$datafile[[1]])$preamble  <- 
          c('-- data --',paste(c('year','step','area','age','length','number'), collapse = '\t'))
      }
      if(x$type=='stomachcontent'){
        attributes(x$datafile[[1]])$preamble  <- 
          c('-- data --',paste(c('year','step','area','predator','prey','ratio'), collapse = '\t'))
      }
      if(x$type=='recaptures'){
        attributes(x$datafile[[1]])$preamble  <- 
          c('-- data --',paste( c('tagid','year','step','area','length','number'), collapse = '\t'))
      }
      if(x$type=='recstatistics'){
        if(x[['function']] == 'lengthgivenstddev')
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('tagid','year','step','area','number','mean','stddev'), collapse = '\t'))
        else
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste( c('tagid','year','step','area','number','mean'), collapse = '\t'))
      }
      if(x$type=='catchinkilos'){
        if(ncol(dat)==4) #x$aggregationlevel==1)
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','area','fleet','biomass'), collapse = '\t'))
        else
          attributes(x$datafile[[1]])$preamble  <- 
            c('-- data --',paste(c('year','step','area','fleet','biomass'), collapse = '\t'))
      } 
      x
    }))->tmp
  
  attributes(tmp[[1]]) <- attributes(lik[[1]])
  
  tmp[[1]]%>% 
    write.gadget.file(path)
}

# time <- main[[1]]$timefile[[1]]
# schedule <- 
#   expand.grid(year = time$firstyear:time$lastyear,
#               step = 1:time$notimesteps[1]) %>%
#   left_join(data_frame(step = seq_along(time$notimesteps[-1]),
#                        dt = time$notimesteps[-1]/sum(time$notimesteps[-1]))) %>% 
#   arrange(year,step) %>% 
#   filter(!(year == time$firstyear & step < time$firststep ),
#          !(year == time$lastyear & step > time$laststep ))
