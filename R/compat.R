fix_headers <- function(main.file = 'main',gd=list(dir='.')){
  main <- read.gadget.file(gd$dir,main.file,file_type = 'main',recursive = TRUE)
  ## fix the areafile
  attributes(main[[1]]$areafile[[1]][4][[1]])$preamble <- 
    list('-- data --', "year\tstep\tarea\ttemperature")
  main[[1]]$areafile %>% write.gadget.file(gd$dir)
  
  st <-
    main$stock$stockfiles %>% 
    map(~read.gadget.file(file_name = ., file_type = 'stock', path = gd$dir,recursive = TRUE)) %>% 
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
    map(~write.gadget.file(.,gd$dir))
  
  lik <- 
    main$likelihood %>% 
    map(~read.gadget.file(file_name = ., file_type = 'likelihood', path = gd$dir,recursive = TRUE))
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
