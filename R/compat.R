fix_headers <- function(main.file = 'main',gd){
  main <- read.gadget.file(gd$dir,'main',file_type = 'main',recursive = TRUE)
  ## fix the areafile
  attributes(main[[1]]$areafile[[1]][4][[1]])$preamble <- 
    list('-- data --', "year\tstep\tarea\ttemperature")
  main[[1]]$areafile %>% write.gadget.file(gd$dir)
  
  st <-
    main$stock$stockfiles %>% 
    map(~read.gadget.file(file_name = ., file_type = 'stock', path = gd$dir,recursive = TRUE))
    
}