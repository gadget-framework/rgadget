setGeneric('gadget_dir_write',def=function(gd,x){standardGeneric('gadget_dir_write')})
setMethod("gadget_dir_write",
          signature(gd = 'list', x = "gadget-time"),
          function (gd,x) {
              header <- sprintf('; time file created in Rgadget\n; %s - %s',
                                gd$dir,Sys.Date())
              time.file <-
                  paste(header,
                        paste('firstyear',x@firstyear,sep='\t'),
                        paste('firststep',x@firststep,sep='\t'),
                        paste('lastyear',x@lastyear,sep='\t'),
                        paste('laststep',x@laststep,sep='\t'),
                        paste('notimesteps',
                              paste(length(x@notimesteps),
                                    paste(x@notimesteps,collapse=' ')),
                              sep='\t'),
                        sep='\n')
              write.unix(time.file,f=sprintf('%s/time',gd$dir))
          })

setMethod("gadget_dir_write",
    signature(gd = 'list', x = "gadget-area"),
    function (gd,x) {
      header <- sprintf('; area file created in Rgadget\n; %s - %s',gd$dir,Sys.Date())
      area.file <-
        paste(header,
              paste('areas',paste(x@areas,collapse=' '),sep='\t'),
              paste('size',paste(x@size,collapse=' '),sep='\t'),
              'temperature',
              '; year - step - area - temperature',
              sep='\n')
      write(area.file,file=sprintf('%s/area',gd$dir))
      write.gadget.table(x@temperature,file=sprintf('%s/area',gd$dir),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      
  })
          

## stockfile methods

setMethod("gadget_dir_write",
    signature(gd = 'list', x = "gadget-prey"),
    function (gd,x) {
        if(is.null(gd$rel.dir)){
            rel.dir <- gd$dir
            gd$rel.dir <- ''
        } else {
            rel.dir <- paste(gd$dir,gd$rel.dir,sep='/')
            gd$rel.dir <- paste0(gd$rel.dir,'/')
        }             

      dir.create(sprintf('%s/Aggfiles',rel.dir),
                 showWarnings = FALSE, recursive = TRUE)
      header <- paste(sprintf('; prey aggregation file for %s',x@name),
                      sprintf('; created using rgadget at %s', Sys.Date()),
                      sep = '\n')
      write.unix(header,f = sprintf('%s/Aggfiles/%s.prey.agg',rel.dir,x@name))
      write.gadget.table(x@preylengths,
                         file = sprintf('%s/Aggfiles/%s.prey.agg',rel.dir,x@name),
                         col.names=FALSE,append=TRUE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      paste(sprintf('preylengths\t%sAggfiles/%s.prey.agg',
                    gd$rel.dir,x@name),
            sprintf('energycontent\t%s',x@energycontent),
            sep = '\n')
  })
          


setMethod("gadget_dir_write",
    signature(gd = 'list', x = "gadget-stock"),
    function (gd,x){
        if(is.null(gd$rel.dir)){
            rel.dir <- gd$dir
            gd$rel.dir <- './'
        } else {
            rel.dir <- paste(gd$dir,gd$rel.dir,sep='/')
            gd$rel.dir <- paste0(gd$rel.dir,'/')
        }             
        
        dir.create(sprintf('%s/Data', rel.dir),
                   showWarnings = FALSE, recursive = TRUE)
        dir.create(sprintf('%s/Aggfiles', rel.dir),
                   showWarnings = FALSE, recursive = TRUE)
        dir.create(sprintf('%s/Modelfiles', rel.dir),
                   showWarnings = FALSE, recursive = TRUE)

        
        
        ref.head <-
          paste(sprintf('; refweight file for %s created using rgadget at %s',
                        x@stockname,Sys.Date()),
                paste(c('; ',names(x@refweight)),collapse = '\t'),
                sep = '\n')
        write.unix(ref.head,f = sprintf('%s/Data/%s.refweightfile',
                        rel.dir,x@stockname))
        tmp <- x@refweight

        write.gadget.table(tmp,
                    file = sprintf('%s/Data/%s.refweightfile',rel.dir,
                        x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)

        ## length aggregation
        lengths <- seq(x@minlength,x@maxlength,by = x@dl)
        lenAgg <- data.frame(length = paste('len',tail(lengths,-1), sep = ''),
                             min = head(lengths,-1),
                             max = tail(lengths,-1)
                             )
        alllenAgg <- data.frame(length = 'alllen',
                                min = min(lengths),
                                max = max(lengths))
        agg.head <-
            paste(sprintf('; length aggregation file for %s created using rgadget at %s',
                                  x@stockname,Sys.Date()),
                          paste(c('; ',names(lenAgg)),collapse = '\t'),
                          sep = '\n')
        write.unix(agg.head,f = sprintf('%s/Aggfiles/%s.len.agg',
                                rel.dir,x@stockname))

        write.gadget.table(lenAgg,
                    file = sprintf('%s/Aggfiles/%s.len.agg',
                        rel.dir,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)

        write.unix(agg.head,
                   f = sprintf('%s/Aggfiles/%s.alllen.agg',
                       rel.dir,x@stockname))
        write.gadget.table(alllenAgg,
                    file = sprintf('%s/Aggfiles/%s.alllen.agg',
                        rel.dir,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)

        ## age agg file
        ageAgg <- data.frame(label = x@minage:x@maxage,
                             age = x@minage:x@maxage)
        write.unix(agg.head,
                   f = sprintf('%s/Aggfiles/%s.age.agg',
                       rel.dir,x@stockname))
        write.gadget.table(ageAgg,
                    file = sprintf('%s/Aggfiles/%s.age.agg',
                        rel.dir,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)
        allagesAgg <- data.frame(label = 'allages',
                                 age = paste(x@minage:x@maxage,collapse = '\t'))
        write.unix(agg.head,
                   f = sprintf('%s/Aggfiles/%s.allages.agg',
                       rel.dir,x@stockname))
        write.gadget.table(allagesAgg,
                    file = sprintf('%s/Aggfiles/%s.allages.agg',
                        rel.dir,x@stockname),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)

        ## initial data
        if(ncol(x@initialdata)==7){
          init.type <- 'normalcond'
        } else if(ncol(x@initialdata)==8){
          init.type <- 'normalparam'
        } else if(ncol(x@initialdata)==5){
          init.type <- 'number'
        }
        init.head <-
            paste(sprintf('; initial (%s) file for %s created using rgadget at %s',
                          init.type,x@stockname,Sys.Date()),
                  paste(c('; ',names(x@initialdata)),collapse = '-'),
                  sep = '\n')
        write.unix(init.head,
                   f = sprintf('%s/Modelfiles/%s.%s',
                       rel.dir,x@stockname,init.type))
        write.gadget.table(x@initialdata,
                    file = sprintf('%s/Modelfiles/%s.%s',
                        rel.dir,x@stockname,init.type),
                    col.names=FALSE,append=TRUE,
                    quote=FALSE,sep='\t',row.names=FALSE)


        stock.text <-
          c(sprintf('; stock definition file for %s created using rgadget',
                    x@stockname),
            sprintf('; at %s',Sys.Date()),
            ';',
            sprintf('stockname\t%s',x@stockname),
            sprintf('livesonareas\t%s',paste(x@livesonareas,collapse = '\n')),
            sprintf('minage\t%s',x@minage),
            sprintf('maxage\t%s',x@maxage),
            sprintf('minlength\t%s',x@minlength),
            sprintf('maxlength\t%s',x@maxlength),
            sprintf('dl\t%s',x@dl),
            sprintf('refweightfile\t%sData/%s.refweightfile',
                    gd$rel.dir,x@stockname),
            sprintf('growthandeatlengths\t%sAggfiles/%s.len.agg',
                    gd$rel.dir,x@stockname),
            sprintf('doesgrow\t%s',x@doesgrow),
            growth = ';',
            sprintf('naturalmortality\t%s',
                    paste(x@naturalmortality,collapse = '\t')),
            sprintf('iseaten\t%s',x@iseaten),
            eaten = ';',
            sprintf('doeseat\t%s',x@doeseat),
            eat = ';',
            'Initialconditions',
            paste(c('minage', 'maxage', 'minlength',
                    'maxlength', 'dl', 'sdev'),
                  x@initialconditions,sep ='\t',collapse = '\n'),
            sprintf('%sfile\t%sModelfiles/%s.%1$s',
                    init.type,gd$rel.dir,x@stockname),
            sprintf('doesmigrate\t%s',x@doesmigrate),
            migration = ';',
            sprintf('doesmature\t%s',x@doesmature),
            maturity = ';',
            sprintf('doesmove\t%s',x@doesmove),
            movement = ';',
            sprintf('doesrenew\t%s',x@doesrenew),
            renewal = ';',
            sprintf('doesspawn\t%s',x@doesspawn),
            spawning = ';',
            sprintf('doesstray\t%s',x@doesstray),
            straying = ';')
        if(x@doesgrow == 1){
          stock.text['growth'] <- toString(x@growth)
        }
        if(x@iseaten == 1){
          stock.text['eaten'] <- gadget_dir_write(gd,x@preyinfo)
        }
        if(x@doeseat == 1){
          stock.text['eat'] <- toString(x@predator)
        }
        if(x@doesspawn == 1){
            stock.text['spawning'] <-
                sprintf('spawnfile\t%sModelfiles/%s.spawnfile',
                        gd$rel.dir,x@stockname)
            write.unix(toString(x@spawning),
                       f=sprintf('%s/Modelfiles/%s.spawnfile', 
                           rel.dir,x@stockname))
        }
        if(x@doesmigrate == 1){
          stock.text['migration'] <-
            paste(sprintf('yearstepfile\t%sModelfiles/%s.yearstep',
                          gd$rel.dir,x@stockname),
                  sprintf('defineratios\t%sModelfiles/%s.migratio',
                          gd$rel.dir,x@stockname),
                  sep = '\n')
          write.gadget.table(x@yearstep,
                      file = sprintf('%s/Modelfiles/%s.yearstep',
                          rel.dir,x@stockname),
                      row.names = FALSE,
                      col.names = FALSE,
                      quote=FALSE)
          write.unix('; migration file',
                     sprintf('%s/Modelfiles/%s.migratio',
                             rel.dir,x@stockname))
          plyr::l_ply(names(x@migrationratio),
                function(y){
                  migratio <- sprintf('%s/Modelfiles/%s.migratio',
                                      rel.dir,x@stockname)
                  write.unix(sprintf('[migrationmatrix]\nname\t%s',y),
                        f = migratio, append = TRUE)
                  write.gadget.table(x@migrationratio[[y]],file= migratio,
                              append = TRUE,
                              row.names = FALSE,
                              col.names = FALSE,
                              quote=FALSE)
                  })
        }

        if(x@doesrenew == 1){
          if(ncol(x@renewal.data) == 9){
            rec.type <- 'normalparam'

          } else if(ncol(x@renewal.data) == 8){
            rec.type <- 'normalcond'

          } else if(ncol(x@renewal.data) == 6){
            rec.type <- 'number'

          }
          stock.text['renewal'] <-
            paste(paste(names(x@renewal),
                        plyr::laply(x@renewal,
                              function(x)
                              paste(x,collapse=' ')),
                        sep='\t',collapse = '\n'),
                  sprintf('%sfile\t%sModelfiles/%s.rec',
                          rec.type,gd$rel.dir,x@stockname),
                  sep='\n')
          write.unix(sprintf('; renewal-file for stock %s\n; %s',
                             x@stockname,paste(names(x@renewal.data),collapse='-')),
                     f = sprintf('%s/Modelfiles/%s.rec',rel.dir,x@stockname))
          write.gadget.table(x@renewal.data,
                             file = sprintf('%s/Modelfiles/%s.rec',
                                 rel.dir,x@stockname),
                             row.names = FALSE,
                             col.names = FALSE,
                             quote=FALSE,
                             append=TRUE)
        }

        if(x@doesmature == 1){
          stock.text['maturity'] <-
            sprintf('maturityfunction\t%s\nmaturityfile\tModelfiles/%s.maturity',
                    x@maturityfunction,x@stockname)
          maturityfile <-
            paste(sprintf('; maturityfile for stock %s',x@stockname),
                  sprintf('maturestocksandratios %s',
                          paste(x@maturestocksandratios,collapse='\t')),
#                  sprintf('maturestocksandratios %s %s',
#                          names(x@maturestocksandratios),
#                          x@maturestocksandratios),
                  sprintf('coefficients %s',
                          paste(x@coefficients,collapse='\t')),
                  sep='\n')
          if(x@maturitysteps != ''){
            maturityfile <- 
              paste(maturityfile, sprintf('maturitysteps\t%s',x@maturitysteps))
          }
          write.unix(maturityfile,f=sprintf('%s/Modelfiles/%s.maturity',
                                            rel.dir,x@stockname))

        }
        if(x@doesmove == 1){
          stock.text['movement'] <-
            paste(sprintf('transitionstocksandratios %s',
                          paste(x@transitionstocksandratios,#$stock,
#                                x@transitionstocksandratios$ratio,
                                collapse='\t')),
                  sprintf('transitionstep %s',x@transitionstep),
                  sep='\n')
        }
        write.unix(paste(stock.text,collapse = '\n'),
                   f = sprintf('%s/%s',rel.dir,x@stockname))
    }
)



setMethod("toString",
          signature(x = "gadget-growth"),
          function (x, ...)
          {
            growth.text <-
              c(sprintf('growthfunction\t%s',x@growthfunction),
                params = '',
                sprintf('beta\t%s',x@beta),
                sprintf('maxlengthgroupgrowth\t%s',x@maxlengthgroupgrowth))
            if(tolower(x@growthfunction) == 'lengthvbsimple'){
              growth.text['params'] <-
                paste(sprintf('growthparameters\t%s',
                              paste(x@growthparameters,collapse = '\t')))
            } else if(tolower(x@growthfunction) == 'weightvb'){
              growth.text['params'] <-
                paste(sprintf('wgrowthparameters\t%s',
                              paste(x@wgrowthparameters,collapse = '\t')),
                      sprintf('lgrowthparameters\t%s',
                              paste(x@lgrowthparameters,collapse = '\t')))
            } else {
              stop('other growth updates currently not supported by Rgadget')
            }
            return(paste(growth.text,collapse = '\n'))
          }
          )

setMethod("gadget_dir_write",
    signature(gd = 'list',x = "gadget-fleet"),
    function (gd,x){
      header <- sprintf('; fleet file created in Rgadget\n; %s - %s\n[fleetcomponent]',
                        gd$dir,Sys.Date())
      ## default text
      fleet.text <-
        c(header = header,
          name = sprintf('%s\t%s',x@type,x@name),
          area = sprintf('livesonareas\t%s',x@livesonareas),
          multi = sprintf('multiplicative\t%s',x@multiplicative),
          suit = sprintf('suitability\n%s',
                  paste(x@suitability$stock,
                        'function',
                        x@suitability$suitability,
                        x@suitability$params,
                        collapse='\n')),
          empty = '; empty space -- move along nothing to see here',
          amount = sprintf('amount\tData/%s.amount',x@name),
          ';')

      if(tolower(x@type) == 'quotafleet')
        fleet.text['empty'] <-
          paste(sprintf('quotafunction\t%s',x@quotafunction),
                sprintf('biomasslevel\t%s',paste(x@biomasslevel,collapse = '\t')),
                sprintf('quotalevel\t%s',paste(x@quotalevel,collapse = '\t')),
                sep = '\n')
      else if(tolower(x@type) == 'effortfleet')
        fleet.text['empty'] <-
          sprintf('catchability\n%s',
                  paste(paste(x@catchability$stock,
                              x@catchability$catchabilty,sep='\t'),
                        collapse = '\n'))
      else
        fleet.text <- fleet.text[names(fleet.text) != 'empty']
      dir.create(sprintf('%s/Data/',gd$dir),showWarnings = FALSE, recursive = TRUE)
      dir.create(sprintf('%s/Modelfiles/',gd$dir),
                 showWarnings = FALSE, recursive = TRUE)

      write.gadget.table(x@amount,file=sprintf('%s/Data/%s.amount',
                                      gd$dir,x@name),
                  col.names=FALSE,
                  quote=FALSE,sep='\t',row.names=FALSE)
      if(file.exists(sprintf('%s/Modelfiles/fleets',gd$dir))){
		  ## added by Paul Frater to test if fleet file already contains fleet names
          ## if so, overwrite the file, if not append fleet to fleet file
          fleetFile <- file(sprintf('%s/Modelfiles/fleets', gd$dir))
          fleetLines <- readLines(fleetFile)
		  close(fleetFile)
          if(any(grepl(x@name, fleetLines))) {
	  		    write.unix(paste(fleet.text,collapse='\n'),
                     f=sprintf('%s/Modelfiles/fleets',gd$dir), 
					 append = FALSE)
		  }
          else {
			    write.unix(paste(fleet.text,collapse='\n'),
                     f=sprintf('%s/Modelfiles/fleets',gd$dir),
                     append = TRUE)
		  }
      } else {
        write.unix(paste(fleet.text,collapse='\n'),
                   f=sprintf('%s/Modelfiles/fleets',gd$dir))
      }
      invisible(fleet.text)
    }
) 


##' @title Write gadget main
##' @return NULL
##' @author Bjarki Thor Elvarsson
setMethod("gadget_dir_write",
    signature(gd = 'list', x = "gadget-main"),
    function (gd,x) {
      loc <- gd$dir
      dir.create(loc, showWarnings = FALSE, recursive = TRUE)
      ## writing ecosystem files
      gadget_dir_write(gd,x@area)
      ## area aggregation files
      allareasAgg <- data.frame(label = 'allareas',
                                areas = paste(x@area@areas,collapse = '\t'))
      dir.create(sprintf('%s/Aggfiles',loc),showWarnings = FALSE, recursive = TRUE)
      write.gadget.table(allareasAgg,
                         file = sprintf('%s/Aggfiles/allareas.agg',loc),
                         col.names=FALSE,append=FALSE,
                         quote=FALSE,sep='\t',row.names=FALSE)
      gadget_dir_write(gd,x@time)
      if(length(x@print) > 0)
        gadget_dir_write(gd,x@print)
      for(stock in x@stocks)
        gadget_dir_write(gd,stock)
#      write(x@tags,file = sprintf('%s/tagfile',loc))
#      write(x@otherfood, file = sprintf('%s/otherfood',loc))
      if(file.exists(sprintf('%s/fleets',gd$dir))){
        file.remove(sprintf('%s/fleets',gd$dir))
      }
      for(fleet in x@fleets)
          gadget_dir_write(gd,fleet)
      ## Likelihood files
      ##write(x@likelhood, file = sprintf('%s/likelihood'))

      main.text <-
        paste(sprintf('; main file for the %s model',x@model.name),
              sprintf('; created using rgadget at %s',Sys.Date()),
              sprintf('timefile\ttime',loc),
              sprintf('areafile\tarea',loc),
              sprintf('printfiles\t%s',
                      ifelse(length(x@print)>0,'printfile',';')),
              '[stock]',
              sprintf('stockfiles\t%s',
                      paste(getStockNames(x),collapse='\t')),
              '[tagging]',
              ifelse(nrow(x@tags@tag.experiments)>0,'tagfiles\tModelfiles/tags',';'),
              '[otherfood]',
              ifelse(length(x@otherfood)>0,
                     'otherfoodfiles\tModelfiles/otherfood',';'),
              '[fleet]',
              ifelse(length(x@fleets)>0,sprintf('fleetfiles\tModelfiles/fleets'),';'),
              '[likelihood]',
              ifelse(length(x@likelihood)>0,
                     'likelihoodfiles\tlikelihood',';'),
              sep='\n'
              )
      write.unix(main.text,f=sprintf('%s/main',gd$dir))
      invisible(main.text)
  }) 
          

setMethod("toString",
          signature(x = "gadget-spawning"),
          function (x){
            spawn.text <-
              paste(sprintf('; spawning file created using Rgadget at %s',Sys.Date()),
                    sprintf('spawnsteps\t%s', paste(x@spawnsteps,collapse='\t')),
                    sprintf('spawnareas\t%s', paste(x@spawnareas,collapse='\t')),
                    sprintf('firstspawnyear\t%s', x@firstspawnyear),
                    sprintf('lastspawnyear\t%s',  x@lastspawnyear),
                    paste('spawnstocksandratios',
                          paste(x@spawnstocksandratio$stock,
                                x@spawnstocksandratio$ratio,sep='\t',collapse ='\t'),
                          sep='\t'),
                    sprintf('proportionfunction\t%s',
                            paste(x@proportionfunction,collapse='\t')),
                    sprintf('mortalityfunction\t%s',
                            paste(x@mortalityfunction,collapse='\t')),
                    sprintf('weightlossfunction\t%s',
                            paste(x@weightlossfunction,collapse='\t')),
                    sprintf('recruitment\t%s',
                            paste(x@recruitment,collapse='\t')),

                    sprintf('stockparameters\t%s\t%s\t%s\t%s',
                            x@stockparameters$mean,
                            x@stockparameters$std.dev,
                            x@stockparameters$alpha,
                            x@stockparameters$beta),
                    sep = '\n')
            return(spawn.text)
          }
          )

#setGeneric('popArray',def=function(object){standardGeneric("popArray")})
#setMethod('gadget-stock',function(object){
#
#
#})
