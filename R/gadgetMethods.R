## Time file methods

setGeneric('getTimeSteps',def=function(x){standardGeneric("getTimeSteps")})
setMethod('getTimeSteps','gadget-time',
          function(x) {
            year <- x@firstyear:x@lastyear
            schedule <- data.frame(year = rep(year,
                                     each = length(x@notimesteps)),
                                   step = rep(seq(along = x@notimesteps),
                                     length(year)))
            subset(schedule,(year> x@firstyear & year < x@lastyear) |
                   (year == x@firstyear & step >= x@firststep) |
                   (year == x@lastyear & step <= x@laststep) )


          })

## area-file methods


setMethod("toString",
          signature(x = "gadget-predator"),
          function (x, ...)
          {
            tmp <- paste(x@suitability$stock,x@suitability$suitability,
                         sep='\t',collapse='\n')

#            tmp <- paste(tmp$stock,tmp$func,sapply(tmp$parameters,
#                                            function(x) paste(x,collapse='\t')),
#                         sep = '\t',collapse = '\n')
            pred.text <-
              paste(sprintf('suitability\n%s',tmp),
                    sprintf('preference\n%s',
                            paste(x@preference$stock,
                                  x@preference$preference,
                                  sep = '\t',
                                  collapse = '\n')),
                    sprintf('maxconsumption\t%s',
                            paste(x@maxconsumption,collapse = '\t')),
                    sprintf('halffeedingvalue\t%s',x@halffeedingvalue),
                    sep = '\n')
            return(pred.text)
          }
)


setGeneric('getMaxage',def=function(object){standardGeneric("getMaxage")})
setMethod('getMaxage','gadget-stock', function(object) return(object@maxage))
setMethod('getMaxage','gadget-main',
          function(object){
            maxage <- max(sapply(object@stocks,getMaxage))
            return(maxage)
          })

setGeneric('getMinage',def=function(object){standardGeneric("getMinage")})
setMethod('getMinage','gadget-stock', function(object) return(object@minage))
setMethod('getMinage','gadget-main',
          function(object){
            minage <- min(sapply(object@stocks,getMinage))
            return(minage)
          })



setGeneric('getMaxlength',def=function(object){standardGeneric("getMaxlength")})
setMethod('getMaxlength','gadget-stock', function(object) return(object@maxlength))
setMethod('getMaxlength','gadget-main',
          function(object){
            maxlength <- max(sapply(object@stocks,getMaxlength))
            return(maxlength)
          })

setGeneric('getMinlength',def=function(object){standardGeneric("getMinlength")})
setMethod('getMinlength','gadget-stock', function(object) return(object@minlength))
setMethod('getMinlength','gadget-main',
          function(object){
            minlength <- min(sapply(object@stocks,getMinlength))
            return(minlength)
          })

setGeneric('getNumStocks',def=function(object){standardGeneric("getNumStocks")})
setMethod('getNumStocks','gadget-main',
          function(object) length(object@stocks))

setGeneric('getStockNames',def=function(object){standardGeneric("getStockNames")})
setMethod('getStockNames','gadget-stock',
          function(object) return(object@stockname))
setMethod('getStockNames','gadget-main',
          function(object) sapply(object@stocks,getStockNames))




setGeneric('getNumOfAreas',def=function(object){standardGeneric("getNumOfAreas")})
setMethod('getNumOfAreas','gadget-area',
          function(object) length(object@areas))
setMethod('getNumOfAreas','gadget-main',
          function(object) getNumOfAreas(object@area))

setGeneric('getAreas',def=function(object){standardGeneric("getAreas")})
setMethod('getAreas','gadget-area',
          function(object) object@areas)
setMethod('getAreas','gadget-main',
          function(object) getAreas(object@area))

setGeneric('getNumLengthGroups',def=function(object){standardGeneric("getNumLengthGroups")})
setMethod('getNumLengthGroups', 'gadget-stock',
          function(object){
            ceiling((object@maxlength - object@minlength)/object@dl)
          })
setMethod('getNumLengthGroups', 'gadget-main',
          function(object) max(ladply(object@stocks,getNumLengthGroups)))

setGeneric('getLengthGroups',def=function(object){standardGeneric("getLengthGroups")})
setMethod('getLengthGroups', 'gadget-stock',
          function(object){
            if((object@maxlength-object@minlength)%%object@dl==0){
              tail(seq(object@minlength, object@maxlength,by=object@dl),-1)
            } else {
              # is this the proper way of dealing with this?
              seq(object@minlength, object@maxlength,by=object@dl)+
                (object@maxlength-object@minlength)%%object@dl
            }
            })
setMethod('getLengthGroups', 'gadget-main',
          function(object) lapply(object@stocks,getLengthGroups))


setGeneric('getNumTimeSteps',def=function(object){
  standardGeneric("getNumTimeSteps")
  })
setMethod('getNumTimeSteps','gadget-time',
          function(object){
            length(object@notimesteps)
          })
setMethod('getNumTimeSteps','gadget-main',
          function(object){
            getNumTimeSteps(object@time)
          })

#setGeneric('getTimesteps',def=function(object){standardGeneric("getTimesteps")})
setGeneric('getNumYears',def=function(object){standardGeneric("getNumYears")})
setMethod('getNumYears', 'gadget-time',
          function(object) object@lastyear - object@firstyear + 1)
setMethod('getNumYears', 'gadget-main',
          function(object) getNumYears(object@time))

setGeneric('getYears',def=function(object){standardGeneric("getYears")})
setMethod('getYears', 'gadget-time',
          function(object) object@firstyear:object@lastyear)
setMethod('getYears', 'gadget-main',
          function(object) getYears(object@time))


setGeneric('getNumFleets',def=function(object){standardGeneric("getNumFleets")})
setMethod('getNumFleets','gadget-main',
          function(object) length(object@fleets))

setGeneric('getFleetNames',def=function(object){standardGeneric("getFleetNames")})
setMethod('getFleetNames','gadget-fleet',
          function(object) object@name)
setMethod('getFleetNames','gadget-main',
          function(object) sapply(object@fleets,getFleetNames))

setGeneric('getNumTagging',def=function(object){standardGeneric("getNumTagging")})
setMethod('getNumTagging', 'gadget-tagging',
          function(object) nrow(object@tag.experiments))
setMethod('getNumTagging', 'gadget-main',
          function(object) getNumTagging(object@tagging))

setGeneric('getTagID',def=function(object){standardGeneric("getTagID")})
setMethod('getTagID','gadget-tagging',
          function(object) object@tag.experiments)
setMethod('getTagID','gadget-main',
          function(object) getTagID(object@tagging))

setGeneric('isPredator',def=function(object){standardGeneric("isPredator")})
setMethod('isPredator', 'gadget-stock',
          function(object) object@doeseat )
setGeneric('getNumPredators',def=function(object){standardGeneric("getNumPredators")})
setMethod('getNumPredators', 'gadget-main',
          function(object) sum(sapply(object@stocks,isPredator)))

setGeneric('getPredatorNames',def=function(object){standardGeneric("getPredatorNames")})
setMethod('getPredatorNames','gadget-main',
          function(object){
            tmp <- sapply(object@stocks,
                         function(x){
                           if(isPredator(x)==1)
                             getStockNames(x)
                           else
                             ''
                         })
            tmp[tmp!='']
          })
setGeneric('getPreyNames',def=function(object){standardGeneric("getPreyNames")})
setMethod('getPreyNames','gadget-stock',
          function(object){
            object@predator@suitability$stock
          })



setGeneric('getGrowth',def=function(object, par){standardGeneric("getGrowth")})
setMethod('getGrowth', 'gadget-growth',
          function(object,par){
            if(tolower(object@growthfunction) == 'lengthvbsimple'){
              if(class(object@growthparameters) == 'numeric' |
                   class(object@growthparameters) == 'integer'){
                tmp <- object@growthparameters
              } else {
                tmp <- merge.formula(unlist(strsplit(object@growthparameters,' ')))
              }
              params <- eval.gadget.formula(tmp,par)$V1

              beta <- eval.gadget.formula(object@beta,par)$V1
              nbin <- eval.gadget.formula(object@maxlengthgroupgrowth,par)$V1
              function(lt,dl,dt){
                growthprob(lt,beta,params[1],params[2],dt,dl,
                           nbin)
              }
            } else {
              stop(sprintf('Error in Growth -- Only lengthvbsimple is supported, %s was supplied',
                           object@growthfunction))
            }
          })
setMethod('getGrowth','gadget-stock',
          function(object, par){
            func <- getGrowth(object@growth,par)
            function(dt){
              func(getLengthGroups(object), object@dl, dt)
            }
          })

setMethod('getGrowth', 'gadget-main',
          function(object){
            yearstep <- getTimeSteps(object@time)
            yearstep$dt <- yearstep$step/12
            plyr::llply(object@stocks,
                  function(x){
                    func <- getGrowth(x)
                    plyr::daply(yearstep,c('year','step'),
                          function(y){
                            func(y[1])
                          })
                  }
                  )
          })


setGeneric('getWeight',def=function(object, l, par){standardGeneric("getWeight")})
setMethod('getWeight', 'gadget-growth',
          function(object,l,par){


            if(tolower(object@growthfunction) == 'lengthvbsimple'){
              if(class(object@growthparameters) == 'numeric' |
                   class(object@growthparameters) == 'integer'){
                tmp <- object@growthparameters
              } else {
                tmp <- merge.formula(unlist(strsplit(object@growthparameters,' ')))
              }
              params <- eval.gadget.formula(tmp,par)$V1[3:4]

              return(params[1]*l^params[2])
            } else {
              stop(sprintf('Error in Growth -- Only lengthvbsimple is supported, %s was supplied',
                           object@growthfunction))
            }
          })
setMethod('getWeight','gadget-stock',
          function(object, l, par){
            #getWeight(object@growth,l,par)
            tmp <- object@refweight
            approxfun(tmp$length,tmp$weight)(l)
          })

setGeneric('getSpawnFunc',
           def=function(object, par){standardGeneric("getSpawnFunc")})
setMethod('getSpawnFunc','gadget-main',
          function(object, par=data.frame()){
              plyr::llply(object@stocks,function(x){ getSpawnFunc(x,par)})
          })

setMethod('getSpawnFunc','gadget-stock',
          function(object, par=data.frame()){
              if(object@doesspawn==1){
                  
                  l <- getLengthGroups(object)
                  w <- getWeight(object,l,par)
                  p <- plyr::llply(object@spawning@recruitment[-1],
                             function(x) {
                                 eval.gadget.formula(x,par)$V1
                             })                  
                  type <- object@spawning@recruitment[1]                      
                  function(n,i=1){
                      pp <- sapply(p,function(x){
                          if(length(x)>1){
                              x[min(length(x),i)]
                          } else {
                              x
                          }
                      })
                      spawnfunc(type,n,w,pp,l)
                  }
              } else {
                  function(n) return(0)
              }                  
          })


setGeneric('getFleetSuitability',
           def=function(object, par){standardGeneric("getFleetSuitability")})
setMethod('getFleetSuitability','gadget-fleet',
          function(object,par=data.frame()){
            plyr::dlply(object@suitability,~stock,function(x){
              function(l){
                tmp <-
                  merge.formula(unlist(strsplit(as.character(x[1,-(1:3)]),
                                                ' ')))
                suitability(eval.gadget.formula(tmp,par)$V1,
                            l,
                            0,
                            type = x$suitability[1])
              }
            })
          }
)
setMethod('getFleetSuitability','gadget-main',
          function(object,par){
            plyr::llply(object@fleets,function(x) getFleetSuitability(x,par))
          })




setGeneric('getMortality',def=function(object, par){standardGeneric("getMortality")})
setMethod('getMortality','gadget-stock',
          function(object,par){
            eval.gadget.formula(object@naturalmortality,par)
          }
          )
setMethod('getMortality','gadget-main',
          function(object,par){
            plyr::llply(object@stocks,function(x) getMortality(x,par))
          })


setGeneric('getPredatorSelection',def=function(object, par){standardGeneric("getPredatorSelection")})

setGeneric('getInitData',def=function(object, par){standardGeneric("getInitData")})
setMethod('getInitData','gadget-stock',
          function(object,par){ ## a bit of a hack
            tmp <- as.data.frame(apply(object@initialdata,2,
                                function(x){
                                  eval.gadget.formula(x,par)$V1
                                }))
            names(tmp) <- names(object@initialdata)
            tmp$age.factor <- 1e4*tmp$age.factor
            return(tmp)
          }
)
setMethod('getInitData','gadget-main',
          function(object,par){ ## a bit of a hack
            plyr::llply(object@stocks,getInitData,par)
})

setMethod('getMortality','gadget-main',
          function(object,par){
            plyr::llply(object@stocks,function(x) getMortality(x,par))
          })
setGeneric('getRecStocks',def=function(object, par){standardGeneric("getRecStocks")})
setGeneric('getRecruitment',def=function(object, par){standardGeneric("getRecruitment")})
setMethod('getRecruitment','gadget-stock',
          function(object,par){ ## a bit of a hack
            tmp <- as.data.frame(apply(object@renewal.data,2,
                                       function(x){
                                         eval.gadget.formula(x,par)$V1
                                       }))
            names(tmp) <- names(object@renewal.data)
            tmp$number <- 1e4*tmp$number
            return(tmp)
          }
)

setGeneric('getMigratingStocks',def=function(object){standardGeneric("getMigratingStocks")})
setGeneric('getMigrationMatrix',def=function(object, par){standardGeneric("getMigrationMatrix")})
setMethod('getMigrationMatrix','gadget-stock',
          function(object,par){ ## a bit of a hack
            NA
          }
)


setGeneric('checkOverconsumption',def=function(object){standardGeneric("checkOverconsumption")})

setGeneric('spawn',def=function(object, par){standardGeneric("spawn")})
setGeneric('getSpawnSteps',def=function(object){standardGeneric("getSpawnSteps")})

setGeneric('straying',def=function(object,par){standardGeneric("straying")})

setGeneric('getLaststep',def=function(object){standardGeneric("getLaststep")})


setGeneric('writeAggfiles',
           def=function(x,folder){standardGeneric("writeAggfiles")})
setMethod('writeAggfiles','gadget-stock',
          function(x,folder){

            ## length aggregation file
            lengths <- seq(x@minlength,x@maxlength,by = x@dl)
            lenAgg <- data.frame(length = paste('len',tail(lengths,-1),
                                   sep = ''),
                                 min = head(lengths,-1),
                                 max = tail(lengths,-1)
                                 )

            agg.head <-
              paste(sprintf('; aggregation file for %s created using rgadget at %s',
                            x@stockname,Sys.Date()),
                    paste(c('; ',names(lenAgg)),collapse = '\t'),
                    sep = '\n')
            write.unix(agg.head,f = sprintf('%s/%s.len.agg',folder,
                             x@stockname))

            write.gadget.table(lenAgg,
                        file = sprintf('%s/%s.len.agg',folder,
                          x@stockname),
                        col.names=FALSE,append=TRUE,
                        quote=FALSE,sep='\t',row.names=FALSE)

            ## all length agg file
            alllenAgg <- data.frame(length = 'alllen',
                                    min = min(lengths),
                                    max = max(lengths))
            write.unix(agg.head,f = sprintf('%s/%s.alllen.agg',folder,
                             x@stockname))
            write.gadget.table(alllenAgg,
                        file = sprintf('%s/%s.alllen.agg',folder,x@stockname),
                        col.names=FALSE,append=TRUE,
                        quote=FALSE,sep='\t',row.names=FALSE)

            ## age agg file
            ageAgg <- data.frame(label = x@minage:x@maxage,
                                 age = x@minage:x@maxage)
            write.unix(agg.head,f = sprintf('%s/%s.age.agg',folder,x@stockname))
            write.gadget.table(ageAgg,
                        file = sprintf('%s/%s.age.agg',folder,x@stockname),
                        col.names=FALSE,append=TRUE,
                        quote=FALSE,sep='\t',row.names=FALSE)

            ## allages.agg
            allagesAgg <- data.frame(label = 'allages',
                                     age = paste(x@minage:x@maxage,
                                       collapse = '\t'))
            write.unix(agg.head,f = sprintf('%s/%s.allages.agg',folder,
                             x@stockname))
            write.gadget.table(allagesAgg,
                        file = sprintf('%s/%s.allages.agg',folder,x@stockname),
                        col.names=FALSE,append=TRUE,
                        quote=FALSE,sep='\t',row.names=FALSE)
            ## Area agg file
            areaAgg <- data.frame(label=paste('area',
                                    x@livesonareas,
                                    sep = ''),
                                  area = x@livesonareas)
            write.unix(agg.head,f = sprintf('%s/%s.area.agg',folder,
                             x@stockname))
            write.gadget.table(areaAgg,
                        file = sprintf('%s/%s.area.agg',folder,x@stockname),
                        col.names=FALSE,append=TRUE,
                        quote=FALSE,sep='\t',row.names=FALSE)

          })




























