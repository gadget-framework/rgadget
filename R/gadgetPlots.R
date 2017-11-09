#' <description>
##'
##' <details>
##' @title plot gadget fit
##' @param fit results from gadget fit
##' @param data what results should be plotted
##' @return ggplot object
##' @author Bjarki Þór Elvarsson
##' @export
plot.gadget.fit <- function(fit,data = 'sidat',type='direct',dat.name=NULL){
  if(data=='summary'){
    fit$likelihoodsummary %>% 
      dplyr::filter(year!='all') %>% 
      dplyr::mutate(year = as.numeric(year)) %>% 
      ggplot(aes(year, likelihood.value)) +
      geom_point() + 
      facet_wrap(~component,scale='free_y') +
      theme_bw()+
      xlab('Year') + ylab('Score') 
    
  } else if(data=='sidat' & type == 'direct'){
    ggplot(fit$sidat, aes(year,observed)) +
      geom_point() +
      geom_line(aes(year,predict)) +
      geom_linerange(data=subset(fit$sidat,year==max(year)),
                     aes(year,ymax=observed,ymin=predict),col='green')+
      geom_text(data=plyr::ddply(fit$sidat,~length,function(x){
        mutate(subset(x,year==min(year)),y=Inf)
      }),
      aes(year,y,label=length), vjust = 2,hjust = -1)+
      facet_wrap(~lower+step,scale='free_y',ncol=2) + theme_bw() +
      ylab('Index') + xlab('Year') +
      theme (panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
             strip.background = element_blank(), strip.text.x = element_blank())
    
  } else if(data=='sidat' & type == 'bio'){
    bio.tmp <- plyr::ddply(fit$sidat,~year + step,summarise,
                     obs=sum(observed*bio),prd=sum(predict*bio))
    ggplot(bio.tmp, aes(year,obs)) +
      geom_point() +
      geom_line(aes(year,prd)) +
      geom_linerange(data=subset(bio.tmp,year==max(year)),
                     aes(year,ymax=obs,ymin=prd),col='green')+
      facet_wrap(~step,scale='free_y',ncol=2) + theme_bw() +
      ylab('Biomass index') + xlab('Year') +
      theme (panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
             strip.background = element_blank(), strip.text.x = element_blank())
    
  } else if(data == 'sidat' & type == 'x-y' ) {
    ggplot(fit$sidat,aes(number.x,predict,label=year)) + geom_text() + 
      facet_wrap(~name+sse+slope,scale='free') + theme_bw() + ylab('Predicted value') + 
      xlab('Observed') + geom_hline(data=filter(fit$sidat,year==max(year)),
                                    aes(yintercept=predict),col='green') + 
      geom_vline(data=filter(fit$sidat,year==max(year)),aes(xintercept=predict),col='green') + 
      geom_text(aes(-Inf,Inf,label=name),vjust=2,hjust=-0.5) + 
      theme(panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
            strip.background = element_blank(), strip.text.x = element_blank())
    
  } else if(data=='catchdist.fleets'){
    if(type == 'direct'){
      if(is.null(dat.name)){
        ldist.fit <- plyr::llply(unique(fit$catchdist.fleets$name),
                           function(x){
                             dat <- subset(fit$catchdist.fleets,name == x)
                             if(length(unique(dat$age))==1){
                               ggplot(dat,aes(lower,predicted)) +
                                 geom_line(aes(lower,observed),col='gray') +
                                 facet_wrap(~year+step) + theme_bw() + geom_line() +
                                 geom_text(data=mutate(subset(dat,
                                                              lower==min(lower)),y=Inf),
                                           aes(lower,y,label=year), vjust = 2,hjust = -1)+
                                 ylab('Proportion') + xlab('length') +
                                 theme (axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        panel.margin = unit(0,'cm'),
                                        plot.margin = unit(c(0,0,0,0),'cm'),
                                        strip.background = element_blank(),
                                        strip.text.x = element_blank())
                             } else {
                               dat <- plyr::mutate(plyr::ddply(dat,~year+step+age,summarise,
                                                   predicted=sum(predicted),
                                                   observed=sum(observed,na.rm=TRUE)),
                                             age=as.numeric(gsub('age','',age)))
                               ggplot(dat,aes(age,predicted)) +
                                 geom_line(aes(age,observed),col='gray') +
                                 facet_wrap(~year+step) + theme_bw() + geom_line() +
                                 geom_text(data=plyr::mutate(subset(dat,
                                                              age==min(age)),y=Inf),
                                           aes(age,y,label=year), vjust = 2,hjust = -1)+
                                 ylab('Proportion') + xlab('Age') +
                                 theme (axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        panel.margin = unit(0,'cm'),
                                        plot.margin = unit(c(0,0,0,0),'cm'),
                                        strip.background = element_blank(),
                                        strip.text.x = element_blank()
                                 )
                             }})
        names(ldist.fit) <- unique(fit$catchdist.fleets$name)
        return(ldist.fit)
      } else {
        dat <- subset(fit$catchdist.fleets,name == dat.name)
        if(length(unique(dat$age))==1){
          ggplot(dat,aes(avg.length,predicted)) +
            geom_line(aes(avg.length,observed),col='gray') +
            facet_wrap(~year+step) + theme_bw() + geom_line() +
            geom_text(data=mutate(subset(dat,
                                         lower==min(lower)),y=Inf),
                      aes(lower,y,label=year), vjust = 2,hjust = -1)+
            ylab('Proportion') + xlab('length') +
            theme (axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.margin = unit(0,'cm'),
                   plot.margin = unit(c(0,0,0,0),'cm'),
                   strip.background = element_blank(),
                   strip.text.x = element_blank())
        } else {
          dat <- plyr::mutate(plyr::ddply(dat,~year+step+age,summarise,
                              predicted=sum(predicted),
                              observed=sum(observed,na.rm=TRUE)),
                        age=as.numeric(gsub('age','',age)))
          ggplot(dat,aes(age,predicted)) +
            geom_line(aes(age,observed),col='gray') +
            facet_wrap(~year+step) + theme_bw() + geom_line() +
            geom_text(data=mutate(subset(dat,
                                         age==min(age)),y=Inf),
                      aes(age,y,label=year), vjust = 2,hjust = -1)+
            ylab('Proportion') + xlab('Age') +
            theme (axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.margin = unit(0,'cm'),
                   plot.margin = unit(c(0,0,0,0),'cm'),
                   strip.background = element_blank(),
                   strip.text.x = element_blank()
            )
        }}
    } else if(type == 'resid'){
      ggplot(fit$catchdist.fleets,aes(lower, observed - predicted,
                                      group=round_any(lower,1))) +
        geom_boxplot() + facet_wrap(~name) + theme_bw() + ylab('Residual') +
        xlab('Length')
    }
    
  } else if(data == 'res.by.year' & type == 'F'){
    ggplot(fit$res.by.year,aes(year,F,col=stock)) + geom_line() +
      theme_bw() + ylab('F') + xlab('Year')
    
  } else if(data == 'res.by.year' & type == 'rec'){
    ggplot(fit$res.by.year,aes(year,recruitment/1e6,col=stock)) + geom_line() +
      theme_bw() + ylab('Recruitment (in millions)') + xlab('Year')
    
  } else if(data == 'res.by.year' & type == 'total'){
    ggplot(fit$res.by.year,aes(year,total.biomass/1e6,col=stock)) + geom_line() +
      theme_bw() + ylab("Biomass (in '000 tons)") + xlab('Year')
    
  } else if(data == 'res.by.year' & type == 'num.total'){
    ggplot(fit$res.by.year,aes(year,total.number/1e6,col=stock)) + geom_line() +
      theme_bw() + ylab("Abundance (in millions)") + xlab('Year')
    
  } else if(data == 'res.by.year' & type == 'ssb'){
    ggplot(fit$res.by.year,aes(year,ssb/1e6,col=stock)) + geom_line() +
      theme_bw() + ylab("SSB (in '000 tons)") + xlab('Year')
  
  } else if(data == 'res.by.year' & type == 'catch'){
    ggplot(fit$res.by.year,aes(year,catch/1e6,fill=stock)) + geom_bar(stat='identity') +
      theme_bw() + ylab("Catch (in '000 tons)") + xlab('Year')
    
  } else if(data == 'res.by.year' & type == 'num.catch'){
    ggplot(fit$res.by.year,aes(year,num.catch/1e6,fill=stock)) + geom_bar(stat='identity') +
      theme_bw() + ylab("Catch in numbers (millions)") + xlab('Year')
    
  } else if(data == 'suitability') {
    ggplot(fit$suitability,
           aes(length,suit,lty=fleet)) +
      geom_line() + theme_bw() + ylab('Suitability') + xlab('Length') +
      theme(legend.position = c(0.8,0.25), legend.title = element_blank(),
            plot.margin = unit(c(0,0,0,0),'cm'))
    
  } else if(data == 'growth') {
    ggplot(fit$stock.growth,
           aes(age,length)) + 
      geom_line() +
      theme_bw() + ylab('Length') + xlab('Age') +
      theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
            plot.margin = unit(c(0,0,0,0),'cm'))
  } else if(data == 'stock.std') {
    year_span <- unique(fit$stock.std$year)
    ggplot(fit$stock.std,aes(year,number,fill=as.factor(year-age)),col='black') + 
      geom_bar(stat='identity') + facet_wrap(~age,ncol=1,scale='free_y') + 
      theme_bw() + theme(legend.position='none',panel.margin = unit(0,'cm'),
                         plot.margin = unit(c(0,0,0,0),'cm'),
                         strip.background = element_blank(),
                         strip.text.x = element_blank()) + 
      annotate("segment", x=year_span-0.5, xend=year_span+.5,y=Inf, yend=-Inf,lty=2,col='gray') + 
      geom_text(aes(Inf,Inf,label=paste('Age',age)),hjust=2,vjust=2,col='gray')+
      xlab('Year') + ylab('Num. fish (in millions)') 
  }
}

