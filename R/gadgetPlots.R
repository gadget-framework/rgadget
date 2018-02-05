#' Plot Gadget fit
##'
##' Plot the results from gadget.fit. The function produces a different plots by datatype and plottype. 
##' Valid datatypes are:
##' \describe{
##'   \item{sidat}{Surveyindices}
##'   \item{summary}{Likelihood summary data}
##'   \item{catchdist.fleets}{Catchdistribution data}
##'   \item{stockdist}{Stockdistribution data}
##'   \item{res.by.year}{Results by year}
##'   \item{stock.std}{Age composition from the model} 
##'   \item{suitability}{Suitability estimated from the model by year and step} 
##' }
##' and valid plottypes are:
##' \describe{
##'   \item{direct}{Default value, plots direct comparisons of data with model output. Valid for all datatype except 'res.by.year'}
##'   \item{weighted}{Only for 'summary'. Plots the weighted likelihood value for each component.}
##'   \item{pie}{Only for 'summary'. Plots the likelihood composition as a pie chart}
##'   \item{lengths}{Only for 'sidat'. Plot the surveyindex based on the SI length group instead of component name.}
##'   \item{bio}{Only for 'sidat'. Plot the biomass weighted survey index, assumes length based abundance index.}
##'   \item{x-y}{Only for 'sidat'. Produces a x-y scatter-plot for the fitted and observed index.}
##'   \item{resid}{Only for 'catchdist.fleets'. Produces a residual plot for each component.}
##'   \item{bubble}{Only for 'catchdist.fleets'. Produces a bubble plot for each component.}
##'   \item{growth}{Only for 'catchdist.fleets'. Produces a plot of fitted growth for each age-length component.}
##'   \item{resid}{Only for 'catchdist.fleets'. Produces a residual plot for each component.}
##'   \item{F}{Only for 'res.by.year'. Produces a F plot by stock.}
##'   \item{total}{Only for 'res.by.year'. Produces a total biomass plot by stock.}
##'   \item{catch}{Only for 'res.by.year'. Produces a total catch plot by stock.}
##'   \item{rec}{Only for 'res.by.year'. Produces a recruitment biomass plot by stock.}
##'   
##'   
##'   
##' }
##' @title plot gadget fit
##'
##' @param fit results from gadget fit
##' @param type The type of plot that is returned, depends on the input data (see details)
##' @param data what results should be plotted
##' @return ggplot object
##' @author Bjarki Þór Elvarsson
##' @export
##' @examples 
##' \dontrun{
##' ## collect the data from gadget
##' fit <- gadget.fit()
##' ## plot survey indices
##' plot(fit) 
##' ## plot likelihood
##' plot(fit,data='summary')
##' }
plot.gadget.fit <- function(fit,data = 'sidat',type='direct'){
  pl <- NULL
  
  if(data=='summary' & type == 'direct'){
    pl <- 
      fit$likelihoodsummary %>% 
      dplyr::filter(year!='all') %>% 
      dplyr::mutate(year = as.numeric(year)) %>%
      ggplot(aes(year, likelihood.value)) +
      geom_point() + 
      facet_wrap(~component,scale='free_y') +
      xlab('Year') + ylab('Score') 
    
  } 
  
 
  
  if(data == 'summary' & type == 'weighted'){
    pl <-  
      fit$likelihoodsummary %>% 
      dplyr::filter(year!='all') %>% 
      dplyr::mutate(year = as.numeric(year)) %>%
      ggplot(aes(year, weight*likelihood.value)) +
      geom_point() + 
      facet_wrap(~component,scale='free_y') +
      xlab('Year') + ylab('Weighted score')
    
  } 
  
  if(data == 'summary' & type == 'pie'){
    pl <- 
      fit$likelihoodsummary %>% 
      dplyr::group_by(component) %>% 
      dplyr::summarise(val = sum(likelihood.value*weight)) %>% 
      ggplot(aes(x="",y=val,fill = component)) + geom_bar(stat='identity',width = 1) + 
      coord_polar("y",start = 0) + 
      scale_fill_brewer(palette="Spectral")
    
  } 
  
  if(data == 'sidat' & type == 'lengths'){
    pl <-
      ggplot(fit$sidat, 
             aes(year,observed)) +
      geom_point() +
      geom_line(aes(year,predict)) +
      geom_linerange(data=subset(fit$sidat,year==max(year)),
                     aes(year,ymax=observed,ymin=predict),col='green')+
      geom_text(data=plyr::ddply(fit$sidat,~length,function(x){
        mutate(subset(x,year==min(year)),y=Inf)
      }),
      aes(year,y,label=length), vjust = 2,hjust = -1)+
      facet_wrap(~lower+step,scale='free_y',ncol=2) + 
      ylab('Index') + xlab('Year') +
      theme (panel.spacing = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
             strip.background = element_blank(), strip.text.x = element_blank())
  
  } 
  
  if(data == 'sidat' & type == 'direct'){
    pl <-
      ggplot(fit$sidat, 
             aes(year,observed)) +
      geom_point() +
      geom_line(aes(year,predict)) +
      geom_linerange(data=fit$sidat %>% 
                       dplyr::filter(year==max(year)),
                     aes(year,ymax=observed,ymin=predict),col='green') +
      geom_text(data=fit$sidat %>% 
                  dplyr::group_by(name) %>% 
                  dplyr::filter(year==min(year)) %>% 
                  dplyr::mutate(y=Inf),
                aes(year,y,label = name), vjust = 2,hjust = -1)+
      facet_wrap(~name,scale='free_y',ncol=2) + 
      ylab('Index') + xlab('Year') +
      theme (panel.spacing = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
             strip.background = element_blank(), strip.text.x = element_blank())
  }
  
  if(data=='sidat' & type == 'bio'){
    bio.tmp <- 
      fit$sidat %>% 
      dplyr::group_by(year,step) %>% 
      dplyr::summarise(obs = sum(observed*bio),
                       prd = sum(predict*bio)) 
    pl <-
      ggplot(bio.tmp, aes(year,obs)) +
      geom_point() +
      geom_line(aes(year,prd)) +
      geom_linerange(data=bio.tmp %>% 
                       dplyr::filter(year==max(year)),
                     aes(year,ymax=obs,ymin=prd),col='green')+
      facet_wrap(~step,scale='free_y',ncol=2)  +
      ylab('Biomass index') + xlab('Year') +
      theme (panel.spacing = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
             strip.background = element_blank(), strip.text.x = element_blank())
  } 
  
  if(data == 'sidat' & type == 'x-y' ) {
    pl <-
      ggplot(fit$sidat,aes(observed,predict,label=year)) + 
      geom_text() + 
      geom_abline(slope = 1, lty = 2) +
      facet_wrap(~name,scale='free') + ylab('Predicted value') + 
      xlab('Observed') + 
      geom_hline(data=filter(fit$sidat,year==max(year)),
                 aes(yintercept=predict),col='green') + 
      geom_vline(data=filter(fit$sidat,year==max(year)),
                 aes(xintercept=predict),col='green') + 
      geom_text(data = fit$sidat %>% 
                  dplyr::select(name) %>% 
                  dplyr::distinct(),
                aes(-Inf,Inf,label=name),vjust=2,hjust=-0.5) + 
      theme(panel.spacing = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
            strip.background = element_blank(), strip.text.x = element_blank())
    
  } 
  
  if(data=='catchdist.fleets'){
    if(type == 'direct'){
      pl <-  
        unique(fit$catchdist.fleets$name) %>% 
        purrr::set_names(.,.) %>% 
        purrr::map(function(x){
          dat <- fit$catchdist.fleets %>% 
            dplyr::filter(name == x)
          if(length(unique(dat$age))==1){
            dat %>% 
              dplyr::ungroup() %>% 
              ggplot(aes(lower,predicted)) +
              geom_line(aes(lower,observed),col='gray') +
              facet_wrap(~year+step,drop = FALSE,
                         ncol = max(2*length(unique(dat$step)),4))  + 
              geom_line() +
              geom_text(data=dat %>% 
                          dplyr::ungroup() %>% 
                          dplyr::filter(lower == min(lower)) %>% 
                          dplyr::mutate(y=Inf,
                                        label = paste(year,step,sep=',')) %>% 
                          dplyr::select(step,lower,y,year,label), 
                        
                        aes(lower,y,label=label), 
                        vjust = 1.3,hjust = -.05,
                        size = 3,
                        inherit.aes = FALSE)+
              ylab('Proportion') + xlab('length') +
              theme (axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.spacing = unit(0,'cm'),
                     plot.margin = unit(c(0,0,0,0),'cm'),
                     strip.background = element_blank(),
                     strip.text.x = element_blank())
          } else {
            dat %>% 
              dplyr::group_by(year,step,age) %>% 
              dplyr::summarise(predicted=sum(predicted),
                               observed=sum(observed,na.rm=TRUE)) %>% 
              dplyr::mutate(age = as.numeric(gsub('age','',age))) %>% 
              dplyr::ungroup() %>% 
              ggplot(aes(age,predicted)) +
              geom_line(aes(age,observed),col='gray') +
              facet_wrap(~year+step,drop = FALSE,
                         ncol = max(2*length(unique(dat$step)),4))  + 
              geom_line() +
              geom_text(data=dat %>% 
                          dplyr::ungroup() %>% 
                          dplyr::mutate(age = as.numeric(gsub('age','',age))) %>% 
                          dplyr::filter(age == min(age)) %>% 
                          dplyr::mutate(y=Inf,
                                        label = paste(year,step,sep=',')) %>% 
                          dplyr::select(step,age,y,year,label) %>% 
                          dplyr::distinct(), 
                        aes(age,y,label=label), 
                        vjust = 1.3,hjust = -.05,
                        size = 3,
                        inherit.aes = FALSE)+
              ylab('Proportion') + xlab('Age') +
              theme (axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.spacing = unit(0,'cm'),
                     plot.margin = unit(c(0,0,0,0),'cm'),
                     strip.background = element_blank(),
                     strip.text.x = element_blank())
          }
        })
      
    } else if(type == 'growth'){
      fit$catchdist.fleets %>%
        dplyr::ungroup() %>% 
        tidyr::nest(-name) %>% 
        dplyr::mutate(plots = purrr::map(data,function(x){
          if(length(unique(x$age))>1)
            x %>% 
            group_by(year,step,age) %>% 
            mutate(o=observed/sum(observed,na.rm=TRUE),p=predicted/max(sum(predicted),1e-6)) %>% 
            select(year,step,age,length = avg.length,observed,o,predicted,p) %>% 
            ungroup() %>% 
            mutate(age=as.numeric(gsub('age','',age))) %>% 
            group_by(year,step,age) %>% 
            summarise(o.ml=sum(o*length,na.rm=TRUE),
                      o.sl=sqrt(sum(o*(length - o.ml)^2,na.rm=TRUE)),
                      p.ml=sum(p*length),
                      p.sl=sqrt(sum(p*(length - p.ml)^2))) %>% 
            mutate(o.ml=ifelse(o.ml==0,NA,o.ml),
                   o.sl=ifelse(o.sl==0,NA,o.sl),
                   upper = p.ml+1.96*p.sl,
                   lower = p.ml-1.96*p.sl,
                   o.upper = o.ml+1.96*o.sl,
                   o.lower = o.ml-1.96*o.sl) %>% 
            ggplot(aes(age,o.ml)) + geom_ribbon(fill='gold',aes(ymax=upper,ymin=lower))+geom_point(size=0.5) + 
            geom_line(aes(y=p.ml))  + geom_linerange(aes(ymax=o.upper,ymin=o.lower))+
            facet_wrap(~year+step,drop = FALSE,
                       ncol = max(2*length(unique(x$step)),4)) + 
            theme_bw() + xlab('Age') + ylab('Average length') +
            geom_text(x=-Inf,y=Inf,
                      aes(label=paste(year,step,sep=',')),
                      size=3,
                      data = x %>% 
                        dplyr::select(year,step) %>% 
                        dplyr::distinct(),vjust = 1.5,hjust = -0.1,
                      inherit.aes = FALSE) + 
            theme(strip.background = element_blank(),strip.text=element_blank()) 
        })) %>% 
        filter(map(plots,~!is.null(.)) %>% unlist()) %>% 
        select(name,plots) -> tmp
      
      pl <-
        tmp$plots %>% 
        purrr::set_names(.,tmp$name)
      
    } else if(type == 'resid'){
      pl <-
        ggplot(fit$catchdist.fleets,aes(lower, observed - predicted,
                                        group=round(lower,1))) +
        geom_boxplot() + facet_wrap(~name)  + ylab('Residual') +
        xlab('Length')
    } else if(type == 'bubble'){
        
      pl <- 
        list(ldist = 
               fit$catchdist.fleets %>% 
               dplyr::group_by(name) %>% 
               dplyr::mutate(n=n_distinct(age)) %>% 
               dplyr::filter(n==1,
                             abs(observed-predicted)!=0) %>% 
               dplyr::left_join(fit$SS$weights %>% rename(name=Component)) %>% 
               ggplot(aes(year+(step-1)/4,avg.length,size=abs((observed-predicted)*sqrt(Weight)), 
                          col=as.factor(sign((observed-predicted))))) + 
               geom_point() + facet_wrap(~name)  + 
               theme_light() + scale_color_manual(values=c('darkblue','red')) + 
               scale_size_area() + theme(legend.position = 'none') + 
               labs(x='Year',y='Length'),
             aldist = 
               fit$catchdist.fleets %>% 
               dplyr::group_by(name) %>% 
               dplyr::mutate(n=n_distinct(age)) %>% 
               dplyr::filter(n!=1) %>% 
               dplyr::mutate(age=as.numeric(gsub('age','',age))) %>% 
               dplyr::group_by(name,year,age,step,total.catch) %>% 
               dplyr::summarise(o=sum(observed,na.rm=TRUE),
                                p=sum(predicted)) %>% 
               dplyr::mutate(o=ifelse(o==0,NA,o)) %>%
               dplyr::filter(abs(o-p)!=0) %>% 
               dplyr::left_join(fit$SS$weights %>% rename(name=Component)) %>% 
               ggplot(aes(year+(step-1)/4,age,size=abs((o-p)*sqrt(Weight)), 
                          col=as.factor(sign((o-p))))) + geom_point() + 
               facet_wrap(~name)  + theme_light() + 
               scale_color_manual(values=c('darkblue','red'))+ theme(legend.position = 'none'))
    }
  }

  if(data == 'res.by.year' & type == 'F'){
    pl <-
      ggplot(fit$res.by.year %>% ungroup(),aes(year,F,col=stock)) + geom_line() +
      ylab('F') + xlab('Year')
    
  } 
  
  if(data == 'res.by.year' & type == 'rec'){
    pl <-
      ggplot(fit$res.by.year,aes(year,recruitment/1e6,col=stock)) + geom_line() +
      ylab('Recruitment (in millions)') + xlab('Year')
    
  } 
  
  if(data == 'res.by.year' & type == 'total'){
    pl <-
      ggplot(fit$res.by.year,aes(year,total.biomass/1e6,col=stock)) + geom_line() +
      ylab("Biomass (in '000 tons)") + xlab('Year')
    
  } 
  
  if(data == 'res.by.year' & type == 'num.total'){
    pl <-
      ggplot(fit$res.by.year,aes(year,total.number/1e6,col=stock)) + geom_line() +
      ylab("Abundance (in millions)") + xlab('Year')
    
  } 
  
  if(data == 'res.by.year' & type == 'ssb'){
    pl <-
      ggplot(fit$res.by.year,aes(year,ssb/1e6,col=stock)) + geom_line() +
      ylab("SSB (in '000 tons)") + xlab('Year')
  
  } 
  
  if(data == 'res.by.year' & type == 'catch'){
    pl <-
      ggplot(fit$res.by.year,aes(year,catch/1e6,fill=stock)) + geom_bar(stat='identity') +
      labs(y="Catch (in '000 tons)",x='Year')
    
  } 
  
  if(data == 'res.by.year' & type == 'num.catch'){
    pl <-
      ggplot(fit$res.by.year,aes(year,num.catch/1e6,fill=stock)) + 
      geom_bar(stat='identity') +
      lab(y="Catch in numbers (millions)",x='Year')
    
  } 
  
  if(data == 'suitability') {
    pl <-
      fit$suitability %>%
      dplyr::ungroup() %>% 
      ggplot(aes(length,suit,lty=fleet)) +
      geom_line() +
      facet_wrap(~year + step) +
      labs(y='Suitability',x='Length') +
      geom_text(data=fit$suitability %>%
                  dplyr::ungroup() %>% 
                  dplyr::select(year,step) %>% 
                  dplyr::mutate(y=Inf,
                                label = paste(year,step,sep=',')) %>% 
                  dplyr::select(step,y,year,label) %>% 
                  dplyr::distinct(), 
                aes(-Inf,Inf,label=label), 
                vjust = 1.3,hjust = -.05,
                size = 3,
                inherit.aes = FALSE) +
      facet_wrap(~year+step,drop = FALSE,
                 ncol = max(2*length(unique(fit$suitability$step)),4))  + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.spacing = unit(0,'cm'),
            plot.margin = unit(c(0,0,0,0),'cm'),
            strip.background = element_blank(),
            strip.text.x = element_blank(), 
            legend.title = element_blank())
    
  } 
  
  if(data == 'growth') {
    pl <-
      fit$stock.std %>% 
      dplyr::filter(step == 1,number > 0) %>% 
      ggplot(aes(age,mean.length,lty=stock)) + 
      geom_line() +
      facet_wrap(~year) + 
      labs(y='Length',x='Age') +
      theme(legend.title = element_blank(),
            plot.margin = unit(c(0,0,0,0),'cm'))
  } 
  
  if(data == 'stock.std') {
    year_span <- 
      unique(fit$stock.std$year)
    
    pl <-
      fit$stock.std %>% 
      dplyr::mutate(yc = as.factor(year - age)) %>% 
      ggplot(aes(year,number,fill=yc),col='black') + 
      geom_bar(stat='identity') + 
      facet_wrap(~age,ncol=1,scale='free_y') + 
      theme(legend.position='none',panel.spacing = unit(0,'cm'),
            plot.margin = unit(c(0,0,0,0),'cm'),
            strip.background = element_blank(),
            strip.text.x = element_blank()) + 
      annotate("segment", 
               x=year_span-0.5, 
               xend=year_span+.5,
               y=Inf, yend=-Inf,lty=2,col='gray') + 
      geom_text(aes(Inf,Inf,label=paste('Age',age)),
                hjust=2,vjust=2,col='gray')+
      labs(x='Year',y='Num. fish (in millions)') 
  }
  
  if(data == 'stockdist'){
    
      fit$stockdist %>%
      tidyr::nest(-name) %>% 
      dplyr::mutate(plots = purrr::map(data,function(x){
        if(x$length %>% unique() %>% length() > 1){
          x %>% 
            mutate(pred.ratio= ifelse(is.nan(pred.ratio),0,pred.ratio)) %>% 
            ggplot(aes(length,
                       obs.ratio)) +
            geom_point() + 
            geom_line(aes(y=pred.ratio,lty = stock))+
            facet_wrap(~year+step) + theme_light() + 
            labs(y='Stock prop.',x='Length') +
            geom_label(data= fit$stockdist %>% 
                         ungroup() %>% 
                         select(year,step) %>% 
                         distinct()%>% 
                         mutate(label=paste(year,step, sep=',')),
                       aes(label=label,group=1),x=-Inf,y=Inf,size=3, vjust = 2,hjust=-0.1) +
            theme(strip.background = element_blank(),strip.text=element_blank())
        } else {
          x %>% 
            mutate(pred.ratio= ifelse(is.nan(pred.ratio),0,pred.ratio),
                   age = gsub('age','',age) %>% as.numeric()) %>% 
            ggplot(aes(age,
                       obs.ratio,col=stock)) +
            geom_point() + 
            geom_line(aes(y=pred.ratio,lty = stock))+
            facet_wrap(~year+step) + theme_light() + 
            labs(y='Stock prop.',x='Age') +
            geom_label(data= fit$stockdist %>% 
                         ungroup() %>% 
                         select(year,step) %>% 
                         distinct()%>% 
                         mutate(label=paste(year,step, sep=',')),
                       aes(label=label,group=1),x=-Inf,y=Inf,size=3, vjust = 2,hjust=-0.1,
                       inherit.aes = FALSE) +
            theme(strip.background = element_blank(),strip.text=element_blank())
        }
      })) %>% 
      filter(map(plots,~!is.null(.)) %>% unlist()) %>% 
      select(name,plots) -> tmp
    
    pl <-
      tmp$plots %>% 
      purrr::set_names(.,tmp$name)
      
  }
  
  return(pl)
}

