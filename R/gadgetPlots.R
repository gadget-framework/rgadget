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
##' @name plot.gadget.fit
##' @param fit results from gadget fit
##' @param type The type of plot that is returned, depends on the input data (see details)
##' @param data what results should be plotted
##' @return ggplot2::ggplot object
##' @author Bjarki Þór Elvarsson
##' @importFrom rlang .data
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
      dplyr::filter(.data$year!='all') %>% 
      dplyr::mutate(year = as.numeric(.data$year)) %>%
      ggplot2::ggplot(ggplot2::aes(.data$year, .data$likelihood_value)) +
      ggplot2::geom_point() + 
      ggplot2::facet_wrap(~.data$component,scale='free_y') +
      ggplot2::labs(x='Year',y='Score') 
    
  } 
  
  
  
  if(data == 'summary' & type == 'weighted'){
    pl <-  
      fit$likelihoodsummary %>% 
      dplyr::filter(.data$year!='all') %>% 
      dplyr::mutate(year = as.numeric(.data$year)) %>%
      ggplot2::ggplot(ggplot2::aes(.data$year, .data$weight*.data$likelihood_value)) +
      ggplot2::geom_point() + 
      ggplot2::facet_wrap(~.data$component,scale='free_y') +
      ggplot2::labs(x='Year',y='Weighted score')
    
  } 
  
  if(data == 'summary' & type == 'pie'){
    pl <- 
      fit$likelihoodsummary %>% 
      dplyr::group_by(.data$component) %>% 
      dplyr::summarise(val = sum(.data$likelihood_value*.data$weight)) %>% 
      ggplot2::ggplot(ggplot2::aes(x="",y=.data$val,fill = .data$component)) + 
      ggplot2::geom_bar(stat='identity',width = 1) + 
      ggplot2::coord_polar("y",start = 0) + 
      ggplot2::scale_fill_brewer(palette="Spectral")
    
  } 
  
  if(data == 'sidat' & type == 'lengths'){
    pl <-
      ggplot2::ggplot(fit$sidat, 
                      ggplot2::aes(.data$year,.data$observed)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(.data$year,.data$predict)) +
      ggplot2::geom_linerange(data=fit$sidat %>% dplyr::filter(.data$year==max(.data$year)),
                              ggplot2::aes(.data$year,ymax=.data$observed,ymin=.data$predict),col='green')+
      ggplot2::geom_text(data = fit$sidat %>% 
                           dplyr::group_by(.data$length) %>% 
                           dplyr::filter(.data$year == min(.data$year)) %>% 
                           dplyr::mutate(y = Inf),
                         ggplot2::aes(.data$year,.data$y,label=length), vjust = 2,hjust = -1) +
      ggplot2::facet_wrap(~.data$lower+.data$step,scale='free_y',ncol=2) + 
      ggplot2::labs(y='Index', x='Year') +
      ggplot2::theme(panel.spacing = ggplot2::unit(0,'cm'), 
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(), 
                     strip.text.x = ggplot2::element_blank())
    
  } 
  
  if(data == 'sidat' & type == 'direct'){
    pl <-
      ggplot2::ggplot(fit$sidat, 
                      ggplot2::aes(.data$year,.data$observed)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(.data$year,.data$predict)) +
      ggplot2::geom_linerange(data=fit$sidat %>% 
                                dplyr::filter(.data$year==max(.data$year)),
                              ggplot2::aes(.data$year,ymax=.data$observed,ymin=.data$predict),col='green') +
      ggplot2::geom_text(data=fit$sidat %>% 
                           dplyr::group_by(.data$name) %>% 
                           dplyr::filter(.data$year==min(.data$year)) %>% 
                           dplyr::mutate(y=Inf),
                         ggplot2::aes(.data$year,.data$y,label = .data$name), vjust = 2,hjust = -1)+
      ggplot2::facet_wrap(~.data$name,scale='free_y',ncol=2) + 
      ggplot2::labs(y='Index', x='Year') +
      ggplot2::theme (panel.spacing = ggplot2::unit(0,'cm'), 
                      plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                      strip.background = ggplot2::element_blank(), 
                      strip.text.x = ggplot2::element_blank())
  }
  
  if(data=='sidat' & type == 'bio'){
    bio.tmp <- 
      fit$sidat %>% 
      dplyr::group_by(.data$year,.data$step) %>% 
      dplyr::summarise(obs = sum(.data$observed*.data$bio),
                       prd = sum(.data$predict*.data$bio)) 
    pl <-
      ggplot2::ggplot(bio.tmp, ggplot2::aes(.data$year,.data$obs)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(.data$year,.data$prd)) +
      ggplot2::geom_linerange(data=bio.tmp %>% 
                                dplyr::filter(.data$year==max(.data$year)),
                              ggplot2::aes(.data$year,ymax=.data$obs,ymin=.data$prd),col='green')+
      ggplot2::facet_wrap(~.data$step,scale='free_y',ncol=2)  +
      ggplot2::labs(y='Biomass index', x='Year') +
      ggplot2::theme (panel.spacing = ggplot2::unit(0,'cm'), 
                      plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                      strip.background = ggplot2::element_blank(), 
                      strip.text.x = ggplot2::element_blank())
  } 
  
  if(data == 'sidat' & type == 'x-y' ) {
    pl <-
      fit$sidat %>% 
      ggplot2::ggplot(ggplot2::aes(.data$observed,.data$predict,label=.data$year)) + 
      ggplot2::geom_text() + 
      ggplot2::facet_wrap(~name,scale='free') + 
      ggplot2::geom_abline(slope = 1, lty = 2) +
      
      ggplot2::labs(y='Predicted value', x='Observed') + 
      ggplot2::geom_hline(data=dplyr::filter(fit$sidat,.data$year==max(.data$year)),
                          ggplot2::aes(yintercept=.data$predict),col='green') + 
      ggplot2::geom_vline(data=dplyr::filter(fit$sidat,.data$year==max(.data$year)),
                          ggplot2::aes(xintercept=.data$predict),col='green') + 
      ggplot2::geom_text(data = fit$sidat %>% 
                           dplyr::select(.data$name) %>% 
                           dplyr::distinct(),
                         ggplot2::aes(-Inf,Inf,label=.data$name),vjust=2,hjust=-0.5) + 
      ggplot2::theme(panel.spacing = ggplot2::unit(0,'cm'), 
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(), 
                     strip.text.x = ggplot2::element_blank())
    
  } 
  
  if(data=='catchdist.fleets'){
    if(type == 'direct'){
      pl <-  
        unique(fit$catchdist.fleets$name) %>% 
        purrr::set_names(.,.) %>% 
        purrr::map(function(x){
          dat <- fit$catchdist.fleets %>% 
            dplyr::filter(.data$name == x)
          if(length(unique(dat$age))==1){
            dat %>% 
              dplyr::ungroup() %>% 
              ggplot2::ggplot(ggplot2::aes(.data$lower,.data$predicted)) +
              ggplot2::geom_line(ggplot2::aes(.data$lower,.data$observed),col='gray') +
              ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                                  ncol = max(2*length(unique(dat$step)),4))  + 
              ggplot2::geom_line() +
              ggplot2::geom_text(data=dat %>% 
                                   dplyr::ungroup() %>% 
                                   dplyr::filter(.data$lower == min(.data$lower)) %>% 
                                   dplyr::mutate(y=Inf,
                                                 label = paste(.data$year,.data$step,sep=',')) %>% 
                                   dplyr::select(.data$step,.data$lower,.data$y,.data$year,.data$label), 
                                 
                                 ggplot2::aes(.data$lower,.data$y,label=.data$label), 
                                 vjust = 1.3,hjust = -.05,
                                 size = 3,
                                 inherit.aes = FALSE)+
              ggplot2::labs('Proportion', x='length') +
              ggplot2::theme (axis.text.y = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank(),
                              panel.spacing = ggplot2::unit(0,'cm'),
                              plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                              strip.background = ggplot2::element_blank(),
                              strip.text.x = ggplot2::element_blank())
          } else {
            dat %>% 
              dplyr::group_by(.data$year,.data$step,.data$age) %>% 
              dplyr::summarise(predicted=sum(.data$predicted),
                               observed=sum(.data$observed,na.rm=TRUE)) %>% 
              dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>% 
              dplyr::ungroup() %>% 
              ggplot2::ggplot(ggplot2::aes(.data$age,.data$predicted)) +
              ggplot2::geom_line(ggplot2::aes(.data$age,.data$observed),col='gray') +
              ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                                  ncol = max(2*length(unique(dat$step)),4))  + 
              ggplot2::geom_line() +
              ggplot2::geom_text(data=dat %>% 
                                   dplyr::ungroup() %>% 
                                   dplyr::mutate(age = as.numeric(gsub('age','',.data$age))) %>% 
                                   dplyr::filter(age == min(.data$age)) %>% 
                                   dplyr::mutate(y=Inf,
                                                 label = paste(.data$year,.data$step,sep=',')) %>% 
                                   dplyr::select(.data$step,.data$age,.data$y,.data$year,.data$label) %>% 
                                   dplyr::distinct(), 
                                 ggplot2::aes(.data$age,.data$y,label=.data$label), 
                                 vjust = 1.3,hjust = -.05,
                                 size = 3,
                                 inherit.aes = FALSE)+
              ggplot2::labs(y='Proportion', x='Age') +
              ggplot2::theme (axis.text.y = ggplot2::element_blank(),
                              axis.ticks.y = ggplot2::element_blank(),
                              panel.spacing = ggplot2::unit(0,'cm'),
                              plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                              strip.background = ggplot2::element_blank(),
                              strip.text.x = ggplot2::element_blank())
          }
        })
      
    } else if(type == 'growth'){
      fit$catchdist.fleets %>%
        dplyr::ungroup() %>% 
        tidyr::nest(-.data$name) %>% 
        dplyr::mutate(plots = purrr::map(data,function(x){
          if(length(unique(x$age))>1)
            x %>% 
            dplyr::group_by(.data$year,.data$step,.data$age) %>% 
            dplyr::mutate(o=.data$observed/sum(.data$observed,na.rm=TRUE),
                          p=.data$predicted/max(sum(.data$predicted),1e-14)) %>% 
            dplyr::select(.data$year,.data$step,.data$age,
                          length = .data$avg.length,.data$observed,
                          .data$o,.data$predicted,.data$p) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(age=as.numeric(gsub('age','',.data$age))) %>% 
            dplyr::group_by(.data$year,.data$step,.data$age) %>% 
            dplyr::summarise(o.ml=sum(.data$o*.data$length,na.rm=TRUE),
                             o.sl=sqrt(sum(.data$o*(.data$length - .data$o.ml)^2,na.rm=TRUE)),
                             p.ml=sum(.data$p*.data$length),
                             p.sl=sqrt(sum(.data$p*(.data$length - .data$p.ml)^2))) %>% 
            dplyr::mutate(o.ml=ifelse(.data$o.ml==0,NA,.data$o.ml),
                          o.sl=ifelse(.data$o.sl==0,NA,.data$o.sl),
                          upper = .data$p.ml+1.96*.data$p.sl,
                          lower = .data$p.ml-1.96*.data$p.sl,
                          o.upper = .data$o.ml+1.96*.data$o.sl,
                          o.lower = .data$o.ml-1.96*.data$o.sl) %>% 
            ggplot2::ggplot(ggplot2::aes(.data$age,.data$o.ml)) + 
            ggplot2::geom_ribbon(fill='gold',
                                 ggplot2::aes(ymax=.data$upper,ymin=.data$lower))+
            ggplot2::geom_point(size=0.5) + 
            ggplot2::geom_line(ggplot2::aes(y=.data$p.ml))  + 
            ggplot2::geom_linerange(ggplot2::aes(ymax=.data$o.upper,ymin=.data$o.lower))+
            ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                                ncol = max(2*length(unique(x$step)),4)) + 
            ggplot2::labs(x='Age', y='Average length') +
            ggplot2::geom_text(x=-Inf,y=Inf,
                               ggplot2::aes(label=paste(.data$year,.data$step,sep=',')),
                               size=3,
                               data = x %>% 
                                 dplyr::select(.data$year,.data$step) %>% 
                                 dplyr::distinct(),vjust = 1.5,hjust = -0.1,
                               inherit.aes = FALSE) + 
            ggplot2::theme(strip.background = ggplot2::element_blank(),strip.text=ggplot2::element_blank()) 
        })) %>% 
        dplyr::filter(purrr::map(.data$plots,~!is.null(.)) %>% unlist()) %>% 
        dplyr::select(.data$name,.data$plots) -> tmp
      
      
      pl <-
        tmp$plots %>% 
        purrr::set_names(.,tmp$name)
      
    } else if(type == 'resid'){
      pl <-
        ggplot2::ggplot(fit$catchdist.fleets,
                        ggplot2::aes(.data$lower, .data$observed - .data$predicted,
                                     group=round(.data$lower,1))) +
        ggplot2::geom_boxplot() + 
        ggplot2::facet_wrap(~.data$name)  + 
        ggplot2::labs(y='Residual', y='Length')
    } else if(type == 'bubble'){
      
      pl <- 
        list(ldist = 
               fit$catchdist.fleets %>% 
               dplyr::group_by(.data$name) %>% 
               dplyr::mutate(n=dplyr::n_distinct(.data$age)) %>% 
               dplyr::filter(.data$n==1,
                             abs(.data$observed-.data$predicted)!=0) %>% 
               dplyr::left_join(fit$SS$weights %>% dplyr::rename(name=.data$Component)) %>% 
               ggplot2::ggplot(ggplot2::aes(.data$year+(.data$step-1)/4,.data$avg.length,
                                            size=abs((.data$observed-.data$predicted)*sqrt(.data$Weight)), 
                                            col=as.factor(sign((.data$observed-.data$predicted))))) + 
               ggplot2::geom_point() + 
               ggplot2::facet_wrap(~.data$name)  + 
               ggplot2::scale_color_manual(values=c('darkblue','red')) + 
               ggplot2::scale_size_area() + 
               ggplot2::theme(legend.position = 'none') + 
               ggplot2::labs(x='Year',y='Length'),
             aldist = 
               fit$catchdist.fleets %>% 
               dplyr::group_by(.data$name) %>% 
               dplyr::mutate(n=dplyr::n_distinct(.data$age)) %>% 
               dplyr::filter(n!=1) %>% 
               dplyr::mutate(age=as.numeric(gsub('age','',.data$age))) %>% 
               dplyr::group_by(.data$name,.data$year,.data$age,.data$step,.data$total.catch) %>% 
               dplyr::summarise(o=sum(.data$observed,na.rm=TRUE),
                                p=sum(.data$predicted)) %>% 
               dplyr::mutate(o=ifelse(.data$o==0,NA,.data$o)) %>%
               dplyr::filter(abs(.data$o-.data$p)!=0) %>% 
               dplyr::left_join(fit$SS$weights %>% dplyr::rename(name=.data$Component)) %>% 
               ggplot2::ggplot(ggplot2::aes(.data$year+(.data$step-1)/4,## CHECK this
                                            .data$age,size=abs((.data$o-.data$p)*sqrt(.data$Weight)), 
                                            col=as.factor(sign((.data$o-.data$p))))) + 
               ggplot2::geom_point() + 
               ggplot2::facet_wrap(~.data$name)  + 
               ggplot2::scale_color_manual(values=c('darkblue','red'))+ 
               ggplot2::theme(legend.position = 'none'))
    }
  }
  
  if(data == 'res.by.year' & type == 'F'){
    pl <-
      ggplot2::ggplot(fit$res.by.year %>% dplyr::ungroup(),ggplot2::aes(.data$year,.data$F,col=.data$stock)) + 
      ggplot2::geom_line() +
      ggplot2::labs(y='F', x='Year',col='Stock')
    
  } 
  
  if(data == 'res.by.year' & type == 'rec'){
    pl <-
      ggplot2::ggplot(fit$res.by.year,ggplot2::aes(.data$year,.data$recruitment/1e6,col=.data$stock)) + 
      ggplot2::geom_line() +
      ggplot2::labs(y='Recruitment (in millions)', x='Year',col='Stock')
    
  } 
  
  if(data == 'res.by.year' & type == 'total'){
    pl <-
      ggplot2::ggplot(fit$res.by.year,ggplot2::aes(.data$year,.data$total.biomass/1e6,col=.data$stock)) + 
      ggplot2::geom_line() +
      ggplot2::labs(y="Biomass (in '000 tons)", x='Year',col='Stock')
    
  } 
  
  if(data == 'res.by.year' & type == 'num.total'){
    pl <-
      ggplot2::ggplot(fit$res.by.year,ggplot2::aes(.data$year,.data$total.number/1e6,col=.data$stock)) + 
      ggplot2::geom_line() +
      ggplot2::labs(y="Abundance (in millions)", x='Year',col='Stock')
    
  } 
  
  if(data == 'res.by.year' & type == 'ssb'){
    pl <-
      ggplot2::ggplot(fit$res.by.year,ggplot2::aes(.data$year,.data$ssb/1e6,col=.data$stock)) + 
      ggplot2::geom_line() +
      ggplot2::labs(y = "SSB (in '000 tons)", x = 'Year',col='Stock')
    
  } 
  
  if(data == 'res.by.year' & type == 'catch'){
    pl <-
      ggplot2::ggplot(fit$res.by.year,ggplot2::aes(.data$year,.data$catch/1e6,fill=.data$stock)) + 
      ggplot2::geom_bar(stat='identity') +
      ggplot2::labs(y="Catch (in '000 tons)",x='Year',fill='Stock')
    
  } 
  
  if(data == 'res.by.year' & type == 'num.catch'){
    pl <-
      ggplot2::ggplot(fit$res.by.year,ggplot2::aes(.data$year,.data$num.catch/1e6,fill=.data$stock)) + 
      ggplot2::geom_bar(stat='identity') +
      ggplot2::labs(y="Catch in numbers (millions)",x='Year',fill = 'Stock')
    
  } 
  
  if(data == 'suitability') {
    pl <-
      fit$suitability %>%
      dplyr::ungroup() %>% 
      ggplot2::ggplot(ggplot2::aes(.data$length,.data$suit,lty=.data$fleet)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~.data$year + .data$step) +
      ggplot2::labs(y='Suitability',x='Length',lty = 'Fleet') +
      ggplot2::geom_text(data=fit$suitability %>%
                           dplyr::ungroup() %>% 
                           dplyr::select(.data$year,.data$step) %>% 
                           dplyr::mutate(y=Inf,
                                         label = paste(.data$year,.data$step,sep=',')) %>% 
                           dplyr::select(.data$step,.data$y,.data$year,.data$label) %>% 
                           dplyr::distinct(), 
                         ggplot2::aes(-Inf,Inf,label=.data$label), 
                         vjust = 1.3,hjust = -.05,
                         size = 3,
                         inherit.aes = FALSE) +
      ggplot2::facet_wrap(~.data$year+.data$step,drop = FALSE,
                          ncol = max(2*length(unique(fit$suitability$step)),4))  + 
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank())
    
  } 
  
  if(data == 'growth') {
    pl <-
      fit$stock.std %>% 
      dplyr::filter(.data$step == 1,.data$number > 0) %>% 
      ggplot2::ggplot(ggplot2::aes(.data$age,.data$mean_length,lty=.data$stock)) + 
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~.data$year) + 
      ggplot2::labs(y='Length',x='Age',lty='Stock') +
      ggplot2::theme(#legend.title = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'))
  } 
  
  if(data == 'stock.std') {
    year_span <- 
      unique(fit$stock.std$year)
    
    pl <-
      fit$stock.std %>% 
      dplyr::mutate(yc = as.factor(.data$year - .data$age)) %>% 
      ggplot2::ggplot(ggplot2::aes(.data$year,.data$number,fill=.data$yc),col='black') + 
      ggplot2::geom_bar(stat='identity') + 
      ggplot2::facet_wrap(~.data$age,ncol=1,scale='free_y') + 
      ggplot2::theme(legend.position='none',panel.spacing = ggplot2::unit(0,'cm'),
                     plot.margin = ggplot2::unit(c(0,0,0,0),'cm'),
                     strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_blank()) + 
      ggplot2::annotate("segment", 
                        x=year_span-0.5, 
                        xend=year_span+.5,
                        y=Inf, yend=-Inf,lty=2,col='gray') + 
      ggplot2::geom_text(ggplot2::aes(Inf,Inf,label=paste('Age',.data$age)),
                         hjust=2,vjust=2,col='gray')+
      ggplot2::labs(x='Year',y='Num. fish (in millions)') 
  }
  
  if(data == 'stockdist'){
    
    fit$stockdist %>%
      tidyr::nest(-.data$name) %>% 
      dplyr::mutate(plots = purrr::map(.data$data,function(x){
        if(x$length %>% unique() %>% length() > 1){
          x %>% 
            dplyr::mutate(pred.ratio= ifelse(is.nan(.data$pred.ratio),0,.data$pred.ratio)) %>% 
            ggplot2::ggplot(ggplot2::aes(.data$length,.data$obs.ratio)) +
            ggplot2::geom_point() + 
            ggplot2::geom_line(ggplot2::aes(y=.data$pred.ratio,lty = .data$stock))+
            ggplot2::facet_wrap(~.data$year+.data$step) + ggplot2::theme_light() + 
            ggplot2::labs(y='Stock prop.',x='Length') +
            ggplot2::geom_label(data= fit$stockdist %>% 
                                  dplyr::ungroup() %>% 
                                  dplyr::select(.data$year,.data$step) %>% 
                                  dplyr::distinct()%>% 
                                  dplyr::mutate(label=paste(.data$year,.data$step, sep=',')),
                                ggplot2::aes(label=.data$label,group=1),
                                x=-Inf,y=Inf,size=3, vjust = 2,hjust=-0.1) +
            ggplot2::theme(strip.background = ggplot2::element_blank(),
                           strip.text = ggplot2::element_blank())
        } else {
          x %>% 
            dplyr::mutate(pred.ratio= ifelse(is.nan(.data$pred.ratio),0,.data$pred.ratio),
                          age = gsub('age','',.data$age) %>% as.numeric()) %>% 
            ggplot2::ggplot(ggplot2::aes(.data$age,.data$obs.ratio,col=.data$stock)) +
            ggplot2::geom_point() + 
            ggplot2::geom_line(ggplot2::aes(y=.data$pred.ratio,lty = .data$stock))+
            ggplot2::facet_wrap(~.data$year+.data$step) + 
            ggplot2::theme_light() + 
            ggplot2::labs(y='Stock prop.',x='Age',lty = 'Stock') +
            ggplot2::geom_label(data= fit$stockdist %>% 
                                  dplyr::ungroup() %>% 
                                  dplyr::select(.data$year,.data$step) %>% 
                                  dplyr::distinct()%>% 
                                  dplyr::mutate(label=paste(.data$year,.data$step, sep=',')),
                                ggplot2::aes(label=.data$label,group=1),
                                x=-Inf,y=Inf,size=3, vjust = 2,hjust=-0.1,
                                inherit.aes = FALSE) +
            ggplot2::theme(strip.background = ggplot2::element_blank(),
                           strip.text = ggplot2::element_blank())
        }
      })) %>% 
      dplyr::filter(purrr::map(.data$plots,~!is.null(.)) %>% unlist()) %>% 
      dplyr::select(.data$name,.data$plots) -> tmp
    
    pl <-
      tmp$plots %>% 
      purrr::set_names(.,tmp$name)
    
  }
  
  return(pl)
}

