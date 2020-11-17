#' Wrapper to choose a component by name
#'
#' @param component	Call the corresponding gadget_(component)_component function
#' @param ...		Arguments to pass to it
#' @export
gadgetprintcomponent <- function (component, ...) {
  # Build a call to the relevant function, call it.
  x <- do.call(call, list(paste0('gadget_', component, '_component'), ...))
  eval(x)
}

#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:stockstdprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param scale		The datafile lists these weights and the power that is to be used for each parameter 
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_stockstdprinter_component <- function (printfile,
                                              stockname, 
                                              scale = 1, 
                                              precision = 1,
                                              printatstart = 1,
                                              yearsandsteps = 'all\tall') {
  
  type <- 'stockstdprinter'
  
  structure(c(
    list(
      type = type,
      stockname = stockname,
      scale = scale, 
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      printatstart = printatstart,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}


#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:stockfullprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_stockfullprinter_component <- function (printfile,
                                               stockname, 
                                               precision = 1,
                                               printatstart = 1,
                                               yearsandsteps = 'all\tall') {
  
  type <- 'stockfullprinter'
  
  structure(c(
    list(
      type = type,
      stockname = stockname,
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      printatstart = printatstart,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}

#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:stockprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_stockprinter_component <- function (printfile,
                                           stocknames, 
                                           area, 
                                           age,
                                           len,
                                           precision = 1,
                                           printatstart = 1,
                                           yearsandsteps = 'all\tall') {
  
  type <- 'stockprinter'
  
  structure(c(
    list(
      type = type,
      stocknames = stocknames,
      areaaggfile = agg_file('area', fname_prefix(type, printfile), area),
      ageaggfile = agg_file('age', fname_prefix(type, printfile), age),
      lenaggfile = agg_file('len', fname_prefix(type, printfile), len),
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      printatstart = printatstart,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}


#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:predatorprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_predatorprinter_component <- function (printfile,
                                              predatornames,
                                              preynames,
                                              area, 
                                              predlen,
                                              preylen,
                                              biomass = 1,
                                              precision = 1,
                                              yearsandsteps = 'all\tall') {
  
  type <- 'predatorprinter'
  
  structure(c(
    list(
      type = type,
      predatornames = predatornames,
      preynames = preynames,
      areaaggfile = agg_file('area', fname_prefix(type, printfile), area),
      predlenaggfile = agg_file('len', fname_prefix(sprintf('%s.predlen',type), printfile), predlen),
      preylenaggfile = agg_file('len', fname_prefix(sprintf('%s.preylen',type), printfile), preylen),
      biomass = biomass,
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}

#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:predatoroverprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_predatoroverprinter_component <- function (printfile,
                                                  predatornames,
                                                  area, 
                                                  len,
                                                  precision = 1,
                                                  yearsandsteps = 'all\tall') {
  
  type <- 'predatoroverprinter'
  
  structure(c(
    list(
      type = type,
      predatornames = predatornames,
      areaaggfile = agg_file('area', fname_prefix(type, printfile), area),
      lenaggfile = agg_file('len', fname_prefix(type, printfile), len),
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}
#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:stockpreyfullprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_stockpreyfullprinter_component <- function (printfile,
                                                   preynames,
                                                   area, 
                                                   len,
                                                   precision = 1,
                                                   yearsandsteps = 'all\tall') {
  
  type <- 'stockpreyfullprinter'
  
  structure(c(
    list(
      type = type,
      preynames = preynames,
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}

#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:stockpreyprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_stockpreyprinter_component <- function (printfile,
                                               preynames,
                                               area,
                                               age,
                                               len,
                                               precision = 1,
                                               yearsandsteps = 'all\tall') {
  
  type <- 'stockpreyprinter'
  
  structure(c(
    list(
      type = type,
      preynames = preynames,
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      areaaggfile = agg_file('area', fname_prefix(type, printfile), area),
      ageaggfile = agg_file('age', fname_prefix(type, printfile), age),
      lenaggfile = agg_file('len', fname_prefix(type, printfile), len),
      precision = precision,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}



#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:predatorpreyprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_predatorpreyprinter_component <- function (printfile,
                                               predatornames,
                                               preynames,
                                               area,
                                               age,
                                               len,
                                               precision = 1,
                                               yearsandsteps = 'all\tall') {
  
  type <- 'predatorpreyprinter'
  
  structure(c(
    list(
      type = type,
      predatornames = predatornames,
      preynames = preynames,
      areaaggfile = agg_file('area', fname_prefix(type, printfile), area),
      ageaggfile = agg_file('age', fname_prefix(type, printfile), age),
      lenaggfile = agg_file('len', fname_prefix(type, printfile), len),
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()),
      precision = precision,
      yearsandsteps = yearsandsteps)),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}


#' https://hafro.github.io/gadget2/userguide/chap-print.html#sec:stockstdprinter
#'
#' @param printfile		A descriptive name for the component, default same as component type
#' @param stockname	A numeric weighting
#' @param scale		The datafile lists these weights and the power that is to be used for each parameter 
#' @param precision 
#' @param printatstart 
#' @param yearsandsteps 
#' 
#' @export
gadget_likelihoodprinter_component <- function (printfile,
                                                likelihoodcomponent) {
  
  type <- 'likelihoodprinter'
  
  structure(c(
    list(
      type = type,
      likelihood = likelihoodcomponent,
      printfile = gadgetfile(
        fname('out', printfile),
        components = list()))),
    class = c(paste0("gadget_", type, "_component"),
              "gadgetprintcomponent"))
}

