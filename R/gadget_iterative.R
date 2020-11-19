
#' @title Iterative reweighting for Gadget models
#' 
#' An implementation of the iterative reweigthing of likelihood
#' components in gadget. It analyzes a given gadget model and, after
#' a series of optimisations where each likelihood component is
#' heavily weigthed, suggests a weigthing for the components based on
#' the respective variance.  If one (or more) components, other than
#' understocking and penalty, are 0 then the gadget optimisation with
#' the final weights will not be completed.
#'
#' In Taylor et. al an objective reweighting scheme for likelihood
#' components is described for cod in Icelandic waters. The authors
#' nota that the issue of component weighting has been discussed for
#' some time, as the data sources have different natural scales (e.g
#' g vs. kg) that should not affect the outcome. A simple heuristic,
#' where the weights are the inverse of the initial sums of squares
#' for the respective component resulting in an initials score equal
#' to the number of components, is therfor often used. This has the
#' intutitive advantage of all components being normalised. There is
#' however a drawback to this since the component scores, given the
#' initial parametrisation, are most likely not equally far from
#' their respective optima resulting in sub-optimal weighting.  The
#' iterative reweighting heuristic tackles this problem by optimising
#' each component separately in order to determine the lowest
#' possible value for each component. This is then used to determine
#' the final weights.  The resoning for this approach is as follows:
#' Conceptually the likelihood components can be thought of as
#' residual sums of squares, and as such their variance can be
#' esimated by dividing the SS by the degrees of freedom. The optimal
#' weighting strategy is the inverse of the variance.  Here the
#' iteration starts with assigning the inverse SS as the initial
#' weight, that is the initial score of each component when
#' multiplied with the weight is 1. Then an optimisation run for each
#' component with the intial score for that component set to
#' 10000. After the optimisation run the inverse of the resulting SS
#' is multiplied by the effective number of datapoints and used as
#' the final weight for that particular component.  The effective
#' number of datapoints is used as a proxy for the degrees of freedom
#' is determined from the number of non-zero datapoints. This is
#' viewed as satisfactory proxy when the dataset is large, but for
#' smaller datasets this could be a gross overestimate. In
#' particular, if the surveyindices are weigthed on their own while
#' the yearly recruitment is esimated they could be overfitted. If
#' there are two surveys within the year Taylor et. al suggest that
#' the corresponding indices from each survey are weigthed
#' simultaneously in order to make sure that there are at least two
#' measurement for each yearly recruit, this is done through
#' component grouping which is implemented. Another approach, which
#' is also implemented, for say a single survey fleet the weight for
#' each index component is estimated from a model of the form
#' \deqn{\log(I_{lts}) = \mu + Y_t + \lambda_l + \Sigma_s +
#' \epsilon_{lts}}{% log(I_lts) = mu + Y_t + lambda_l + Sigma_s +
#' e_lts} where the residual term, \eqn{\epsilon_{lts}}{e_lts}, is
#' independent normal with variance
#' \eqn{\sigma_{ls}^2}{sigma_ls^2}. The inverse of the estimated
#' variance from the above model as the weights between the
#' surveyindices.  After these weights have been determined all
#' surveyindices are weighted simultaneously.

#' @param gd gadget directory
#' @param grouping a list naming the groups of components that should be reweighted together.
#' @param wgts a string containing the path the folder where the
#' interim weighting results should be stored.
#' @param params.in  a string containing the location of the input
#' parameters
#' @param ... passed to gadget_evaluate
#'
#' @return list of gadget variant dirs 
#' @export
#'
#' @examples
#' 
#' 
#' \dontrun{
#' gd <- gadget.variant.dir('01-base')
#' 
#' gadget_iterative_stage_1(gd, params.in = 'params.in') %>% 
#'   parallel::mclapply(gadget_optimize, mc.cores = parallel::detectCores()) %>% 
#'   gadget_iterative_stage_2() %>% 
#'   gadget_optimize()
#' }
gadget_iterative_stage_1 <- function(gd, 
                                     grouping = list(),
                                     wgts = 'WGTS',
                                     params.in = 'params.in',
                                     ...){
  dir.create(paste(gd,wgts, sep = '/'),showWarnings = FALSE)
  
  ## check if gadget can run, read in init SS and place the params.in into the WGTS folder
  gadget_evaluate(gd,params.in, lik.out = paste(wgts,'lik.init', sep = '/'), 
                  params.out = paste(wgts,'params.in', sep = '/'), ...)
  
  lik.init <- read.gadget.lik.out(paste(gd, wgts, 'lik.init', sep = '/'))
  
  if(is.null(lik.init))
    stop('Gadget model failed to run')
  
  main <- read.gadget.file(gd,attr(gd,'mainfile'), file_type = 'main', recursive = FALSE)
  lik <- 
    read.gadget.file(gd,main$likelihood$likelihoodfiles, file_type = 'likelihood', recursive = FALSE)
  
  
  
  lik.names <- lik %>% purrr::map('name')
  lik.type <- 
    lik %>% 
    purrr::map('type') %>% 
    purrr::set_names(lik.names)
  
  for(name in lik.names){
    ## skip penalty functions
    if(name %in% unlist(grouping) | 
       lik.type[[name]] %in% c('penalty','understocking','migrationpenalty','catchinkilos'))
      next
    grouping <- within(grouping,assign(name,name))
  }
  
  setup_function <- function(group){
    group_name <- paste(group, collapse = '__')
    
    gv <- gadget.variant.dir(gd,variant_dir = wgts,
                             mainfile = paste('main', group_name, sep='.'))
    ## we need to ensure that parameters are passed between stages
    attr(gv,'params_in') <- paste(attr(gv,'variant_dir'), 'params.in', sep = '/')
    attr(gv,'params_out') <- sprintf('%s/params.%s', attr(gv,'variant_dir'), group_name)
    
    ## we need to write the main file again as we are changing the names
    main$likelihood$likelihoodfiles <- ''
    attr(main,'file_name') <- paste('main', group_name, sep='.')
    write.gadget.file(main,gv)  
    
    for(name in seq_along(lik.names)){
      if(lik.type[[name]] %in% c('penalty','understocking','migrationpenalty','catchinkilos'))
        next
      lik[[name]]$weight <- 
        1/lik.init$data[lik.names[[name]]]
      if(lik.names[name] %in% group)
        lik[[name]]$weight <- 
          1e4/lik.init$data[lik.names[[name]]]
    }
    ## make sure that the likelihood is unique
    attr(lik,'file_name') <- paste('likelihood',group_name,sep = '.')
    write.gadget.file(lik, gv)
    return(gv)
  }
  
  grouping %>% 
    purrr::map(setup_function)
}



#' @describeIn gadget_iterative_stage_1
#'
#' @param variants list of gadget directories 
#' @param cv_floor minimum value for the survey CV
#'
#' @return gadget directory 
#' @export
gadget_iterative_stage_2 <- function(variants, cv_floor = 0){
  variants %>% 
    purrr::map(~read.gadget.parameters(paste(., attr(., 'params_in'), sep = '/'))) %>%
    purrr::map(wide_parameters) %>% 
    dplyr::bind_rows() %>% 
    write.gadget.parameters(paste(variants[[1]], attr(variants[[1]], 'variant.dir'), 'params.wgts', sep = '/'))
  
  gadget_evaluate(variants[[1]], params.in = 'params.wgts', lik.out = 'lik.wgts', params.out = tempfile())
  
  lik.wgts <- 
    read.gadget.lik.out(paste(variants[[1]], attr(variants[[1]], 'variant.dir'), 'lik.wgts', sep = '/'))
  
  if(is.null(lik.wgts))
    stop('Something went wrong when calling gadget')
  
  
  
  ## read model information
  main <- read.gadget.file(variants[[1]],attr(variants[[1]],'mainfile'), file_type = 'main', recursive = FALSE)
  
  ## read an editable, data free likelihood file
  lik <- 
    read.gadget.file(variants[[1]],main$likelihood$likelihoodfiles, file_type = 'likelihood', recursive = FALSE)
  
  lik.names <- 
    lik %>% 
    purrr::map('name') %>% 
    unlist() %>% 
    as.character()
  
  ## same with data to count the numbers of lines in the 
  lik.df <- 
    read.gadget.file(variants[[1]],main$likelihood$likelihoodfiles, file_type = 'likelihood', recursive = TRUE) %>% 
    purrr::set_names(lik.names) %>% 
    purrr::map('datafile') %>% 
    purrr::map(1) %>% 
    purrr::map(tibble::as_tibble) %>% 
    purrr::map(dplyr::summarise,n=dplyr::n()) %>% 
    dplyr::bind_rows(.id = 'component')
  
  
  lik.type <- 
    lik %>% 
    purrr::map('type') %>% 
    purrr::set_names(lik.names)
  
  SStable <- 
    lik.wgts$data %>% 
    dplyr::mutate(id = names(variants)) %>% 
    dplyr::select(all_of(c('id',lik.names)))
  
  weights <- 
    SStable %>% 
    tidyr::pivot_longer(-id,names_to = 'component',values_to = 'SS') %>%
    dplyr::left_join(lik.df, by = 'component') %>% 
    dplyr::left_join(tibble::tibble(component = names(lik.type),
                                    type = unlist(lik.type)),
                     by = 'component') %>%
    dplyr::filter(!(type %in% c('penalty','understocking','migrationpenalty','catchinkilos')))
  
  if(sum(weights$SS == 0) > 0){
    stop(paste("Likelihood component score exactly 0", weights$component[weigths$SS == 0]))
  }
  
  weights <- 
    weights %>%
    dplyr::mutate(variance = SS/n,
                  variance = ifelse(type == 'surveyindices', 
                                    pmax(variance,cv_floor^2), variance)) %>%
    dplyr::group_by(component) %>% 
    dplyr::summarise(w = mean(n)/min(pmax(SS,1))) 
  
  weights <- 
    weights$w %>% 
    purrr::set_names(weights$component)
  for(comp in seq_along(lik)){
    if(!lik.names[comp] %in% names(weights))
      next
    lik[[comp]]$weight <- 
      weights[lik.names[comp]]
  }
  
  gd <- variants[[1]]
  attr(gd, 'mainfile') <- paste(attr(gd, 'variant_dir'), 'main.final', sep = '/')
  attr(main, 'file_name') <- 'main.final'
  main$likelihood$likelihoodfiles <- NULL
  write.gadget.file(main,gd)
  attr(gd,'params_in') <- paste(attr(gd, 'variant_dir'), 'params.in', sep = '/')
  attr(gd,'params_out') <- paste(attr(gd, 'variant_dir'), 'params.final', sep = '/')
  attr(lik,'file_name') <- paste(attr(gd, 'variant_dir'), 'likelihood.final', sep = '/')
  write.gadget.file(lik, gd)
  return(gd)
}


#' Removed functions
#'
#' @param ... 
#'
#' @export
gadget.iterative <- function(...){
  print('gadget.iterative no longer supported use gadget_iterative_stage_* functions')
}

