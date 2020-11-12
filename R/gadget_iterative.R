#' Title
#'
#' @param gd 
#' @param grouping 
#' @param wgts 
#' @param params.in 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' library(Rgadget)
#' 
#' gd <- gadget.variant.dir('01-base')
#' 
#' gadget_iterative_stage_1(gd, params.in = 'params.in') %>% 
#'   parallel::mclapply(gadget_optimize, mc.cores = parallel::detectCores()) %>% 
#'   gadget_iterative_stage_2() %>% 
#'   gadget_optimize()
#' 
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
    dplyr::left_join(lik.df) %>% 
    dplyr::left_join(tibble::tibble(component = names(lik.type),
                                    type = unlist(lik.type))) %>%
    dplyr::filter(component %in% strsplit(id,'__'))
  
  if(sum(weigths$SS == 0) > 0){
    stop(paste("Likelihood component score exactly 0", weights$component[weigths$SS == 0]))
  }
  
  weights <- 
    weights %>%
    dplyr::mutate(variance = SS/n,
                  variance = ifelse(type == 'surveyindices', 
                                    pmin(variance,cv_floor^2), variance)) %>%
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
  attr(gd, 'mainfile') <- paste(attr(gd, 'variant_dir'), 'main.final')
  attr(main, 'file_name') <- 'main.final'
  write.gadget.file(main,gd)
  attr(gd,'params_in') <- paste(attr(gd, 'variant_dir'), 'params.in')
  return(gd)
}

