#' Create a gadgetstock object
#'
#' Create a gadgetstock object, from fresh or an existing file
#'
#' @param stock_name The name of the stock, as 
#' @param path The path to the gadget directory to read from
#' @param missingOkay If \code{TRUE}, return an empty gadgetstock object if file does not exist.
#' @return A list of lists representing the gadget stock file
#' @examples
#' path <- './cod-model'
#' gadgetstock('codmat', path, missingOkay = TRUE)  # Read codmat from './cod-model', throw an error if it doesn't exist
#' gadgetstock('codimm', path, missingOkay = TRUE)  # Read codimm stock / create a new one
#' @export
gadgetstock <- function(stock_name, path, missingOkay = FALSE) {
    # Fetch the name of all stockfiles
    mainfile_name <- 'main'
    if (isTRUE(nzchar(attr(path, 'mainfile')))) mainfile_name <- attr(path, 'mainfile')
    mainfile <- read.gadget.file(path, mainfile_name, file_type = "main", missingOkay = TRUE)
    stock_files <- mainfile$stock$stockfiles

    # Try each stock file to see if it's the one we want
    gf <- NULL
    for (stock_file in grep(stock_name, stock_files, value = TRUE)) {
        gf <- read.gadget.file(path, stock_file, file_type = 'stock', missingOkay = missingOkay)
        if (length(gf) > 0 && stock_name == gf[[1]]$stockname) break
        gf <- NULL
    }

    if (is.null(gf)) {
        # Can't find it, so either error or create a new one.
        gf <- read.gadget.file(path, stock_name, file_type = 'stock', missingOkay = missingOkay)
    }

    # Sanitise stockfile
    class(gf) <- c("gadgetstock", class(gf))
    if (length(gf) == 0) {
        gf[[1]] <- list(
            stockname = stock_name,
            livesonareas = c(),
            minage = c(),
            maxage = c(),
            minlength = c(),
            maxlength = c(),
            dl = c(),
            refweightfile = NULL,
            growthandeatlengths = NULL)
        defaults <- list(
            doesgrow = 1,
            naturalmortality = c(),
            iseaten = 0,
            doeseat = 0,
            initialconditions = 0,
            doesmigrate = 0,
            doesmature = 0,
            doesmove = 0,
            doesrenew = 0,
            doesspawn = 0,
            doesstray = 0)
        for (comp in names(defaults)) gf <- gadget_update(gf, comp, defaults[[comp]])
    }

    return(gf)
}

#' Update comonents of a gadgetstock object
#'
#' Replace components of a gadgetstock with either new values or or content
#' derived from an MFDB query.
#'
#' @param gf The gadgetfile object to update
#' @param component The component to update, or 'stock' to update the initial values
#' @param ... Either 0, data = (mfdb_query), or keys to update. See details.
#' 
#' If \code{...} is \code{0}, then the component is replaced with "doesgrow 0", for instance.
#' If \code{...} is \code{data = mfdb_query}, then the component is generated based
#' on the data. How depends in which component:
#' \describe{
#'   \item{stock}{The query should contain age & length columns, inserts min/max for both}
#'   \item{refweight}{The query should contain age & length columns, inserted into refweight file}
#'   \item{initialconditions}{The query should contain area, age, length, number, mean columns}
#'   \item{doesrenew}{The query should contain year, step, area, age, length, number, mean columns} 
#' }
#' Finally, any other value of \code{...} will update the relevant keys/values in that component.
#'
#' naturalmortality is slightly different. It takes a single vector with one value per-age-group.
#' It will be pre-populated with 0.2 for each age group.
#'
#' doesgrow / growth can be populated with default values with \code{gadget_update(gs, 'growth', 1)}
#' 
#' \code{gadget_update('refweight', data = data.frame(length = ..., weight = ...))} will also update
#' minlength/maxlength/dl
#'
#' \code{gadget_update('refweight', data = data.frame(length = ..., alpha = ..., beta = ...))} will
#' generate lengths via \code{alpha * length^beta}
#'
#' doesmature / maturation will populate the maturityfile data file with all given parameters apart
#' from \code{maturityfunction}, e.g.
#' \code{gadget_update('maturation',maturityfunction = 'constant', maturestocksandratios = c( ... ), coefficients = c( ... ))}
#'
#' doesrenew / recruitment can be populated from MFDB queries, e.g.
#' \code{gadget_update('doesrenew', number = data)}
#'
#' @examples
#' path <- './model'
#' gadgetstock('codimm', path, missingOkay = TRUE) %>%  # Create a skeleton if missing
#'    gadget_update('stock', minage = 2, maxage = 4) %>%
#'    gadget_update('stock', stockname = 'codmat') %>%  # Will change the name of the file we write
#'    gadget_update('initialconditions', data = agg_file) %>%
#'    gadget_update('doesmigrate', yearstepfile = gadgetfile('data/yearstepfile', components = list(
#'                                   data.frame(year = 1998, step = 1:4, matrix = 'codmat-migration')))) %>%
#'    gadget_update('doesrenew', 0) %>%
#'    write.gadget.file(path)
#' @export
gadget_update.gadgetstock <- function(gf, component, ...) {
    args <- list(...)

    # Check component name, resolving any aliases
    comp_aliases <- list(
        stock = 1,  # i.e. the first unlabelled component
        refweight = 'refweight',  # NB: This is a cheat, it's really part of 'stock'
        doesgrow = 'doesgrow', growth = 'doesgrow',
        naturalmortality = 'naturalmortality', mortality = 'naturalmortality',
        iseaten = 'iseaten', prey = 'iseaten',
        doeseat = 'doeseat', predator = 'doeseat',
        initialconditions = 'initialconditions',
        doesmigrate = 'doesmigrate', migration = 'doesmigrate',
        doesmature = 'doesmature', maturation = 'doesmature',
        doesmove = 'doesmove', movement = 'doesmove',
        doesrenew = 'doesrenew', recruitment = 'doesrenew', renewal = 'doesrenew',
        doesspawn = 'doesspawn', spawning = 'doesspawn',
        doesstray = 'doesstray', straying = 'doesstray',
        null = NULL)
    if (is.null(comp_aliases[[component]])) stop(
        "Unknown component '", component, "'. ",
        "Expected one of: ", paste(names(comp_aliases), collapse = ", "),
        "")
    component <- comp_aliases[[component]]

    if (isTRUE(all.equal(args, list(0)))) {
        if (component == 'initialconditions') {
            gf[[component]] <- list()
        } else {
            # Only argument is null, so turn this off.
            gf[[component]] <- structure(list(0), names = c(component), class=c("gadgetstock_component", "list"))
        }

    } else if (component == 'doesgrow' && isTRUE(all.equal(args, list(1)))) {
        # Populate doesgrow with defaults
        gf$doesgrow <- list(
            doesgrow = 1,
            growthfunction = 'lengthvbsimple',
            growthparameters = list(
                linf = paste0('#', gf[[1]]$stockname, '.Linf'),
                k = sprintf('(* 0.001 #%s.k)',gf[[1]]$stockname),
                sprintf('#%s.walpha',gf[[1]]$stockname),
                sprintf('#%s.wbeta',gf[[1]]$stockname)),
            beta = sprintf('(* 10 #%s.bbin)',gf[[1]]$stockname),
            maxlengthgroupgrowth = 15)

    } else if (component == 1 && isTRUE(all.equal(names(args), c('data')))) {
        for (col in c('age', 'length')) {
            if (!(col %in% colnames(data))) {
                stop("Data missing column ", col)
            }
        }

        gf[[1]][c('minage', 'maxage', 'minlength', 'maxlength')] <- list(
            min(unlist(agg_prop(attr(data, 'age'), "min"))),
            max(unlist(agg_prop(attr(data, 'age'), "max"))),
            min(unlist(agg_prop(attr(data, 'length'), "min"))),
            max(unlist(agg_prop(attr(data, 'length'), "max"))))
        # Update naturalmortality defaults
        gf <- gadget_update(gf, 'naturalmortality', c())
        
          

    } else if (component == 'refweight' && isTRUE(all.equal(names(args), c('length', 'alpha', 'beta')))) {
        refwgt <- data.frame(
            length = args$length,
            weight = args$alpha * args$length ^ args$beta,
            stringsAsFactors = TRUE)

        gf[[1]][c('minlength', 'maxlength', 'dl', 'refweightfile')] <- list(
            min(refwgt$length),
            max(refwgt$length),
            min(diff(refwgt$length)),
            gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.refwgt'), refwgt))

    } else if (component == 'refweight' && isTRUE(all.equal(names(args), c('data')))) {
        data <- args$data
        if (ncol(data) < 2) stop("data should have 2 columns")

        if ('length' %in% names(data)) {
            if ('length' %in% names(attributes(data))) {
                # It's an MFDB table, use the minimum length for each group
                lengths <- unlist(agg_prop(attr(data, 'length')[data$length], "min"))
                min_length <- min(lengths)
                max_length <- max(unlist(agg_prop(attr(data, 'length')[data$length], "max")))
                dl_length <- min(unlist(agg_prop(attr(data, 'length'), "diff")))
            } else {
                # Regular table, copy lengths
                lengths <- data$length
                min_length <- min(lengths)
                max_length <- max(lengths)
                dl_length <- min(diff(lengths[order(lengths)]))
            }
            length_col <- 'length'
        } else stop("data has no length column")

        if ('weight' %in% names(data)) {
            weights <- data$weight
        } else if ('mean' %in% names(data)) {
            # MFDB mean (hopefully) length
            weights <- data$mean
        } else stop("data has no weight column")

        # Create sorted data.frame
        refwgt <- data.frame(length = lengths, weight = weights)
        refwgt <- refwgt[order(refwgt$length), c('length', 'weight'), drop = FALSE]

        gf[[1]][c('minlength', 'maxlength', 'dl', 'refweightfile')] <- list(
            min_length,
            max_length,
            dl_length,
            gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.refwgt'), refwgt))

    } else if (component == 'initialconditions') {
      if(isTRUE(all.equal(names(args), c('number')))) {
        data <- args$number
        for (col in c('area', 'age', 'length', 'number', 'weight')) {
          if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
          }
        }
        
        numberfile <- data.frame(
          age = data$age,
          area = data$area,
          length = unlist(agg_prop(attr(data, 'length')[data$length], "min")), # Grouping -> minimum value
          number = data$number,
          weight = data$weight,  # Assuming it's mean weight here
          stringsAsFactors = FALSE)
        gf$initialconditions <- list(
          minage = ifelse(is.null(args$minage),gf[[1]]$minage,args$minage),
          maxage = ifelse(is.null(args$maxage),gf[[1]]$maxage,args$maxage),
          minlength = ifelse(is.null(args$minlength),gf[[1]]$minlength,args$minlength),
          maxlength = ifelse(is.null(args$maxlength),gf[[1]]$maxlength,args$maxlength),
          dl = ifelse(is.null(args$dl),gf[[1]]$dl,args$dl),
          numberfile = gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.init.number'), numberfile))
        
        
      } else if (isTRUE(all.equal(names(args), c('normalcond')))) {
        data <- args$normalcond
        for (col in c('area', 'age','age.factor','area.factor', 'mean', 'stddev', 'relcond')) {
          if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
          }
        }
        
        numberfile <- data.frame(
          age = data$age,
          area = data$area,
          age.factor = data$age.factor,
          area.factor = data$area.factor,
          mean = data$mean,  
          stddev = data$stddev,
          relcond = data$relcond,
          stringsAsFactors = FALSE)
        gf$initialconditions <- list(
          minage = ifelse(is.null(args$minage),gf[[1]]$minage,args$minage),
          maxage = ifelse(is.null(args$maxage),gf[[1]]$maxage,args$maxage),
          minlength = ifelse(is.null(args$minlength),gf[[1]]$minlength,args$minlength),
          maxlength = ifelse(is.null(args$maxlength),gf[[1]]$maxlength,args$maxlength),
          dl = ifelse(is.null(args$dl),gf[[1]]$dl,args$dl),
          normalcondfile = gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.init.normalcond'), numberfile))

      } else if (isTRUE(all.equal(names(args), c('normalparam')))) {
        data <- args$normalparam
        for (col in c('area', 'age','age.factor','area.factor', 'mean', 'stddev', 'alpha','beta')) {
          if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
          }
        }
        
        numberfile <- data.frame(
          age = data$age,
          area = data$area,
          age.factor = data$age.factor,
          area.factor = data$area.factor,
          mean = data$mean,  
          stddev = data$stddev,
          alpha = data$alpha,
          beta = data$beta,
          stringsAsFactors = FALSE)
        gf$initialconditions <- list(
          minage = ifelse(is.null(args$minage),gf[[1]]$minage,args$minage),
          maxage = ifelse(is.null(args$maxage),gf[[1]]$maxage,args$maxage),
          minlength = ifelse(is.null(args$minlength),gf[[1]]$minlength,args$minlength),
          maxlength = ifelse(is.null(args$maxlength),gf[[1]]$maxlength,args$maxlength),
          dl = ifelse(is.null(args$dl),gf[[1]]$dl,args$dl),
          normalparamfile = gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.init.normalparam'), numberfile))
        
      } else {
        stop("No initialcondition scheme found - allowed options are:\n- number\n- normalcond\n- normalparam")
      } 
      
      
    } else if (component == 'doesmature' && 'maturityfunction' %in% names(args)) {
        gf$doesmature <- list(
            doesmature = 1,
            maturityfunction = args$maturityfunction,
            maturityfile = gadgetfile(
                paste0('Modelfiles/', gf[[1]]$stockname, '.maturity'),
                components = list(args[names(args) != 'maturityfunction'])))
    } else if (component == 'doesmove') {
      gf$doesmove <- list(
        doesmove = 1,
        transitionstocksandratios = args$transitionstocksandratios,
        transitionstep = args$transitionstep)
      
    } else if (component == 'doesrenew') {
      if(isTRUE(all.equal(names(args), c('number')))) {
        data <- args$number
        for (col in c('year', 'step', 'area', 'age', 'length', 'number', 'mean')) {
            if (!(col %in% colnames(data))) {
                stop("Data missing column ", col)
            }
        }

        numberfile <- data.frame(
            year = data$year,
            step = data$step,
            area = data$area,
            age = data$age,
            length = unlist(agg_prop(attr(data, 'length')[data$length], "min")), # Grouping -> minimum value
            number = data$number,
            weight = data$mean,  # Assuming it's mean weight here
            stringsAsFactors = FALSE)
        gf$doesrenew <- list(
            doesrenew = 1,
            minlength = min(unlist(agg_prop(attr(data, 'length'), "min"))),
            maxlength = max(unlist(agg_prop(attr(data, 'length'), "max"))),
            dl = min(unlist(agg_prop(attr(data, 'length'), "diff"))),
            numberfile = gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.rec.number'), numberfile))
        
      } else if (isTRUE(all.equal(names(args), c('normalcond')))) {
          data <- args$normalcond
          for (col in c('year', 'step', 'area', 'age', 'number', 'mean', 'stddev', 'relcond')) {
            if (!(col %in% colnames(data))) {
              stop("Data missing column ", col)
            }
          }
          
          numberfile <- data.frame(
            year = data$year,
            step = data$step,
            area = data$area,
            age = data$age,
            number = data$number,
            mean = data$mean,  
            stddev = data$stddev,
            relcond = data$relcond,
            stringsAsFactors = FALSE)
          gf$doesrenew <- list(
            doesrenew = 1,
            minlength = ifelse(is.null(args$minlength),gf[[1]]$minlength,args$minlength),
            maxlength = ifelse(is.null(args$maxlength),gf[[1]]$maxlength,args$maxlength),
            dl = ifelse(is.null(args$dl),gf[[1]]$dl,args$dl),
            normalcondfile = gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.rec.normalcond'), numberfile))
      
      } else if (isTRUE(all.equal(names(args), c('normalparam')))) {
        data <- args$normalparam
        for (col in c('year', 'step', 'area', 'age', 'number', 'mean', 'stddev', 'alpha','beta')) {
          if (!(col %in% colnames(data))) {
            stop("Data missing column ", col)
          }
        }
        
        numberfile <- data.frame(
          year = data$year,
          step = data$step,
          area = data$area,
          age = data$age,
          number = data$number,
          mean = data$mean,  
          stddev = data$stddev,
          alpha = data$alpha,
          beta = data$beta,
          stringsAsFactors = FALSE)
        gf$doesrenew <- list(
          doesrenew = 1,
          minlength = ifelse(is.null(args$minlength),gf[[1]]$minlength,args$minlength),
          maxlength = ifelse(is.null(args$maxlength),gf[[1]]$maxlength,args$maxlength),
          dl = ifelse(is.null(args$dl),gf[[1]]$dl,args$dl),
          normalparamfile = gadgetdata(paste0('Modelfiles/', gf[[1]]$stockname, '.rec.normalparam'), numberfile))
      } else {
        stop("No recruiment scheme found - allowed options are:\n- number\n- normalcond\n- normalparam")
      }
      
    } else if (component == 'naturalmortality') {
        # Assume size of age groups is 1
        extra_age_groups <- gf[[1]]$maxage - gf[[1]]$minage + 1 - length(args[[1]])
        if(length(extra_age_groups) != 1) extra_age_groups <- 0

        gf$naturalmortality <- list(naturalmortality = c(
            args[[1]], rep(sprintf('#%s.M',gf[[1]]$stockname), 
                           times = extra_age_groups),
            NULL))

    } else if (component == 'iseaten'){
      
      gf$iseaten <- list(
        iseaten = 1,
        preylengths = gf[[1]]$growthandeatlengths,
        energycontent = ifelse(is.null(args$energycontent),1,args$energycontent))
      
    } else {
        # Update the selected component with variables provided
        gf[[component]][names(args)] <- args

        if (component != 1) {
            # make sure "does"whatever is 1
            gf[[component]][[component]] <- 1
        }

        if (component == 1 && 'stockname' %in% names(args)) {
            # Stockname changed, so update the file name we use
            attr(gf, 'file_name') <- args$stockname
        } 
        if (component == 1 && ('minage' %in% names(args) || 'maxage' %in% names(args))) {
            # Update naturalmortality defaults
            gf <- gadget_update(gf, 'naturalmortality', c())
        } 
        if(component == 1 && ('minlength' %in% names(args) & 'maxlength' %in% names(args) & 
                                     'dl' %in% names(args))){
          gf['growthandeatlengths'] <- NULL
          # Update growthandeatlengths 
          lgr <- seq(gf[[1]]$minlength,gf[[1]]$maxlength,by=gf[[1]]$dl)
          len.agg <- data.frame(name = paste0('len',head(lgr,-1)),
                                lower = head(lgr,-1),
                                upper = tail(lgr,-1))
          gf[[1]]$growthandeatlengths <- 
            gadgetdata(paste0('Aggfiles/', gf[[1]]$stockname, '.stock.len.agg'), len.agg)
        }
    }

    return(gf)
}

# If data has an attribute of name func_name, return that
# otherwise evaluate func_name(data)
agg_prop <- function (data, func_name) {
    get_prop <- function (x, func_name) {
        if (func_name == "diff") {
            return(diff(get_prop(x, "min/max")))
        }
        if (func_name == "min/max") {
            return(c(get_prop(x, "min"), get_prop(x, "max")))
        }
        if (!is.null(attr(x, func_name))) {
            return(attr(x, func_name))
        }
        # No shortcut attribute, eval x properly
        do.call(func_name, list(eval(x)))
    }

    lapply(as.list(data), function (d) get_prop(d, func_name))
}


gadget_datatable <- function(type = 'renewal',format = 'normalparam',stockname = 'imm',
                             growth = c(Linf='#%s.linf', k='(* 0.01 #%s.k )',
                                        alpha = '#%s.walpha', beta='#%s.wbeta',
                                        recl = '#%s.recl'),
                             sigma = '#%s.recsd',
                             ...){
  if(!(type %in% c('renewal','initial'))){
    stop('Type not supported. Allowed types are:\n- renewal\n- inital')
  }
  ## param names
  if(type=='renewal'){
    param <- 'rec'
  } else {
    param <- 'age'
  }
  
  ## find age range
  if(is.null(args$minage)){
    stop('No minimum age given')
  } else {
    minage <- args$minage
    if(is.null(args$maxage)){
      maxage <- minage
    }
  }
  
  ## mean length at age
  t0 <- sprintf('(+ %s (/ (log (- 1 (/ %s %s))) %s))',
                rec.age,
                sprinft(growth['recl'], stockname),
                sprinft(growth['linf'], stockname),
                sprinft(growth['k'], stockname))
  
  mu.string <- '( * %s (-  1 (exp (* (* -1 %s ) (- %s %s)))))'
  mu <- sprintf(mu.string,
                sprintf(growth['linf'], stockname),
                sprintf(growth['k'], stockname),
                minage:maxage,t0)
  
  ## stock multiplier (x1e4)
  st.mult <- sprintf('(* %1$s.%2$s.mult #%1$s.%2$s.%%s)',stockname,param)
  
  if(type == 'initial'){
    st.mult <- sprintf(st.mult,minage:maxage)
    if(format == 'normalcond'){
      data.frame(age = minage:maxage,
                 area = 1,
                 age.factor = st.mult,
                 area.factor = 1,
                 mean = mu,
                 stddev = sigma,
                 relcond = 1,
                 stringsAsFactors = FALSE)
    } else if(format == 'normalparam'){
      data.frame(age = minage:maxage,
                 area = 1,
                 age.factor = st.mult,
                 area.factor = 1,
                 mean = mu,
                 stddev = sigma,
                 alpha = sprintf(growth['']),
                 stringsAsFactors = FALSE)
    }
  }
  
}