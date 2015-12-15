#' Create a gadgetstock object
#'
#' Create a gadgetstock object, from fresh or an existing file
#'
#' @param stock_name	The name of the stock, as 
#' @param path		The path to the gadget directory to read from
#' @param missingOkay	If \code{TRUE}, return an empty gadgetstock object if file does not exist.
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
        for (comp in c('doesgrow', 'naturalmortality', 'iseaten', 'doeseat', 'initialconditions',
                       'doesmigrate', 'doesmature', 'doesmove', 'doesrenew', 'doesspawn', 'doesstray')) {
            gf <- gadget_update(gf, comp, 0)
        }
    }

    return(gf)
}

#' Update comonents of a gadgetstock object
#'
#' Replace components of a gadgetstock with either new values or or content
#' derived from an MFDB query.
#'
#' @param gf		The gadgetfile object to update
#' @param component	The component to update, or 'stock' to update the initial values
#' @param ...		Either 0, data = (mfdb_query), or keys to update. See details.
#'
#' @details
#' If \code{...} is \code{0}, then the component is replaced with "doesgrow 0", for instance.
#' If \code{...} is \code{data = mfdb_query}, then the component is generated based
#' on the data. How depends in which component:
#' \describe{
#'   \item{stock} The query should contain age & length columns, inserts min/max for both
#'   \item{refweight} The query should contain age & length columns, inserted into refweight file
#'   \item{initialconditions} The query should contain area, age, length, number, mean columns
#'   \item{doesrenew} The query should contain year, step, area, age, length, number, mean columns 
#' }
#' Finally, any other value of \code{...} will update the relevant keys/values in that component.
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
    # Resolve any aliases to what's used in GADGET
    comp_alias <- list(
        stock = 1,  # i.e. the first unlabelled component
        migration = 'doesmigrate',
        growth = 'doesgrow',
        mortality = 'naturalmortality',
        prey = 'iseaten',
        predator = 'doeseat',
        migration = 'doesmigrate',
        maturation = 'doesmature',
        movement = 'doesmove',
        recruitment = 'doesrenew',
        renewal = 'doesrenew')[[component]]
    if (!is.null(comp_alias)) component <- comp_alias

    if (isTRUE(all.equal(args, list(0)))) {
        if (component == 'initialconditions') {
            gf[[component]] <- list()
        } else {
            # Only argument is null, so turn this off.
            gf[[component]] <- structure(list(0), names = c(component), class=c("gadgetstock_component", "list"))
        }

    } else if (component == 1 && isTRUE(all.equal(names(args), c('data')))) {
        for (col in c('age', 'length')) {
            if (!(col %in% colnames(data))) {
                stop("Data missing column ", col)
            }
        }

        gf[[1]]$minage <- min(unlist(agg_prop(attr(data, 'age'), "min")))
        gf[[1]]$maxage <- max(unlist(agg_prop(attr(data, 'age'), "max")))
        gf[[1]]$minlength <- min(unlist(agg_prop(attr(data, 'length'), "min")))
        gf[[1]]$maxlength <- max(unlist(agg_prop(attr(data, 'length'), "max")))

    } else if (component == 'refweight' && isTRUE(all.equal(names(args), c('data')))) {
        data <- args$data
        for (col in c('length', 'mean')) {
            if (!(col %in% colnames(data))) {
                stop("Data missing column ", col)
            }
        }

        # Sort incoming data, then regroup
        refwgt <- data[order(data$length), c('length', 'mean')]
        refwgt <- data.frame(
            length = unlist(agg_prop(attr(data, 'length')[refwgt$length], "min")), # Grouping -> minimum value
            weight = refwgt$mean,  # Assuming it's mean weight here
            stringsAsFactors = TRUE)
        gf[[1]]$dl <- min(unlist(agg_prop(attr(data, 'length'), "diff")))
        gf[[1]]$refweightfile <- gadgetfile(paste0('Modelfiles/', stock_name, '.refwgt'), file_type = "data", list(data = refwgt))

    } else if (component == 'initialconditions' && isTRUE(all.equal(names(args), c('data')))) {
        data <- args$data
        for (col in c('area', 'age', 'length', 'number', 'mean')) {
            if (!(col %in% colnames(data))) {
                stop("Data missing column ", col)
            }
        }

        numberfile <- data.frame(
            area = data$area,
            age = data$age,
            length = unlist(agg_prop(attr(data, 'length')[data$length], "min")), # Grouping -> minimum value
            number = data$number,
            weight = data$mean,  # Assuming it's mean weight here
            stringsAsFactors = TRUE)
        gf$initialconditions <- list(
            minage = min(unlist(agg_prop(attr(data, 'age'), "min"))),
            maxage = max(unlist(agg_prop(attr(data, 'age'), "max"))),
            minlength = min(unlist(agg_prop(attr(data, 'length'), "min"))),
            maxlength = max(unlist(agg_prop(attr(data, 'length'), "max"))),
            dl = min(unlist(agg_prop(attr(data, 'length'), "diff"))),
            numberfile = gadgetfile(paste0('Modelfiles/', stock_name, '.init.number'), file_type = "data", data = numberfile))

    } else if (component == 'doesrenew' && isTRUE(all.equal(names(args), c('data')))) {
        data <- args$data
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
            stringsAsFactors = TRUE)
        gf$doesrenew <- list(
            doesrenew = 1,
            minlength = min(unlist(agg_prop(attr(data, 'length'), "min"))),
            maxlength = max(unlist(agg_prop(attr(data, 'length'), "max"))),
            dl = min(unlist(agg_prop(attr(data, 'length'), "diff"))),
            numberfile = gadget_file(paste0('Modelfiles/', stock_name, '.rec.number'), file_type = "data", data = numberfile))
    
    } else {
        # Update the selected component with variables provided
        for (n in names(args)) {
            # TODO: Memory-efficiency?
            gf[[component]][[n]] <- args[[n]]
        }

        if (component != 1) {
            # make sure "does"whatever is 1
            gf[[component]][[component]] <- 1
        }

        if (component == 1 && 'stockname' %in% names(args)) {
            # Stockname changed, so update the file name we use
            attr(gf, 'file_name') <- args$stockname
        }
    }

    return(gf)
}
