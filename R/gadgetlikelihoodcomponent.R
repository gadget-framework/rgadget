#' Wrapper to choose a component by name
#'
#' @param component	Call the corresponding gadget_(component)_component function
#' @param ...		Arguments to pass to it
#' @export
gadgetlikelihoodcomponent <- function (component, ...) {
    # Build a call to the relevant function, call it.
    x <- do.call(call, list(paste0('gadget_', component, '_component'), ...))
    eval(x)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:boundlike
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_penalty_component <- function (weight = 0, name = 'penalty', data = NULL) {
    data <- deparse_mfdb_data(data)
    obj <- list(datafile = gadgetfile(
        fname('Data', name, '.penaltyfile'),
        components = list(data = if (length(data) > 0) data else data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE))))
    finalise_component('penalty', obj, name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:understocking
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_understocking_component <- function (weight = 0, name = 'understocking') {
    finalise_component('understocking', list(), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:catchstat
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_catchstatistics_component <- function (
        weight = 0, name = 'catchstatistics',
        data_function = NULL,
        data = NULL, area = NULL, age = NULL,
        fleetnames = c(), stocknames = c()) {

    if (is.null(data)) {
        stop("No data provided")
    }
    data <- deparse_mfdb_data(data)

    # Work out data_function based how data was generated
    if (!is.null(data_function)) {
        # It's already set, so nothing to do
    } else if (is.null(attr(data, "generator"))) {
        stop("Cannot work out the required function, and data_function not provided")
    } else if (attr(data, "generator") == "mfdb_sample_meanlength_stddev") {
        data_function <- 'lengthgivenstddev'
    } else if (attr(data, "generator") == "mfdb_sample_meanlength") {
        data_function <- 'lengthnostddev'
    } else if (attr(data, "generator") == "mfdb_sample_meanweight_stddev") {
        data_function <- 'weightgivenstddev'
    } else if (attr(data, "generator") == "mfdb_sample_meanweight") {
        data_function <- 'weightnostddev'
    } else {
        stop(paste("Unknown generator function", attr(data, "generator")))
    }

    # Make sure we have the columns we need
    compare_cols(names(data), list(
        lengthcalcstddev = c("year", "step", "area", "age", "number", "mean"),
        lengthgivenstddev = c("year", "step", "area", "age", "number", "mean", "stddev"),
        weightgivenstddev = c("year", "step", "area", "age", "number", "mean", "stddev"),
        weightnostddev = c("year", "step", "area", "age", "number", "mean"),
        lengthnostddev = c("year", "step", "area", "age", "number", "mean"),
        null = NULL)[[data_function]])

    finalise_component('catchstatistics', list(
        datafile = gadgetfile(fname('Data', fname_prefix('catchstatistics', name), data_function), components=list(data=data)),
        "function" = data_function,
        areaaggfile = agg_file('area', fname_prefix('catchstatistics', name), if(is.null(area)) attr(data, "area") else area),
        ageaggfile  = agg_file('age', fname_prefix('catchstatistics', name), if(is.null(age)) attr(data, "age") else age),
        fleetnames = fleetnames,
        stocknames = stocknames), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:catchdist
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_catchdistribution_component <- function (
        weight = 0, name = 'catchdistribution',
        data_function = 'sumofsquares',
        data_function_params = list(),
        aggregationlevel = FALSE,
        overconsumption = FALSE,
        epsilon = 10,
        data = NULL, area = NULL, age = NULL, length = NULL,
        fleetnames = c(), stocknames = c()) {
    data <- deparse_mfdb_data(data)

    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", "age", "length", "number"))

    finalise_component('catchdistribution', c(
        list(
            datafile = gadgetfile(fname('Data', fname_prefix('catchdistribution', name), data_function), components=list(data=data)),
            "function" = data_function
        ),
        data_function_params,
        list(
            aggregationlevel = if (aggregationlevel) 1 else 0,
            overconsumption = if (overconsumption) 1 else 0,
            epsilon = epsilon,
            areaaggfile = agg_file('area', fname_prefix('catchdistribution', name), if(is.null(area)) attr(data, "area") else area),
            ageaggfile  = agg_file('age', fname_prefix('catchdistribution', name), if(is.null(age)) attr(data, "age") else age),
            lenaggfile  = agg_file('len', fname_prefix('catchdistribution', name), if(is.null(length)) attr(data, "length") else length),
            fleetnames = fleetnames,
            stocknames = stocknames)), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:stockdist
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_stockdistribution_component <- function (
        weight = 0, name = 'stockdistribution',
        data_function = 'sumofsquares',
        overconsumption = FALSE,
        epsilon = 10,
        data = NULL, area = NULL, age = NULL, length = NULL,
        fleetnames = c(), stocknames = c()) {
    data <- deparse_mfdb_data(data)

    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", NA, "age", "length", "number"))

    # For stock distribution, anything in column 4 should be called stock
    if (length(names(data)) > 4) {
        names(data)[4] <- 'stock'
    }

    finalise_component('stockdistribution', list(
        datafile = gadgetfile(fname('Data', fname_prefix('stockdistribution', name), data_function), components=list(data=data)),
        "function" = data_function,
        overconsumption = if (overconsumption) 1 else 0,
        epsilon = epsilon,
        areaaggfile = agg_file('area', fname_prefix('stockdistribution', name), if(is.null(area)) attr(data, "area") else area),
        ageaggfile  = agg_file('age', fname_prefix('stockdistribution', name), if(is.null(age)) attr(data, "age") else age),
        lenaggfile  = agg_file('len', fname_prefix('stockdistribution', name), if(is.null(length)) attr(data, "length") else length),
        fleetnames = fleetnames,
        stocknames = stocknames), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:surveyindices
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_surveyindices_component <- function (
        weight = 0, name = 'surveyindices',
        sitype = 'lengths',
        biomass = 0,
        data = NULL,
        area = NULL,
        fittype = NULL,
        length = NULL,
        age = NULL,
        fleetnames = NULL,
        surveynames = NULL,
        stocknames = NULL,
        slope = NULL,
        intercept = NULL) {
    if (is.null(fittype)) {
        stop("fittype missing. It is a required parameter")
    }
    data <- deparse_mfdb_data(data)

    if (sitype == 'lengths') {
        compare_cols(names(data), c("year", "step", "area", "length", "number"))
        si_cols <- list(
            lenaggfile  = agg_file(
                'len',
                fname_prefix('surveyindices', name),
                if(is.null(length)) attr(data, "length") else length))

    } else if (sitype == 'ages') {
        compare_cols(names(data), c("year", "step", "area", "age", "number"))
        si_cols <- list(
            ageaggfile  = agg_file(
                'age',
                fname_prefix('surveyindices', name),
                if(is.null(age)) attr(data, "age") else age))

    } else if (sitype == 'fleets') {
        compare_cols(names(data), c("year", "step", "area", "length", "number"))
        if (is.null(fleetnames)) {
            stop("Expected vector of fleetnames for effort surveyindices")
        }
        si_cols <- list(
            lenaggfile  = agg_file(
                'len',
                fname_prefix('surveyindices', name),
                if(is.null(length)) attr(data, "length") else length),
            fleetnames = fleetnames)

    } else if (sitype == 'acoustic') {
        compare_cols(names(data), c("year", "step", "area", NA, NA))
        if (is.null(surveynames)) {
            stop("Expected vector of surveynames for acoustic surveyindices")
        }
        si_cols <- list(surveynames = surveynames)

    } else if (sitype == 'effort') {
        compare_cols(names(data), c("year", "step", "area", NA, NA))
        if (is.null(fleetnames)) {
            stop("Expected vector of fleetnames for effort surveyindices")
        }
        si_cols <- list(fleetnames = fleetnames)

    } else {
        stop("Unknown sitype", sitype)
    }

    # Mix in other default columns
    finalise_component('surveyindices', c(
        list(
            datafile = gadgetfile(fname('Data', fname_prefix('surveyindices', name), sitype), components=list(data=data)),
            sitype = sitype,
            biomass = biomass,
            areaaggfile = agg_file('area', fname_prefix('surveyindices', name), if(is.null(area)) attr(data, "area") else area)),
        si_cols,
        if (is.null(stocknames)) c() else list(stocknames = stocknames),
        if (is.null(fittype)) c() else list(fittype = fittype),
        if (is.null(slope)) c() else list(slope = slope),
        if (is.null(intercept)) c() else list(intercept = intercept),
        NULL), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:surveydistribution
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_surveydistribution_component <- function (
        weight = 0, name = 'surveydistribution',
        data = NULL,
        area = NULL,
        length = NULL,
        age = NULL,
        stocknames = c(),
        fittype = 'linearfit',
        parameters = NULL,
        suitability = list(),
        slope = NULL,
        intercept = NULL,
        epsilon = 10,
        likelihoodtype = 'multinomial') {
    data <- deparse_mfdb_data(data)
    compare_cols(names(data), c("year", "step", "area", "age", "length", "number"))

    # Combine standard columns with fit type parameters
    finalise_component('surveydistribution', c(
        list(
            datafile = gadgetfile(fname('Data', fname_prefix('surveydistribution', name)), components=list(data=data)),
            areaaggfile = agg_file('area', fname_prefix('surveydistribution', name), if(is.null(area)) attr(data, "area") else area),
            lenaggfile  = agg_file('len', fname_prefix('surveydistribution', name), if(is.null(length)) attr(data, "length") else length),
            ageaggfile  = agg_file('age', fname_prefix('surveydistribution', name), if(is.null(age)) attr(data, "age") else age),
            stocknames = stocknames,
            fittype = fittype,
            parameters = parameters,
            # NB: No name, as gadget requires the suitability function on it's own line
            suitability),
        if (is.null(slope)) c() else list(slope = slope),
        if (is.null(intercept)) c() else list(intercept = intercept),
        list(
            epsilon = epsilon,
            likelihoodtype = likelihoodtype),
        NULL), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:stomach
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_stomachcontent_component <- function (
        weight = 0, name = 'stomachcontent',
        data_function = 'scsimple',
        epsilon = 10,
        area = NULL,
        predator_length = NULL,
        prey_length = NULL,
        prey_labels = list(),
        prey_digestion_coefficients = list(),
        predator_names = c(),
        data = NULL) {
    data <- deparse_mfdb_data(data)
    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", "predator_length", "prey_length", "ratio"))

    # Check prey_length is available
    if(is.null(prey_length)) prey_length <- attr(data, "prey_length")
    prey_minmax <- agg_prop(prey_length, "min/max")

    find_prey_metadata <- function(metadata, n) {
        # Backwards-compatibility for non-list args
        if(!is.list(metadata)) return(metadata)

        # Nothing in list
        if(length(metadata) == 0) return(c())

        # Nothing in list has names, so assume we want the first one.
        if(is.null(names(metadata))) return(metadata[[1]])

        # Treat each item name as a regexp, see if it matches
        for (i in seq_len(length(metadata))) {
            regexp <- names(metadata)[[i]]
            if (!nzchar(regexp) || grepl(paste0('^', regexp), n)) return(metadata[[i]])
        }
        return(c())
    }

    prey_components <- lapply(names(prey_length), function (name) {
        lbl <- find_prey_metadata(prey_labels, name)
        if (length(lbl) < 1) stop("No prey labels found for ", name)

        structure(
            list(
                name = NULL,
                lbls = (if (length(lbl) > 1) lbl[seq(2,length(lbl))]),
                lengths = prey_minmax[[name]],
                digestioncoefficients = find_prey_metadata(prey_digestion_coefficients, name)),
            names = c(name, lbl[[1]], 'lengths', 'digestioncoefficients'),
            preamble = "")
    })

    finalise_component('stomachcontent', list(
        "function" = data_function,
        datafile = gadgetfile(fname('Data', fname_prefix('stomachcontent', name), data_function), components=list(data=data)),
        epsilon = epsilon,
        areaaggfile = agg_file('area', fname_prefix('stomachcontent', name), if(is.null(area)) attr(data, "area") else area),
        predatornames = predator_names,
        predatorlengths = NULL,
        lenaggfile  = agg_file('len', fname_prefix('stomachcontent', name), if(is.null(predator_length)) attr(data, "predator_length") else predator_length),
        preyaggfile = gadgetfile(fname('Aggfiles', fname_prefix('stomachcontent', name), 'prey.agg'),components=prey_components)), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:recaptures
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_recaptures_component <- function (
        weight = 0, name = 'recaptures',
        data = NULL) {
    stop("Not implemented")
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:recstat
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_recstatistics_component <- function (
        weight = 0, name = 'recstatistics',
        data = NULL) {
    stop("Not implemented")
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:migpenalty
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_migrationpenalty_component <- function (
        weight = 0, name = 'migrationpenalty',
        stockname = c(),
        powercoeffs = c()) {

    finalise_component('migrationpenalty', list(
        stockname = stockname,
        powercoeffs = powercoeffs), name, weight)
}

#' http://hafro.github.io/gadget/docs/userguide/chap-like.html#sec:catchinkilos
#'
#' @param name		A descriptive name for the component, default same as component type
#' @param weight	A numeric weighting
#' @export
gadget_catchinkilos_component <- function (
        weight = 0, name = 'catchinkilos',
        data = NULL,
        data_function = 'sumofsquares',
        epsilon = 10,
        area = NULL,
        fleetnames = c(), stocknames = c()) {
    data <- deparse_mfdb_data(data)
    # Make sure we have the columns we need
    compare_cols(names(data), c("year", "step", "area", NA, "total_weight"))

    # If aggregated yearly, then switch to aggregationlevel 1 and drop step column
    if (isTRUE(identical(attr(data, 'step'), mfdb::mfdb_timestep_yearly))) {
        aggregationlevel <- 1
        data <- data[,names(data) != 'step', drop = FALSE]
    } else {
        aggregationlevel <- 0
    }

    finalise_component('catchinkilos', list(
        datafile = gadgetfile(fname('Data', fname_prefix('catchinkilos', name), data_function), components=list(data=data)),
        "function" = data_function,
        aggregationlevel = aggregationlevel,
        epsilon = epsilon,
        areaaggfile = agg_file('area', fname_prefix('catchinkilos', name), if(is.null(area)) attr(data, "area") else area),
        fleetnames = fleetnames,
        stocknames = stocknames), name, weight)
}

############# Internal helpers

deparse_mfdb_data <- function (data) {
    if(!is.null(data) && !is.data.frame(data)) {
        if (length(data) == 1 && is.data.frame(data[[1]])) {
            # List-wrapped data.frame from mfdb_*, be nice and unwrap it.
            return(data[[1]])
        }
        stop("data supplied is a ", class(data), ", not a data.frame.")
    }
    return(data)
}

finalise_component <- function (type, obj, name, weight) {
    # Wrap up with common bits of class
    obj <- structure(c(
        list(name = name, weight = weight, type = type),
        obj
    ), class = c(  # TODO: Removed likelihoodfile, I don't think it's useful?
        paste0("gadget_", type, "_component"),
        "gadgetlikelihoodcomponent"))
    return(obj)
}

# Transform agg summary by either applying func_name, or fishing out pre-baked values
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

agg_file <- function (type, prefix, data) {
    if (type == 'area') {
        # Areas should just be a => 1, b => 2, ...
        comp <- structure(
            as.list(seq_len(length(data))),
            names = names(data))
    } else if (type == 'len') {
        # Lengths should be min/max
        comp <- agg_prop(data, "min/max")
    } else {
        # Convert to list
        comp <- agg_prop(data, "c")
    }

    return(gadgetfile(
        fname('Aggfiles', prefix, type, '.agg'),
        components=list(comp)))
}

# Prefix for filenames based on callee and likelihood name
fname_prefix <- function (fn, name) {
    paste0(
        fn,
        '.',
        name,
        '.')
}

#TODO: Find the Rgadget equivalent
fname <- function (dir, ...) {
    file.path(dir, paste0(c(...), collapse = ""))
}

#TODO: Find the Rgadget equivalent
# Make sure the data frame colums match what is required
compare_cols <- function (actual, expected) {
    if (is.null(expected)) return(invisible(NULL))

    if (length(actual) == 0) {
        stop("data given to ",
            as.character(sys.call(-1)[[1]]),
            " is empty")
    }

    # Fill NAs in expected with whatever we did get
    expected[is.na(expected)] <- actual[is.na(expected)]

    if (!isTRUE(all.equal(actual, expected))) {
        stop(
            as.character(sys.call(-1)[[1]]),
            " expects data to have columns '",
            paste(expected, collapse=","),
            "', not '",
            paste(actual, collapse=","),
            "'")
    }
    return(invisible(NULL))
}
