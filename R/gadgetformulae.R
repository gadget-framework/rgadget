##' @title Parse a GADGET formulae string
##' @param input_formulae A single formulae string, e.g. "(+ 2 (log (- #moo 1)))"
##' @return An R expression representing the gadget formulae, which could be run with \code{eval(.., list(moo = 3))}
##' @export
parse.gadget.formulae <- function(input_formulae) {
    index <- 1  # NB: We use this as a global in the recursion below

    # Ingest anything that matches the regexp, shunting our pointer along if we do
    ingest_regexp <- function (r) {
        subst <- substring(input_formulae, index)
        m <- regmatches(subst, regexec(paste0("^\\s*", r), subst, perl = TRUE))[[1]]

        if (length(m) > 0) {
            index <<- index + nchar(m[[1]])
            # Return first captured expression, or the whole thing
            return(m[[if (length(m) > 1) 2 else 1]])
        }
        return(NULL)
    }

    # Recursively pick off formulae expressions
    get_expression <- function () {
        # Ingest any intial bracket
        open_bracket <- identical(ingest_regexp("(\\()"), "(")

        # Ingest token (anything up until whitespace, closing bracket or end-of-string)
        token <- ingest_regexp("(.+?)(?=\\s|\\)|$)")
        if (!isTRUE(nzchar(token))) {
            stop("Could not find a function name")
        }

        if (substring(token, 1, 1) == "#") {
            # Token is a name
            if (open_bracket) ingest_regexp("\\)")
            return(as.name(substring(token, 2)))
        }

        if (!is.na(suppressWarnings(as.numeric(token)))) {
            # Token is a numeric constant
            if (open_bracket) ingest_regexp("\\)")
            return(as.numeric(token))
        }

        # Assume anything else is a function call
        # Ingest parameters until we hit a close bracket
        # NB: Any function parameters will ingest their own close bracket
        out <- list(as.name(token))
        while(!identical(ingest_regexp("(\\))"), ")")) {
            out[[length(out) + 1]] <- get_expression()
        }

        return(as.call(out))
    }

    # Start recursion
    return(get_expression())
}

##' @title Turn R expression into GADGET formulae string
##' @param ex An unevaluated R expression, e.g. quote(2 + log(moo - 1))
##' @return A character vector representing the GADGET formulae, e.g. "(+ 2 (log (- #moo 1)))"
##' @export
to.gadget.formulae <- function(ex) {
    if (is.name(ex)) {
        # It's a variable reference
        return(paste0("#", ex))
    }

    if (is.numeric(ex) || is.character(ex)) {
        # It's a constant, stringify it
        return(as.character(ex))
    }

    if (is.call(ex)) {
        ex_fn <- as.character(ex[[1]])
        ex_args <- if (length(ex) > 1) as.list(ex[2:length(ex)]) else list()

        if (ex_fn == "(" && length(ex_args) == 1) {
            # Pass over R's explicit bracket "function"
            return(to.gadget.formulae(ex_args[[1]]))
        }

        # Function call, write out in RPN
        return(paste0(
            "(", ex[[1]], " ",
            paste(lapply(ex_args, to.gadget.formulae), collapse = " "),
            ")"))
    }

    stop("Don't know what to do with: ", capture.output(str(ex)))
}

# Characters look like a gadget formulae
possible.gadget.formulae <- function(x) {
    grepl("^\\s*\\(.*\\)\\s*$", x)
}

##' @title Replace variables in formulae
##' @param ex		An unevaluated R expression or the result of parse.gadget.formulae, e.g. quote(2 + log(moo - 1))
##' @param replacements	A list of variable names to either: a TimeVariable file, replacement gadget/R formulae, or value
##' @param year		If using TimeVariable files, specify the year to pull out of file
##' @param step		If using TimeVariable files, specify the step to pull out of file
##' @return An R expression representing the gadget formulae, which could be run with \code{eval(.., list(moo = 3))}
##' @examples
##' sub.gadget.formulae(quote(log(moo) + oink + baa), list(moo = "#fish", oink = quote(2 + 2), baa = 5))
##' # ==> log(fish) + (2 + 2) + 5
##'
##' \dontrun{
##' tv <- read.gadget.file('.', 'timevariable', file_type = 'timevariable')
##' sub.gadget.formulae(quote(log(moo) + oink), list(moo = tv), year = 1995, step = 1)
##' # ==> log(grow1995) + oink
##' }
##' @export
sub.gadget.formulae <- function (
        ex,
        replacements,
        year = stop('Specify year to use timevariable'),
        step = stop('Specify step to use timevariable')) {
    if (is.name(ex)) {
        repl <- replacements[[as.character(ex)]]

        if (is.null(repl)) {
            # No valid replacement, leave alone
            return (ex)
        }

        if ("gadgetfile" %in% class(repl)) {
            # Repl is a gadgetfile, assume timevariable and try replacing
            repl <- repl[[1]]$data
            repl <- as.character(repl[repl$year == year & repl$step == step, 'value'])
            if (length(repl) == 0) {
                stop("No value for ", as.character(ex), " timevariable, year/step ", year, "/", step)
            }
        }

        if(is.name(repl) || is.numeric(repl) || is.call(repl)) {
            # Formulae-like thing, use that instead
            return(repl)
        }

        if(isTRUE(nzchar(repl))) {
            # Assume strings are gadget formulae
            return(parse.gadget.formulae(repl))
        }

        stop("Not sure how to replace ", as.character(ex), " with", repl)
    }

    if (is.numeric(ex) || is.character(ex)) {
        return(ex)
    }

    if (is.call(ex)) {
        # Recurse into every argument of call
        return(as.call(lapply(seq_along(ex), function (i) {
            if (i > 1) sub.gadget.formulae(ex[[i]], replacements, year, step) else ex[[i]];
        })))
    }

    stop("Don't know what to do with: ", capture.output(str(ex)))
}
