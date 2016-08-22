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
        out <- list(token)
        while(!identical(ingest_regexp("(\\))"), ")")) {
            out[[length(out) + 1]] <- get_expression()
        }

        # NB: do.call evaluates inputs, so hard-code calls to call()
        if (length(out) == 1) return(call(out[[1]]))
        if (length(out) == 2) return(call(out[[1]], out[[2]]))
        if (length(out) == 3) return(call(out[[1]], out[[2]], out[[3]]))
        stop("Unsupported " + (length(out) - 1) + "-ary function")
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
