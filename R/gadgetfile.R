# Helper to pull file_type out of datafile
get_filetype <- function (file_type) {
    t <- Rgadget::gadget_filetypes[Rgadget::gadget_filetypes$file_type == file_type,]
    if (nrow(t) == 0) stop("Unknown gadget file type ", file_type)
    lapply(as.list(t), function (x) if (is.factor(x)) as.character(x) else x)
}

#' Construct a new gadgetfile S3 object
#'
#' Constructor for objects representing a gadget model input
#' file.
#'
#' gadgetfile objects break all files down into the following form:
#' \preformatted{; preamble before the first unnamed component
#' key	value
#' ; preamble before the second named component
#' [component2_name]
#' key1	value
#' key2	value
#' ; preamble before the second named component
#' [component3_name]
#' key	value
#'   . . .
#' ; preamble before the data component
#' [data_component]
#' ; -- data --
#' ; a	b	c
#' 1	2	3
#' }
#'
#' Note that values can also be \code{gadgetfile} objects, in this case
#' That gadgetfile is written out separately, and value is replaced with
#' the filename of the sub-file.
#'
#' @param file_name	Filename the output should be written to / read from
#' @param file_type	What sort of gadget file this is, e.g. 'time', 'area', ...
#'			See \code{Rgadget::gadget_filetypes} for recognised types.
#'			Setting this will enable any quirks of that particular file type,
#' @param components	A list of lists, representing each component.
#' @return A gadgetfile S3 class, use \code{write.gadget.file} to write it to disk
#' @examples
#' gadgetfile("cod.likelihood", "likelihood",
#'    components = list(
#'        component = list(type = "penalty", aggfile = gadgetfile("aggfile", "areaagg", components = list()))
#'    ))
#' @export
gadgetfile <- function (file_name, file_type = "generic", components = list()) {
    structure(
        components,
        names = names(components),
        file_name = file_name,
        file_config = get_filetype(file_type),
        class = c("gadgetfile", "list"))
}

#' Construct a data-only gadget file
#'
#' @param file_name	Filename the output should be written to / read from
#' @param data		A data.frame that the file should contain
#' @return A gadgetfile S3 class, use \code{write.gadget.file} to write it to disk
#' @export
gadgetdata <- function (file_name, data) {
    gf <- gadgetfile(file_name, file_type = "data", list(data))
    class(gf) <- c("gadgetdata", class(gf))
    return(gf)
}

#' Update gadget components
#'
#' Update a component within this file.
#'
#' @param gf		The gadgetfile object to update
#' @param component	The component to update
#' @param ...		Keys to update.
#'
#' @details
#' For more information, see a specific implementation:
#' \enumerate{
#'   \item \link{gadget_update.gadgetstock}
#' }
#'
#' @export
gadget_update <- function(gf, component, ...) UseMethod("gadget_update", gf)

#' Recognised GADGET file types and their quirks
#'
#' \describe{
#'     \item{file_type}{The name of the file type}
#'     \item{mainfile_section}{Writing a file of this type will cause the corresponding mainfile entry to be updated}
#'     \item{bare_component}{Components in this file do not have brackets around their names}
#'     \item{implicit_component}{Keys matching this regex are treated as if the are implicitly broken up into components}
#' }
#' 
#' @name gadget_filetypes
#' @docType data
NULL

is_implicit_component <- function(config, name) {
    if (!isTRUE(nzchar(name))) return(FALSE)
    if (!isTRUE(nzchar(config$implicit_component))) return(FALSE)
    if (regexpr(config$implicit_component, name) < 0) return(FALSE)
    return(TRUE)
}

#' Print given gadgetfile to stdout
#'
#' @param x	gadgetfile object to print
#' @param ...	unused
#' @export
print.gadgetfile <- function (x, ...) {
    preamble_str <- function (obj) {
        lines <- as.list(attr(obj, 'preamble'))
        if (length(lines) > 0) paste0("; ", lines, "\n", collapse = "") else ""
    }
    print_component <- function (comp, name, file_config) {
        # Print all preambles as comments
        cat(preamble_str(comp))

        if (!is.character(name) || !nzchar(name)) {
            # No name, do nothing
        } else if (is_implicit_component(file_config, name)) {
            # Do nothing, the name comes from the key/value line
        } else if (isTRUE(file_config$bare_component)) {
            cat(paste0(name,'\n'))
        } else {
            cat(paste0('[', name,']\n'))
        }

        # If it's a data-frame component, just print it out
        if (is.data.frame(comp)) {
            cat("; -- data --\n; ")
            write.table(comp,
                file = "",
                quote = FALSE,
                sep = "\t",
                col.names = TRUE,
                row.names = FALSE,
                fileEncoding = "utf-8")
        } else if (is.list(comp)) {
            # properties are in key\tvalue1\tvalue2... form
            for (i in seq_len(length(comp))) {
                cat(names(comp)[[i]], "\t", sep = "")
                cat(if ("gadgetfile" %in% class(comp[[i]])) attr(comp[[i]], 'file_name') else comp[[i]], sep = "\t")

                if (length(attr(comp[[i]], "comment")) > 0) {
                    if (length(comp[[i]]) > 0) cat("\t\t")
                    cat("; ", attr(comp[[i]], "comment"), sep = "")
                }
                cat("\n")
            }
        } else {
            stop("Type of component, ", name, " unknown")
        }
    }

    # Print header to top of file
    cat(paste0("; Generated by Rgadget ", packageVersion("Rgadget"), "\n"))

    for (i in seq_len(length(x))) {
        print_component(x[[i]], names(x)[[i]], file_config = attr(x, 'file_config'))
    }
}

#' Write gadgetfile to disk, including any dependant files, and update the mainfile
#'
#' @param obj		gadgetfile object to write
#' @param path		Base directory to write out to
#' @param mainfile	The name of the directories mainfile (or NULL to disable mainfile updating)
#' @export
write.gadget.file <- function(obj, path, mainfile = 'main') {
    file_name <- attr(obj, 'file_name')
    file_config <- attr(obj, 'file_config')

    dir.create(
        dirname(file.path(path, file_name)),
        recursive = TRUE,
        showWarnings = FALSE)

    # For each component, inspect for any stored gadgetfiles and write these out first
    for (comp in obj) {
        if (is.list(comp)) for (field in comp) {
            if ("gadgetfile" %in% class(field)) {
                write.gadget.file(field, path, mainfile = mainfile)
            }
        }
    }

    fh = file(file.path(path, file_name), "w")
    tryCatch(
        capture.output(print(obj), file = fh),
        finally = close(fh))

    # If this file is mentioned by the mainfile, also update that
    if (!is.null(mainfile) && !is.na(file_config$mainfile_section)) {
        do.call(gadget_mainfile_update, c(
            list(path, mainfile),
            structure(list(file_name), names = c(file_config$mainfile_section)),
        NULL))
    }
}

#' Read gadget input file, return gadgetfile S3 class representing file
#'
#' @param path		Base directory to read file from
#' @param file_name	File name, or path relative to (path), to read
#' @param file_type	What sort of gadget file this is, e.g. 'time', 'area', ...
#'			See \code{Rgadget::gadget_filetypes} for recognised types
#' @param fileEncoding	Character encoding of file, defaults to "UTF-8"
#' @param missingOkay	If \code{TRUE}, return an empty gadgetfile object if file does not exist.
#' @export
read.gadget.file <- function(path, file_name, file_type = "generic", fileEncoding = "UTF-8", missingOkay = FALSE) {
    extract <- function (pattern, line) {
        if (length(line) == 0) return(c())
        m <- regmatches(line, regexec(pattern, line))[[1]]
        if (length(m) > 1) m[2:length(m)] else c()
    }
    file_config <- get_filetype(file_type)

    # Open file
    full_path <- file.path(path, file_name)
    if (file.access(full_path, 4) == -1) {
        if (isTRUE(missingOkay)) {
            return(gadgetfile(file_name, file_type = file_type))
        }
        stop("File ", file_name, " does not exist")
    }
    file <- file(full_path, "rt", encoding = fileEncoding)
    on.exit(close(file))

    components <- list()
    component_names <- list()
    comp_name <- ""
    cur_comp <- list()
    cur_preamble <- list()

    while(TRUE) {
        line <- readLines(file, n = 1)

        # Ignore version preamble, since this will be replaced on output
        if (length(grep("^; Generated by", line)) > 0) {
            next
        }

        # Switching to data mode
        if (length(line) > 0 && is.data.frame(cur_comp)) {
            header <- strsplit(line, "\\s")[[1]]
            if(length(header) < 2) stop(paste("Not enough parts in data header", header))
            header <- header[2:length(header)]  # Remove initial ';'
            cur_comp <- read.table(file,
                header=FALSE,
                quote = "",
                sep = "\t",
                col.names = header,
                fileEncoding = fileEncoding)
            attr(cur_comp, 'preamble') <- cur_preamble
            cur_preamble <- list()
            next
        }

        # Start of new component / end of file
        x <- extract(paste(c(
            if(isTRUE(file_config$bare_component)) "^(\\w+)$" else NULL,
            "^; -- (data) --$",
            "^\\[(\\w+)\\]"), collapse = "|"), line)
        x <- x[nzchar(x)]  # Throw away matches that didn't work
        if (length(line) == 0 || length(x) > 0) {
            # Put old component on heap
            component_names[[length(component_names) + 1]] <- comp_name
            components[[length(components) + 1]] <- cur_comp

            if (length(line) == 0) {
                # End of file, finish now.
                break
            } else if (x[[1]] == 'data') {
                comp_name <- ""
                cur_comp <- data.frame()
            } else {
                comp_name <- x[[1]]
                cur_comp <- list()
            }
            next
        }

        # Add any full-line comments as a preamble
        x <- extract("^;\\s*(.*)", line)
        if (length(x) > 0) {
            cur_preamble <- c(cur_preamble, list(x[[1]]))
            next
        }

        # Any other line shoud be a tab seperated list
        match <- extract("([a-zA-Z0-9\\-_]*)\\s+([^;]*);?\\s*(.*)", line)
        line_name <- match[[1]]
        line_values <- if (length(match[[2]]) > 0) unlist(strsplit(sub("\\s+$", "", match[[2]]), "\\t+")) else c()
        line_comment <- match[[3]]

        # This might be an implicit component, if so start a new component but carry on parsing
        if (is_implicit_component(file_config, line_name)) {
            # Put old component on heap
            component_names[[length(component_names) + 1]] <- comp_name
            components[[length(components) + 1]] <- cur_comp

            comp_name <- line_name
            cur_comp <- list()
        }

        if (length(line_name) > 0) {
            # Started writing items, so must have got to the end of the preamble
            if (length(cur_preamble) > 0) {
                attr(cur_comp, 'preamble') <- cur_preamble
                cur_preamble <- list()
            }

            # Append to cur_comp
            cur_comp[[length(cur_comp) + 1]] <- structure(
                tryCatch(as.numeric(line_values), warning = function (w) line_values),
                comment = (if (nzchar(line_comment)) line_comment else NULL))
            names(cur_comp)[[length(cur_comp)]] <- line_name
            next
        }
    }
    gadgetfile(
        file_name = basename(file_name),
        file_type = file_type,
        components = structure(components, names = component_names))
}

# For each option, make sure values contained are in main file
gadget_mainfile_update <- function (
        path,
        mainfile = 'main',
        timefile = NULL,
        areafile = NULL,
        printfiles = NULL,
        stockfiles = NULL,
        tagfiles = NULL,
        otherfoodfiles = NULL,
        fleetfiles = NULL,
        likelihoodfiles = NULL,
        fileEncoding = "UTF-8") {
    made_change <- FALSE
    swap <- function (old_val, repl_val, single = FALSE) {
        # NULL means leave alone
        if (is.null(repl_val)) return(old_val);

        # Merge vectors
        new_val <- if (single) repl_val else unique(c(old_val, repl_val))
        if (!identical(all.equal(old_val, new_val), TRUE)) {
            made_change <<- TRUE
        }
        return(new_val);
    }

    # Read file, create basic outline if doesn't exist
    mfile <- read.gadget.file(path, mainfile, file_type = 'main', fileEncoding = fileEncoding, missingOkay = TRUE)
    if (length(mfile) == 0) {
        mfile <- gadgetfile(mainfile, file_type = 'main', components = list(
            list(timefile = "", areafile = "", printfiles = structure(c(), comment = "Required comment")),
            stock = list(),
            tagging = list(),
            otherfood = list(),
            fleet = list(),
            likelihood = list()))
    }

    # Do simple swaps first
    mfile[[1]]$timefile <- swap(mfile[[1]]$timefile, timefile, single = TRUE)
    mfile[[1]]$areafile <- swap(mfile[[1]]$areafile, areafile, single = TRUE)

    # Printfiles is mandatory, but can specify empty by adding a comment
    mfile[[1]]$printfiles <- swap(mfile[[1]]$printfiles, printfiles)

    # Rest are in their own component
    mfile$stock$stockfiles <- swap(mfile$stock$stockfiles, stockfiles)
    mfile$tagging$tagfiles <- swap(mfile$tagging$tagfiles, tagfiles)
    mfile$otherfood$otherfoodfiles <- swap(mfile$otherfood$otherfoodfiles, otherfoodfiles)
    mfile$fleet$fleetfiles <- swap(mfile$fleet$fleetfiles, fleetfiles)
    mfile$likelihood$likelihoodfiles <- swap(mfile$likelihood$likelihoodfiles, likelihoodfiles)

    # Write file back out again
    if (made_change) write.gadget.file(mfile, path)
}
