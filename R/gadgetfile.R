# Helper to pull file_type out of datafile
get_filetype <- function (file_type) {
  t <- Rgadget::gadget_filetypes[Rgadget::gadget_filetypes$file_type == file_type,]
  if (nrow(t) == 0) stop("Unknown gadget file type ", file_type)
  lapply(as.list(t), function (x) if (is.factor(x)) as.character(x) else x)
}

# Split line whitespace-separated sections, keeping formulae together
split_gadgetfile_line <- function (line) {
  # Count the number of times (char) occurs in (string)
  countchar <- function (string, char) {
    vapply(regmatches(string, gregexpr(char, string, fixed = TRUE)), length, 0)
  }
  join_strings <- function (string) {
    paste(string, collapse = " ")
  }
  
  # Remove all comments
  line <- gsub(';.+', '', line)
  # Split by any whitespace first
  parts <- unlist(strsplit(line, "\\s+"))
  # Compare count of opening brackets and closing brackets in parts
  stack <- countchar(parts, "(") - countchar(parts, ")")
  # 0 ==> part of an unclosed expression, 1 ==> combine with any previous 0 parts
  stack <- c(1, ifelse(cumsum(stack) > 0, 0, 1))[1:length(stack)]
  # Use this as a factor to split up the parts into groups of whole expressions
  parts <- split(parts, cumsum(stack))
  # Collapse groups back together
  return(vapply(parts, join_strings, "", USE.NAMES = FALSE))
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
#'   \item \link{gadget_update.gadgetlikelihood}
#' }
#' @export
gadget_update <- function(gf, component, ...) UseMethod("gadget_update", gf)

#' Discard named component
#' 
#' Remove unwanted components from file
#' @param gf		The gadgetfile object to update
#' @param comp_name named components to remove
#' @export 
gadget_discard <- function(gf,comp_name, ...) UseMethod("gadget_discard",gf)

#' Recognised GADGET file types and their quirks
#'
#' \describe{
#'     \item{file_type}{The name of the file type}
#'     \item{mainfile_section}{Writing a file of this type will cause the corresponding mainfile entry to be updated}
#'     \item{bare_component}{Keys matching this regex (or every component, if TRUE) do not have brackets around their names}
#'     \item{implicit_component}{Keys matching this regex are treated as if the are implicitly broken up into components}
#' }
#' 
#' @name gadget_filetypes
#' @docType data
NULL

is_sub_component <- function(config, name) {
  if (!isTRUE(nzchar(name))) return(FALSE)
  if (!isTRUE(nzchar(config$sub_component))) return(FALSE)
  if (regexpr(config$sub_component, name) < 0) return(FALSE)
  return(TRUE)
}

is_implicit_component <- function(config, name) {
  if (!isTRUE(nzchar(name))) return(FALSE)
  if (!isTRUE(nzchar(config$implicit_component))) return(FALSE)
  if (regexpr(config$implicit_component, name) < 0) return(FALSE)
  return(TRUE)
}

is_bare_component <- function(config, name) {
  if (config$bare_component == "TRUE") return(TRUE)
  if (config$bare_component == "FALSE") return(FALSE)
  if (nzchar(config$bare_component) && regexpr(config$bare_component, name) >= 0) return(TRUE)
  return(FALSE)
}

bare_component_regex <- function(config) {
  if (config$bare_component == "TRUE") return("^(\\w+)$")
  if (config$bare_component == "FALSE") return(NULL)
  if (nzchar(config$bare_component)) return(config$bare_component)
  return(NULL)
}

#' Print given gadgetfile to stdout
#'
#' @param x	gadgetfile object to print
#' @param ...	unused
#' @export
print.gadgetfile <- function (x, ...) {
  args <- list(...)
  print_comments <- function (obj, name='preamble') {
    lines <- as.list(attr(obj, name))
    if (length(lines) > 0) {
      cat(paste0("; ", lines, "\n", collapse = ""))
    }
  }
  print_component <- function (comp, name, file_config) {
    # Print all preambles as comments
    print_comments(comp, 'preamble')
    
    if (is.data.frame(comp)) {
      cat("; -- data --\n")
    } else if (!is.character(name) || !nzchar(name)) {
      # No name, do nothing
    } else if (is_implicit_component(file_config, name)) {
      # Do nothing, the name comes from the key/value line
    } else if (is_bare_component(file_config, name)) {
      cat(paste0(name,'\n'))
    } else {
      cat(paste0('[', name,']\n'))
    }
    
    # If it's a data-frame component, just print it out
    if (is.data.frame(comp)) {
      for (i in seq_len(ncol(comp))) {
        if (length(comp[,i]) == 0) {
          # Nothing to convert
          next
        } else if (is.call(comp[1, i][[1]])) {
          # Convert formulae column to character
          comp[, i] <- vapply(comp[, i], to.gadget.formulae, "")
        }
      }
      cat("; ")
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
        print_comments(comp[[i]], 'preamble')
        cat(names(comp)[[i]])
        trailing_str <- "\n"
        if (length(comp[[i]]) == 1 && is.na(comp[[i]])) {
          # Don't print anything extra for NAs
          cat("\t")
        } else if ("gadgetfile" %in% class(comp[[i]])) {
          # Print gadget file path, not file
  
          file_name <- attr(comp[[i]], 'file_name')
          if(!is.null(args$path)){
            variant_dir <- attr(args$path, 'variant_dir')
            if (isTRUE(nzchar(variant_dir))) {
              file_name <- variant_full_path(variant_dir, file_name)
            } 
          }
            
          cat("\t")
          cat(file_name, sep = "")
        } else if ("gadget_file" %in% class(comp[[i]])) {
          # Print MFDB gadget_file
          file_name <- comp[[i]]$filename
          if(!is.null(args$path)){
            variant_dir <- attr(args$path, 'variant_dir')
            if (isTRUE(nzchar(variant_dir))) {
              file_name <- variant_full_path(variant_dir, file_name)
            } 
          }
          
          cat("\t")
          cat(file_name, sep = "")
        } else if (is_sub_component(file_config, names(comp)[[i]]) && is.list(comp[[i]])) {
          # Subcomponent
          cat("\n")
          print_component(comp[[i]], "", file_config)
          trailing_str <- ""
        } else if (is.call(comp[[i]])) {
          # Single forumla value (as opposed to a list of formulae)
          cat("\t")
          cat(to.gadget.formulae(comp[[i]]))
        } else {
          cat("\t")
          cat(vapply(comp[[i]], function (x) {
            ifelse(is.call(x), to.gadget.formulae(x), as.character(x))
          }, ""), sep = "\t")
        }
        
        if (length(attr(comp[[i]], "comment")) > 0) {
          if (length(comp[[i]]) > 0) cat("\t\t")
          cat("; ", attr(comp[[i]], "comment"), sep = "")
        }
        cat(trailing_str)
        print_comments(comp[[i]], 'postamble')
      }
    } else {
      stop("Type of component, ", name, " unknown")
    }
    
    print_comments(comp, 'postamble')
  }
  
  # Print header to top of file
  cat(paste0("; Generated by Rgadget ", packageVersion("Rgadget"), "\n"))
  
  for (i in seq_len(length(x))) {
    print_component(x[[i]], names(x)[[i]], file_config = attr(x, 'file_config'))
  }
}

#' Write the changes to the model into a model variant directory
#'
#' @param path		Base directory to write out to
#' @param variant_dir	A subdirectory to write any changes out to
#' @param mainfile	The name of the variant directories' mainfile
#' @export
gadget.variant.dir <- function(path, variant_dir = NULL, mainfile = 'main') {
  return(structure(
    as.character(path),
    variant_dir = variant_dir,
    mainfile = variant_full_path(variant_dir, mainfile),
    class = c("gadget.variant", "list")))
}

# Prepend variant_dir to file_name unless it already has it
variant_full_path <- function(variant_dir, file_name) {
  base <- file.path(variant_dir, "")
  
  if (grepl(paste0("^", base), file_name)) {
    # Starts with variant_dir already
    return(file_name)
  } else {
    return(file.path(variant_dir, file_name))
  }
}

# Strip variant_dir from file_name, if there
variant_strip_path <- function(variant_dir, file_name) {
  base <- file.path(variant_dir, "")
  
  return(sub(paste0("^", base), "", file_name))
}

#' Write gadgetfile to disk, including any dependant files, and update the mainfile
#'
#' @param obj		gadgetfile object to write
#' @param path		Base directory to write out to
#' @param recursive	Write out all nested files too (default TRUE)?
#' @export
write.gadget.file <- function(obj, path, recursive = TRUE) {
  file_name <- attr(obj, 'file_name')
  file_config <- attr(obj, 'file_config')
  
  mainfile <- attr(path, 'mainfile')
  if (!isTRUE(nzchar(mainfile))) {
    mainfile <- 'main'
  }
  
  # Is the path a model variant?
  variant_dir <- attr(path, 'variant_dir')
  if (isTRUE(nzchar(variant_dir))) {
    file_name <- variant_full_path(variant_dir, file_name)
  }
  
  dir.create(
    dirname(file.path(path, file_name)),
    recursive = TRUE,
    showWarnings = FALSE)
  
  # For each component, inspect for any stored gadgetfiles and write these out first
  write_comp_subfiles <- function(comp) {
    if (!is.list(comp)) return()
    
    for (field in comp) {
      if ("gadget_file" %in% class(field)) {
        # MFDB-style gadget_file object, convert first
        if(is.null(field$data)){
          field <- gadgetfile(
            field$filename,
            file_type = "generic",
            field$components)  
        } else{
          field <- gadgetfile(
            field$filename,
            file_type = "generic",
            c(field$components, list(data = field$data)))
        }
      }
      if ("gadgetfile" %in% class(field)) {
        if (isTRUE(nzchar(variant_dir))) {
          attr(field, 'file_name') <- variant_full_path(
            variant_dir,
            attr(field, 'file_name'))
        }
        write.gadget.file(field, path)
      } else {
        write_comp_subfiles(field)
      }
    }
  }
  if (recursive) write_comp_subfiles(obj)
  
  fh = file(file.path(path, file_name), "w")
  tryCatch(
    capture.output(print(obj,path=path), file = fh),
    finally = close(fh))
  
  # If this file is mentioned by the mainfile, also update that
  if (!is.null(mainfile) && !is.na(file_config$mainfile_section)) {
    do.call(gadget_mainfile_update, c(
      list(path, mainfile),
      structure(list(file_name), 
                names = c(file_config$mainfile_section)),
      structure(list(file_config$mainfile_overwrite),
                names = 'overwrite'),
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
#' @param recursive	Read in all nested files too (default TRUE)?
#' @export
read.gadget.file <- function(path, file_name, file_type = "generic", 
                             fileEncoding = "UTF-8", missingOkay = FALSE, recursive = TRUE) {
  extract <- function (pattern, line) {
    if (length(line) == 0) return(c())
    m <- regmatches(line, regexec(pattern, line))[[1]]
    if (length(m) > 1) m[2:length(m)] else c()
  }
  
  # Append (new) to (l), optionally naming it (name)
  list_append <- function (l, name, new) {
    l[[length(l) + 1]] <- new
    if(length(name) > 0 && nzchar(name)) {
      names(l)[[length(l)]] <- name
    } else {
      names(l)[[length(l)]] <- ""
    }
    return(l)
  }
  file_config <- get_filetype(file_type)
  
  is_readable <- function (path) {
    # TRUE iff we can read the file path
    file.access(path, 4) == 0
  }
  
  is_open <- function (fh) {
    # Fixed isOpen that returns FALSE when file is closed
    tryCatch(isOpen(fh), error = function (e) FALSE)
  }
  
  is_eof <- function (line) {
    # EOF is a 0-length vector
    return(length(line) == 0)
  }
  
  is_component_header <- function (line, default = NA) {
    # Is this line an implicit component?
    line_name <- extract('^([a-zA-Z0-9\\-_]*)', line)
    if (length(line_name) > 0 && is_implicit_component(file_config, line_name)) {
      # Implicit component
      return(list(name = line_name, type = 'list', implicit = TRUE))
    }
    
    # Is this a data table separator?
    if (line == '; -- data --') {
      return(list(name = '', type = 'data.frame'))
    }
    
    # Is this line a component separator?
    x <- extract(paste(c(
      bare_component_regex(file_config),
      "^\\[(\\w+)\\]"), collapse = "|"), line)
    x <- x[nzchar(x)]  # Throw away matches that didn't work
    if (length(x) > 0) {
      return(list(name = x[[1]], type = 'list'))
    }
    
    # Is this line a subcomponent?
    if (is_sub_component(file_config, line_name)) {
      return(list(
        name = line_name,
        type = 'list',
        implicit = (line_name != line),  # i.e there's more data on this line
        sub_component = TRUE))
    }
    
    # Nothing we understand, return default
    return(default)
  }
  
  read_component_header <- function (fh) {
    line <- readLines(fh, n = 1)
    if (is_eof(line)) {
      stop("Reached EOF! Shouldn't do here")
    }
    
    # Is the current line a component separator? If not, it's the first component
    header <- is_component_header(line, default = list(name = NULL, type = 'list', implicit = TRUE))
    
    # If implicit, rewind so this line can be parsed again
    if (isTRUE(header$implicit)) {
      pushBack(line, fh)
    }
    
    return(header)
  }
  
  read_preamble <- function (fh) {
    # Ingest comments until first 'real' line
    preamble <- NULL
    while(TRUE) {
      line <- readLines(fh, n = 1)
      if (is_eof(line)) {
        return(preamble)
      }
      
      if(regexec('^; Generated by', line) > -1) {
        # Ignore version preamble, since this will be replaced on output
        next
      }
      
      comment <- extract("^;\\s*(.*)", line)
      if (is.null(comment) || comment == "-- data --") {
        # Reached a non-comment line, rewind so we re-ingest this line and return
        pushBack(line, fh)
        return(preamble)
      } else {
        # It's a comment, add it to heap
        if (is.null(preamble)) preamble <- list()
        preamble <- c(preamble, comment)
      }
    }
  }
  
  read_component <- function (fh, reading_subcomponent = FALSE) {
    # Read any preamble comments
    comp_preamble <- read_preamble(fh)
    # Read component header
    comp_header <- read_component_header(fh)
    comp_postamble <- NULL
    
    if (comp_header$type == 'data.frame') {
      # Component is a data.frame
      line <- readLines(fh, n = 1)
      header <- strsplit(line, "\\s")[[1]]
      if(length(header) < 2) stop(paste("Not enough parts in data header", header))
      header <- header[2:length(header)]  # Remove initial ';'
      
      # Read table into buffer
      data_fifo <- file("")
      while (TRUE) {
        line_preamble <- read_preamble(fh)
        line <- readLines(fh, n = 1)
        
        if (is_eof(line)) {
          close(fh)
          comp_postamble <- line_preamble
          break
        }
        if (is.list(is_component_header(line))) {
          # Rewind to before preamble
          if (length(line_preamble) > 0) {
            pushBack(c(paste("; ", unlist(line_preamble)), line), fh)
          } else {
            pushBack(line, fh)
          }
          break
        }
        # Split line up so we can sanitise spacing
        line <- split_gadgetfile_line(line)
        writeLines(paste(line, collapse = "\t"), data_fifo)
      }
      
      # Re-read the buffer
      cur_comp <- read.table(data_fifo,
                             header=FALSE,
                             quote = "",
                             sep = "\t",
                             col.names = header,
                             comment.char = ";",
                             fileEncoding = 'utf8',
                             stringsAsFactors = FALSE)
      attr(cur_comp, 'preamble') <- comp_preamble
      
      # Test columns for gadget formule, if so convert
      # NB: We don't use colClasses to get a list instead of vector column
      for (i in seq_len(ncol(cur_comp))) {
        if (possible.gadget.formulae(as.character(cur_comp[1, i]))) {
          cur_comp[[i]] <- I(lapply(as.character(cur_comp[[i]]), parse.gadget.formulae))
        }
      }
      
      cur_preamble <- list()
      close(data_fifo)
    } else {
      # Read component as list
      cur_comp <- list()
      while(TRUE) {
        if (!is_open(fh)) break
        line_preamble <- read_preamble(fh)
        line <- readLines(fh, n = 1)
        
        if (is_eof(line)) {
          # We're done here.
          close(fh)
          comp_postamble <- line_preamble
          break
        }
        
        # Break up line into name\tvalues...; comment
        line <- stringr::str_trim(line)
        match <- extract("([a-zA-Z0-9\\-_]*)\\s*([^;]*);?\\s*(.*)", line)
        line_name <- match[[1]]
        line_values <- if (nzchar(match[[2]])) split_gadgetfile_line(match[[2]]) else c()
        line_values <- if (length(line_values) > 0) type.convert(line_values, as.is = TRUE) else as.numeric(c())
        line_comment <- if (length(match[[3]]) > 0 && nzchar(match[[3]])) match[[3]] else NULL
        
        # If there are any gadget formulae here, convert to list and parse them
        if(any(possible.gadget.formulae(line_values))) {
          line_values <- lapply(line_values, function (x) {
            if (possible.gadget.formulae(x)) parse.gadget.formulae(x) else x
          })
        }
        
        line_comp <- is_component_header(line)
        if (!isTRUE(comp_header$implicit) && is.list(line_comp)) {
          # Rewind to before preamble
          if (length(line_preamble) > 0) {
            pushBack(c(paste("; ", unlist(line_preamble)), line), fh)
          } else {
            pushBack(line, fh)
          }
          
          if (!reading_subcomponent && isTRUE(line_comp$sub_component)) {
            line_values <- read_component(fh, reading_subcomponent = TRUE)$component
            if (isTRUE(all.equal(names(line_values), line_name))) {
              # sub-component with only one value, so smoosh it down to a regular line
              # (i.e. it's not really a sub-component, but used to signify the end of one)
              line_values <- line_values[[1]]
            }
          } else {
            break
          }
        }
        comp_header$implicit <- FALSE  # Moved on from implicit component, so check on following rounds
        
        # If this is a reference to a gadget_file, read it in
        if (recursive && grepl("^amount$|file$", line_name) && class(line_values) == "character" && length(line_values) == 1) {  # NB: Can't have a vector of gadgetfile
          line_values <- read.gadget.file(
            path,
            line_values,
            file_type = "generic",
            fileEncoding = fileEncoding,
            missingOkay = FALSE)
        }
        
        cur_comp <- list_append(cur_comp, line_name, structure(line_values, preamble = line_preamble, comment = line_comment))
      }
    }
    
    return(list(
      name = comp_header$name,
      component = structure(
        cur_comp,
        preamble = comp_preamble,
        postamble = comp_postamble)))
  }
  
  # Open file
  open_file <- function(full_path) {
    # Open file if we can, or return NULL
    if (!is_readable(full_path)) return(NULL)
    return(file(full_path, "rt", encoding = fileEncoding))
  }
  variant_dir <- attr(path, 'variant_dir')
  fh <- NULL
  if (isTRUE(nzchar(variant_dir))) {
    # Try opening the file in a variant directory first
    file_name <- variant_strip_path(variant_dir, file_name)
    fh <- open_file(file.path(path, variant_dir, file_name))
  }
  if (is.null(fh)) {
    # No variant dir (or file doesn't have variant version yet)
    fh <- open_file(file.path(path, file_name))
  }
  if (is.null(fh)) {
    # Still haven't found anything to read
    if (isTRUE(missingOkay)) {
      return(gadgetfile(file_name, file_type = file_type))
    } else {
      stop("File ", variant_dir, file_name, " does not exist")
    }
  }
  
  # Read compoments until our file gets closed
  components <- list()
  while(is_open(fh)) {
    comp <- read_component(fh)
    components <- list_append(components, comp$name, comp$component)
  }
  
  # Make a gadgetfile object out of it
  return(gadgetfile(
    file_name = file_name,
    file_type = file_type,
    components = components))
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
  fileEncoding = "UTF-8",
  overwrite = NULL) {
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
  mfile <- read.gadget.file(path, mainfile, file_type = 'main', fileEncoding = fileEncoding, missingOkay = TRUE, recursive = FALSE)
  if (length(mfile) == 0) {
    mfile <- gadgetfile(mainfile, file_type = 'main', components = list(
      list(timefile = NA, areafile = NA, printfiles = structure(c(), comment = "Required comment")),
      stock = list(),
      tagging = list(),
      otherfood = list(),
      fleet = list(),
      likelihood = list()))
  } 
  
  if(is.null(overwrite)){
    overwrite <- FALSE
  } 
  
  # Do simple swaps first
  mfile[[1]]$timefile <- swap(mfile[[1]]$timefile, timefile, single = TRUE)
  mfile[[1]]$areafile <- swap(mfile[[1]]$areafile, areafile, single = TRUE)
  
  # Printfiles is mandatory, but can specify empty by adding a comment
  mfile[[1]]$printfiles <- swap(mfile[[1]]$printfiles, printfiles, single = overwrite)
  
  # Rest are in their own component
  mfile$stock$stockfiles <- swap(mfile$stock$stockfiles, stockfiles, single = overwrite)
  mfile$tagging$tagfiles <- swap(mfile$tagging$tagfiles, tagfiles, single = overwrite)
  mfile$otherfood$otherfoodfiles <- swap(mfile$otherfood$otherfoodfiles, otherfoodfiles, single = overwrite)
  mfile$fleet$fleetfiles <- swap(mfile$fleet$fleetfiles, fleetfiles, single = overwrite)
  mfile$likelihood$likelihoodfiles <- swap(mfile$likelihood$likelihoodfiles, likelihoodfiles, single = overwrite)
  
  # Write file back out again
  if (made_change) write.gadget.file(mfile, path, recursive = FALSE)
}
