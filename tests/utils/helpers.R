cmp <- function(a, b) {
    cmp_test <- all.equal(a,b, tolerance = 1e-6)
    if(identical(cmp_test, TRUE)) return(TRUE)

    if (file.exists(Sys.which('git'))) {
        totmp <- function(x) {
            f <- tempfile(pattern = "str.")
            capture.output(str(x,
                vec.len = 1000,
                digits.d = 5,
                nchar.max = 1000), file = f)
            return(f)
        }

        diff_output <- suppressWarnings(system2(
            Sys.which('git'),
            c("diff", "--no-index", "--color-words", totmp(a), totmp(b)),
            input = "",
            stdout = TRUE, stderr = TRUE))
        if (length(diff_output) > 0) return(diff_output)
    }

    return(c(
        capture.output(str(a)),
        "... does not equal...",
        capture.output(str(b)),
        cmp_test
    ))
}
cmp_error <- function(exp, expected_regexp) {
    msg <- tryCatch({exp ; "No error returned"}, error = function(e) e$message)
    if(grepl(expected_regexp, msg)) TRUE else paste0("'", msg, "' should contain '", expected_regexp, "'")
}
cmp_str <- function(exp, expected_str) {
    cmp(capture.output(str(exp)), expected_str)
}

# Read all files in a directory
dir_list <- function (dir) {
    files <- list.files(dir, full.names = FALSE, recursive = TRUE)
    structure(
        lapply(files, function (f) readLines(file.path(dir, f), n = -1)),
        names = files
    )
}
