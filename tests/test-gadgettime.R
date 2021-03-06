library(Rgadget)
library(unittest, quietly = TRUE)
library(magrittr)

ver_string <- paste("; Generated by Rgadget", utils::packageVersion("Rgadget"))

cmp_dir <- function(path, expected) {
    # Read all files in a directory
    dir_list <- function (dir) {
        files <- sort(list.files(dir, full.names = FALSE, recursive = TRUE), method = 'radix')
        structure(
            lapply(files, function (f) readLines(file.path(dir, f), n = -1)),
            names = files
        )
    }

    dir_list_filter <- function (x) {
        invisible(lapply(names(x), function (n) { writeLines(paste0("\n=== ", n, " ===")); writeLines(x[[n]]) }))
    }

    ut_cmp_identical(dir_list(path), expected, filter = dir_list_filter)
}

ok_group("time", {
    path <- tempfile()

    gadgettime('timef', path, missingOkay = TRUE) %>%
        gadget_update(
            firstyear = 1990,
            lastyear = 2000, 
            notimesteps = c(4,3,3,3,3)) %>%
        write.gadget.file(path)

    ok(cmp_dir(path, list(
        main = c(
            "; Generated by Rgadget 0.5",
            "timefile\ttimef",
            "areafile\t",
            "printfiles\t; Required comment",
            "[stock]",
            "[tagging]",
            "[otherfood]",
            "[fleet]",
            "[likelihood]",
            NULL),
        timef = c(
            "; Generated by Rgadget 0.5",
            "firstyear\t1990",
            "lastyear\t2000",
            "notimesteps\t4\t3\t3\t3\t3",
            NULL)
    )), "Created time file")
})

