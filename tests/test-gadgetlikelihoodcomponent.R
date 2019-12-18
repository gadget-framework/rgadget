library(Rgadget)
library(unittest, quietly = TRUE)
source('tests/utils/helpers.R')

all_components <- c(
        "penalty",
        "understocking",
        "catchdistribution",
        "catchstatistics",
        "stockdistribution",
        "surveydistribution",
        "stomachcontent",
        "migrationpenalty",
        "catchinkilos",
        NULL)

ok_group("Can generate gadgetlikelihoodcomponent objects", {
    expect_error(
        gadgetlikelihoodcomponent("aardvark"),
        "aardvark")

    expect_error(
        gadgetlikelihoodcomponent("e12 ; bobby tables"),
        "e12 ; bobby tables")

    expect_equal(
        class(gadgetlikelihoodcomponent("penalty")),
        c("gadget_penalty_component", "gadgetlikelihoodcomponent"))
})

ok_group("Can use as.character on gadgetlikelihoodcomponents", {
    comp <- gadgetlikelihoodcomponent("penalty", name = "wibble", weight = 0.5)
    expect_equal(comp$name, "wibble")
    expect_equal(comp$weight, 0.5)
    expect_equal(comp$type, "penalty")
})

ok_group("Can nest data.frame in a list", {
    df <- data.frame(year = 1, step = 1, area = 1, fleet = 1, total_weight = 1)

    comp <- gadgetlikelihoodcomponent("catchinkilos", data = df)
    ok(ut_cmp_identical(comp$datafile$data, df), "Data included with regular data.frame")

    comp <- gadgetlikelihoodcomponent("catchinkilos", data = list(df))
    ok(ut_cmp_identical(comp$datafile$data, df), "Data included with nested data.frame")

    ok(ut_cmp_error(
        gadgetlikelihoodcomponent("catchinkilos", data = list(df, df)),
        "list"), "More than one item causes an error")
})

for (type in all_components) {
    if (type == "catchstatistics") {
        default_opts <- list(type,
            data = data.frame(), data_function = "customfunction")
    } else if (type == "catchdistribution") {
        default_opts <- list(type,
            data = data.frame(year = 1, step = 1, area = 1, age = 1, length = 1, number = 1))
    } else if (type == "stockdistribution") {
        default_opts <- list(type,
            data = data.frame(year = 1, step = 1, area = 1, aardvark = 1, age = 1, length = 1, number = 1))
    } else if (type == "surveydistribution") {
        default_opts <- list(type,
            data = data.frame(year = 1, step = 1, area = 1, age = 1, length = 1, number = 1))
    } else if (type == "stomachcontent") {
        default_opts <- list(type,
            data = data.frame(year = 1, step = 1, area = 1, predator_length = 'pred100', prey_length = 'prey10', ratio = 1),
            predator_length = list('pred100' = 'pred100'),
            prey_length = list('prey10' = 'prey10'),
            prey_labels = list('prey10' = 'Prey 10'))
    } else if (type == "catchinkilos") {
        default_opts <- list(type,
            data = data.frame(year = 1, step = 1, area = 1, fleet = 1, total_weight = 1))
    } else {
        default_opts <- list(type)
    }

    ok_group(paste("Name, type and weight for component", type), {
        comp <- do.call(gadgetlikelihoodcomponent, default_opts)

        # Class should match type
        ok(ut_cmp_identical(class(comp), c(
            paste0("gadget_", type, "_component"),
            "gadgetlikelihoodcomponent")), "Has correct class")

        # Type and name should match the name we gave
        ok(ut_cmp_identical(comp$type, type), "Type set")
        ok(ut_cmp_identical(comp$name, type), "Default name same as type")
        ok(ut_cmp_identical(comp$weight, 0), "Weight defaults to 0")

        # Can customise name & weight
        comp <- do.call(gadgetlikelihoodcomponent, c(
            default_opts,
            list(name = "gerald", weight = 0.542),
            NULL))
        ok(ut_cmp_identical(comp$name, "gerald"), "Can set name")
        ok(ut_cmp_identical(comp$weight, 0.542), "Can set weight")
    })

    ok_group(paste("Filenames for component", type), {
        contains <- function(pattern, x) {
            if(length(grep(pattern, x, value = FALSE)) > 0) {
                TRUE
            } else {
                 paste0(x, " does not contain ", pattern)
            }
        }

        comp <- do.call(gadgetlikelihoodcomponent, c(
            default_opts,
            list(name = "gerald", weight = 0.542),
            NULL))
        for (key in names(comp)) {
            if ("gadget_file" %in% class(comp[[key]]) && comp$type != "penalty") {
                ok(contains(
                    paste0("^Data/", comp$type, "\\.", comp$name, "\\."),
                    comp$datafile$filename), "Filename starts with name and type")
            }
        }
    })
}

# Adapt old code -> new
gadget_directory <- mfdb::gadget_directory
gadget_dir_write <- function (gd, comp) {
    gadgetlikelihood('likelihood', gd$dir, missingOkay = TRUE) %>%
        gadget_update(comp) %>%
        write.gadget.file(gd$dir)
}
gadget_dir_read <- mfdb::gadget_dir_read
cmp_file <- function (gd, filename, ...) {
    f <- file(file.path(gd$dir, filename))
    lines <- readLines(f, n = -1)
    close(f)
    ut_cmp_identical(lines, c(...))
}
ver_string <- paste("; Generated by Rgadget", packageVersion("Rgadget"))

ok_group("Can write likelihood components", {
    # Create a temporary directory, starts off empty
    dir <- tempfile()
    gd <- gadget_directory(dir)

    # Create some components
    gadget_dir_write(gd, gadgetlikelihoodcomponent("understocking", name="head-bone", weight = 0.3))
    expect_equal(list.files(dir), c("likelihood", "main"))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = structure(list(name = "head-bone", weight = 0.3, type = "understocking"), preamble = list(gsub('^; ', '', ver_string), ""))))
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        "likelihoodfiles\tlikelihood"))

    gadget_dir_write(gd, gadgetlikelihoodcomponent("penalty", name="neck-bone", weight = 0.5))
    ok(ut_cmp_identical(
        list.files(dir, recursive = TRUE),
        c("Data/neck-bone.penaltyfile", "likelihood", "main")), "Created penaltyfile")
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = structure(list(name = "head-bone", weight = 0.3, type = "understocking"), preamble = list(gsub('^; ', '', ver_string), "")),
            component = structure(list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "Data/neck-bone.penaltyfile"), preamble = list(""))))
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        "likelihoodfiles\tlikelihood"))

    # Override one, add another
    gadget_dir_write(gd, gadgetlikelihoodcomponent("understocking", name="head-bone", weight = 0.8))
    gadget_dir_write(gd, gadgetlikelihoodcomponent("understocking", name="shoulder-bone", weight = 0.2))
    expect_equal(
        gadget_dir_read(gd, "likelihood")$components,
        list(
            list(),
            component = structure(list(name = "head-bone", weight = 0.8, type = "understocking"), preamble = list(gsub('^; ', '', ver_string), "")),
            component = structure(list(name = "neck-bone", weight = 0.5, type = "penalty", datafile = "Data/neck-bone.penaltyfile"), preamble = list("")),
            component = structure(list(name = "shoulder-bone", weight = 0.2, type = "understocking"), preamble = list(""))))
    ok(cmp_file(gd, "main",
        ver_string,
        "timefile\t",
        "areafile\t",
        "printfiles\t; Required comment",
        "[stock]",
        "[tagging]",
        "[otherfood]",
        "[fleet]",
        "[likelihood]",
        "likelihoodfiles\tlikelihood"))
})

###############################################################################
ok_group("Can generate an understocking component with default parameters", {
    comp <- gadgetlikelihoodcomponent("understocking")
    expect_equal(comp$name, "understocking")
    expect_equal(comp$weight, 0)
    expect_equal(comp$type, "understocking")
})

ok_group("Can customise it", {
    comp <- gadgetlikelihoodcomponent("understocking", name = "alfred", weight = 0.3)
    expect_equal(comp$name, "alfred")
    expect_equal(comp$weight, 0.3)
    expect_equal(comp$type, "understocking")
})

###############################################################################
ok_group("Function either provided explicitly or based on generator", {
    expect_error(
        gadgetlikelihoodcomponent("catchstatistics"),
        "No data provided")
    expect_error(
        gadgetlikelihoodcomponent("catchstatistics", data = data.frame()),
        "Cannot work out the required function, and data_function not provided")
    expect_error(
        gadgetlikelihoodcomponent("catchstatistics", data = structure(data.frame(), generator = "camel")),
        "Unknown generator function camel")

    expect_equal(
        gadgetlikelihoodcomponent("catchstatistics",
            data = data.frame(), data_function = "customfunction")[['function']],
        "customfunction")

    ok(ut_cmp_identical(
        gadgetlikelihoodcomponent("catchstatistics", data = structure(
            data.frame(year = 1990, step = 1, area = 1, age = 1, number = 1, mean = 1, stddev = 1),
            generator = "mfdb_sample_meanlength_stddev"))[['function']],
        "lengthgivenstddev"), "Guessed function given generator")

    ok(ut_cmp_error(
        gadgetlikelihoodcomponent("catchstatistics", data = structure(
            data.frame(),
            generator = "mfdb_sample_meanlength_stddev")),
        "data given to gadget_catchstatistics_component is empty"), "Noticed missing data")

    ok(ut_cmp_error(
        gadgetlikelihoodcomponent("catchstatistics", data = structure(
            data.frame(year = 1990, step = 1, area = 1, age = 1, number = 1, mean = 1),
            generator = "mfdb_sample_meanlength_stddev")),
        "stddev"), "Noticed missing column")
})

###############################################################################
ok_group("Aggregation files", {
    cmp_agg <- function (agg_type, agg, ...) {
        gd <- gadget_directory(tempfile())
        agg_summ <- agg_summary(fake_mdb(), agg, 'c.col', 'col', data.frame(col = 1:5), 0)
        gadget_dir_write(gd, gadgetlikelihoodcomponent(
            "catchdistribution",
            name="cd",
            weight = 0.8,
            data = structure(
                data.frame(year = 1, step = 1, area = 1, age = 1, length = 1, number = 1),
                area = if (agg_type == 'area') agg_summ else NULL,
                age = if (agg_type == 'age') agg_summ else NULL,
                length = if (agg_type == 'len') agg_summ else NULL,
                generator = "mfdb_sample_meanlength")
            ))
        do.call(cmp_file, c(
            list(gd, paste0("Aggfiles/catchdistribution.cd.", agg_type, ".agg")),
            list(...)))
    }

    ok(cmp_agg('area', list(divA = c('x', 't'), divB = c('s', 'r')),
        ver_string,
        "divA\t1",
        "divB\t2",
        NULL), "Area aggregation file has subdivisions hidden")

    ok(cmp_agg('age', list(young = 1:4, old = 5:8),
        ver_string,
        "young\t1\t2\t3\t4",
        "old\t5\t6\t7\t8",
        NULL), "Age aggregation matches input")

    ok(cmp_agg('age', 1:4,
        ver_string,
        "1\t1",
        "2\t2",
        "3\t3",
        "4\t4",
        NULL), "1:4 converted into 1 group for each")

    ok(cmp_agg('age', mfdb::mfdb_unaggregated(),
        ver_string,
        "1\t1",
        "2\t2",
        "3\t3",
        "4\t4",
        "5\t5",
        NULL), "mfdb_unaggregated")

    ok(cmp_agg('len', mfdb::mfdb_interval("len", seq(0, 50, by = 10)),
        ver_string,
        "len0\t0\t10",
        "len10\t10\t20",
        "len20\t20\t30",
        "len30\t30\t40",
        "len40\t40\t50",
        NULL), "mfdb_interval (with len, so min/max)")

    ok(cmp_agg('len', mfdb::mfdb_step_interval("len", to = 30, by = 5),
        ver_string,
        "len0\t0\t5",
        "len5\t5\t10",
        "len10\t10\t15",
        "len15\t15\t20",
        "len20\t20\t25",
        "len25\t25\t30",
        NULL), "mfdb_step_interval (with len, so min/max)")

    ok(cmp_agg('age', mfdb::mfdb_interval("age", seq(0, 20, by = 10)),
        ver_string,
        "age0\t0\t1\t2\t3\t4\t5\t6\t7\t8\t9",
        "age10\t10\t11\t12\t13\t14\t15\t16\t17\t18\t19",
        NULL), "mfdb_interval (with age, so discrete numbers)")

    ok(cmp_agg('age', mfdb::mfdb_step_interval("age", to = 30, by = 5),
        ver_string,
        "age0\t0\t1\t2\t3\t4",
        "age5\t5\t6\t7\t8\t9",
        "age10\t10\t11\t12\t13\t14",
        "age15\t15\t16\t17\t18\t19",
        "age20\t20\t21\t22\t23\t24",
        "age25\t25\t26\t27\t28\t29",
        NULL), "mfdb_step_interval (with age, so discrete numbers)")

    ok(cmp_agg('age', mfdb::mfdb_step_interval("age", to = 5, by = 1),
        ver_string,
        "age0\t0",
        "age1\t1",
        "age2\t2",
        "age3\t3",
        "age4\t4",
        NULL), "mfdb_step_interval (with age, so discrete numbers)")

})

###############################################################################
ok_group("surveyindices", {
    cmp_component <- function (comp, ...) {
        gd <- gadget_directory(tempfile())
        gadget_dir_write(gd, comp)
        cmp_file(gd, "likelihood", ...)
    }

    component <- gadgetlikelihoodcomponent('surveyindices', name = 'si', sitype = 'lengths', fittype = 'linearfit',
        data = structure(
            data.frame(year = 1998, step = 1:2, area = 101, length = c(100,200), number = c(2,4)),
            area = list(all = 101),
            length = list(len100 = c(100,500))))
    ok(ut_cmp_identical(class(component)[[1]], 'gadget_surveyindices_component'), "Made lengths sitype")
    ok(cmp_component(component,
        ver_string,
        "; ",
        "[component]",
        "name\tsi",
        "weight\t0",
        "type\tsurveyindices",
        "datafile\tData/surveyindices.si.lengths",
        "sitype\tlengths",
        "biomass\t0",
        "areaaggfile\tAggfiles/surveyindices.si.area.agg",
        "lenaggfile\tAggfiles/surveyindices.si.len.agg",
        "fittype\tlinearfit",
        NULL), "Wrote component with lengths sitype")

    component <- gadgetlikelihoodcomponent('surveyindices', name = 'si', sitype = 'ages', fittype = 'linearfit',
        data = structure(
            data.frame(year = 1998, step = 1:2, area = 101, age = c(100,200), number = c(2,4)),
            area = list(all = 101),
            age = list(age100 = c(100,500))))
    ok(ut_cmp_identical(class(component)[[1]], 'gadget_surveyindices_component'), "Made ages sitype")
    ok(cmp_component(component,
        ver_string,
        "; ",
        "[component]",
        "name\tsi",
        "weight\t0",
        "type\tsurveyindices",
        "datafile\tData/surveyindices.si.ages",
        "sitype\tages",
        "biomass\t0",
        "areaaggfile\tAggfiles/surveyindices.si.area.agg",
        "ageaggfile\tAggfiles/surveyindices.si.age.agg",
        "fittype\tlinearfit",
        NULL), "Wrote component with ages sitype")

    component <- gadgetlikelihoodcomponent('surveyindices', name = 'si', sitype = 'fleets', fittype = 'linearfit',
        data = structure(
            data.frame(year = 1998, step = 1:2, area = 101, length = c(100,200), number = c(2,4)),
            area = list(all = 101),
            length = list(len100 = c(100,500))),
        fleetnames = c("cuthbert", "dibble"))
    ok(ut_cmp_identical(class(component)[[1]], 'gadget_surveyindices_component'), "Made fleets sitype")
    ok(cmp_component(component,
        ver_string,
        "; ",
        "[component]",
        "name\tsi",
        "weight\t0",
        "type\tsurveyindices",
        "datafile\tData/surveyindices.si.fleets",
        "sitype\tfleets",
        "biomass\t0",
        "areaaggfile\tAggfiles/surveyindices.si.area.agg",
        "lenaggfile\tAggfiles/surveyindices.si.len.agg",
        "fleetnames\tcuthbert\tdibble",
        "fittype\tlinearfit",
        NULL), "Wrote component with fleets sitype")

    component <- gadgetlikelihoodcomponent('surveyindices', name = 'si', sitype = 'acoustic', fittype = 'linearfit',
        data = structure(
            data.frame(year = 1998, step = 1:2, area = 101, survey = c(100,200), acoustic = c(2,4)),
            area = list(all = 101)),
        surveynames = c("cuthbert", "dibble"))
    ok(ut_cmp_identical(class(component)[[1]], 'gadget_surveyindices_component'), "Made acoustic sitype")
    ok(cmp_component(component,
        ver_string,
        "; ",
        "[component]",
        "name\tsi",
        "weight\t0",
        "type\tsurveyindices",
        "datafile\tData/surveyindices.si.acoustic",
        "sitype\tacoustic",
        "biomass\t0",
        "areaaggfile\tAggfiles/surveyindices.si.area.agg",
        "surveynames\tcuthbert\tdibble",
        "fittype\tlinearfit",
        NULL), "Wrote component with acoustic sitype")

    component <- gadgetlikelihoodcomponent('surveyindices', name = 'si', sitype = 'effort', fittype = 'linearfit',
        data = structure(
            data.frame(year = 1998, step = 1:2, area = 101, fleet = c(100,200), effort = c(2,4)),
            area = list(all = 101)),
        fleetnames = c("cuthbert", "dibble"))
    ok(ut_cmp_identical(class(component)[[1]], 'gadget_surveyindices_component'), "Made effort sitype")
    ok(cmp_component(component,
        ver_string,
        "; ",
        "[component]",
        "name\tsi",
        "weight\t0",
        "type\tsurveyindices",
        "datafile\tData/surveyindices.si.effort",
        "sitype\teffort",
        "biomass\t0",
        "areaaggfile\tAggfiles/surveyindices.si.area.agg",
        "fleetnames\tcuthbert\tdibble",
        "fittype\tlinearfit",
        NULL), "Wrote component with effort sitype")

})

###############################################################################
ok_group("surveydistribution", {
    component <- gadgetlikelihoodcomponent('surveydistribution', name = 'sd',
        data = structure(
            data.frame(year = 1998, step = 1:2, area = 101, age = c(1,2), length = c(3,4), number = c(100,200)),
            area = list(all = 101),
            age = list(all = 1:20),
            length = list(all = 1:10)),
        stocknames = c('frank', 'barbara'),
        fittype = 'linearfit',
        parameters = c(2, 4),
        suitability = "function woo",
        epsilon = 10,
        likelihoodtype = 'multinomial')
    ok(ut_cmp_identical(class(component)[[1]], 'gadget_surveydistribution_component'), "Made effort sitype")
    ok(cmp_component(component,
        ver_string,
        "; ",
        "[component]",
        "name\tsd",
        "weight\t0",
        "type\tsurveydistribution",
        "datafile\tData/surveydistribution.sd.",
        "areaaggfile\tAggfiles/surveydistribution.sd.area.agg",
        "lenaggfile\tAggfiles/surveydistribution.sd.len.agg",
        "ageaggfile\tAggfiles/surveydistribution.sd.age.agg",
        "stocknames\tfrank\tbarbara",
        "fittype\tlinearfit",
        "parameters\t2\t4",
        "\tfunction woo",  # TODO: Where did this extra tab come from?
        "epsilon\t10",
        "likelihoodtype\tmultinomial",
        NULL), "Wrote component with effort sitype")
})

###############################################################################
ok_group("stomachcontent - matching of prey to labels", {
    # Create a temporary directory, starts off empty
    dir <- tempfile()
    gd <- gadget_directory(dir)
    expect_equal(list.files(dir), character(0))

    sample_data <- data.frame(year = 1, step = 1, area = 1:3, predator_length = 'pred100',
        prey_length = c('cod', 'anacoda', 'cod.imm'),
        ratio = 1)

    # Default to get assigned eveywhere
    gadget_dir_write(gd, gadgetlikelihoodcomponent(
        'stomachcontent',
        data = sample_data,
        area = list(all = 101),
        predator_length = list(len100 = c(100,500)),
        prey_length = list(cod = 10:20, anacoda = 1:5, "cod.imm" = 1:10),
        prey_labels = 'Prey 10'))
    ok(cmp_file(gd, gadget_dir_read(gd, "likelihood")$components[[2]]$preyaggfile,
        ver_string,
        "; ",
        "cod\t",
        "Prey 10\t",
        "lengths\t10\t20",
        "digestioncoefficients\t",
        "; ",
        "anacoda\t",
        "Prey 10\t",
        "lengths\t1\t5",
        "digestioncoefficients\t",
        "; ",
        "cod.imm\t",
        "Prey 10\t",
        "lengths\t1\t10",
        "digestioncoefficients\t",
        NULL))

    # The first match in the list wins, we match from the start
    gadget_dir_write(gd, gadget_stomachcontent_component(
        data = sample_data,
        area = list(all = 101),
        predator_length = list(len100 = c(100,500)),
        prey_length = list(cod = 10:20, anacoda = 1:5, "cod.imm" = 1:10),
        prey_labels = list("cod" = "all.cod", "cod.imm" = "immature.cod", "other"),
        prey_digestion_coefficients = list("cod.imm" = c(1,1,1), "cod" = c(3,3,3), c(9,9,9))))
    ok(cmp_file(gd, gadget_dir_read(gd, "likelihood")$components[[2]]$preyaggfile,
        ver_string,
        "; ",
        "cod\t",
        "all.cod\t",
        "lengths\t10\t20",
        "digestioncoefficients\t3\t3\t3",
        "; ",
        "anacoda\t",
        "other\t",
        "lengths\t1\t5",
        "digestioncoefficients\t9\t9\t9",
        "; ",
        "cod.imm\t",
        "all.cod\t",
        "lengths\t1\t10",
        "digestioncoefficients\t1\t1\t1",
        NULL))
    gadget_dir_write(gd, gadget_stomachcontent_component(
        data = sample_data,
        area = list(all = 101),
        predator_length = list(len100 = c(100,500)),
        prey_length = list(cod = 10:20, anacoda = 1:5, "cod.imm" = 1:10),
        prey_labels = list("cod.imm" = "immature.cod", "cod" = "all.cod", "other"),
        prey_digestion_coefficients = list("cod" = c(3,3,3), "cod.imm" = c(1,1,1), c(9,9,9))))
    ok(cmp_file(gd, gadget_dir_read(gd, "likelihood")$components[[2]]$preyaggfile,
        ver_string,
        "; ",
        "cod\t",
        "all.cod\t",
        "lengths\t10\t20",
        "digestioncoefficients\t3\t3\t3",
        "; ",
        "anacoda\t",
        "other\t",
        "lengths\t1\t5",
        "digestioncoefficients\t9\t9\t9",
        "; ",
        "cod.imm\t",
        "immature.cod\t",
        "lengths\t1\t10",
        "digestioncoefficients\t3\t3\t3",
        NULL))
})

###############################################################################
ok_group("catchinkilos", {
    # aggregationlevel = 0
    comp <- gadgetlikelihoodcomponent(
        'catchinkilos',
        data = data.frame(year = 1, step = 1:3, area = 1, fleet = 1, total_weight = 11:13))
    ok(ut_cmp_identical(names(comp$datafile$data), c("year", "step", "area", "fleet", "total_weight")), "Has step column")

    # aggregationlevel = 1
    comp <- gadgetlikelihoodcomponent(
        'catchinkilos',
        data = structure(data.frame(year = 1, step = 1, area = 1, fleet = 1, total_weight = 11:13),
            step = mfdb::mfdb_timestep_yearly))
    ok(ut_cmp_identical(names(comp$datafile$data), c("year", "area", "fleet", "total_weight")), "step column removed")
})
