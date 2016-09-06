library(Rgadget)
library(unittest, quietly = TRUE)
source('utils/helpers.R')

ok_group("parse.gadget.formulae", {
    ok(cmp_str(
        parse.gadget.formulae("2"),
        " num 2"), "Constants will parse")

    ok(cmp_str(
        parse.gadget.formulae("(+ 2 (log (- #moo 1)))"),
        " language 2 + log(moo - 1)"), "Parsed deeply nested functions")

    ok(cmp_str(
        parse.gadget.formulae("(+ (2) (log (- (#moo) 1)))"),
        " language 2 + log(moo - 1)"), "Bracketed constants are fine")
})

ok_group("to.gadget.formulae", {
    ok(cmp(
        to.gadget.formulae(quote(2 + log(moo - 1))),
        "(+ 2 (log (- #moo 1)))"), "Can generate from quoted expressions")
    ok(cmp(
        to.gadget.formulae(parse.gadget.formulae("(+ (2) (log (- (#moo) 1)))")),
        "(+ 2 (log (- #moo 1)))"), "Can generate from the output of parse.gadget.formulae")

    ok(cmp(
        to.gadget.formulae(quote(4 + (2 - 8))),
        "(+ 4 (- 2 8))"), "We ignore R's explicit bracket function")
})

ok_group("sub.gadget.formulae", {
    ok(cmp_str(
        sub.gadget.formulae(quote(2 + log(moo)), list()),
        " language 2 + log(moo)"), "No substitutions")
    ok(cmp_str(
        sub.gadget.formulae(quote(2 + moo), list(frank = 3)),
        " language 2 + moo"), "No substitutions with correct name")
    ok(cmp_str(
        sub.gadget.formulae(quote(2 + log(moo) + oink), list(moo = 8, oink = quote(log(5)))),
        " language 2 + log(8) + log(5)"), "Can substitute values and strings")
    ok(cmp_str(
        sub.gadget.formulae(quote(2 + log(moo) + oink), list(moo = "(+ #baa #neigh)")),
        " language 2 + log(baa + neigh) + oink"), "Character strings are assumed to be gadget formulae")
    ok(cmp_str(
        sub.gadget.formulae(quote(2 + log(moo) + oink), list(moo = "(+ #baa #neigh)")),
        " language 2 + log(baa + neigh) + oink"), "Character strings are assumed to be gadget formulae")

    tv <- read.gadget.string(
        ver_string,
        "annualgrowth",
        "data",
        "; year  step    value",
        "1995    1       #grow1995",
        "1996    1       #grow1996",
        "1997    1       #grow1997",
        "1998    1       #grow1998",
        "1999    1       #grow1999",
        "2000    1       #grow2000",
        dir = path,
        file_type = "timevariable")
    ok(cmp_error(
        sub.gadget.formulae(quote(log(moo) + oink), list(moo = tv)),
        "Specify year"), "Need year before timevariables are useful")
    ok(cmp_error(
        sub.gadget.formulae(quote(log(moo) + oink), list(moo = tv), year = 1995),
        "Specify step"), "Need step before timevariables are useful")
    ok(cmp_error(
        sub.gadget.formulae(quote(log(moo) + oink), list(moo = tv), year = 1995, step = 99),
        "No value for moo"), "Outside timevariable range is an error")
    ok(cmp_str(
        sub.gadget.formulae(quote(log(moo) + oink), list(moo = tv), year = 1995, step = 1),
        " language log(grow1995) + oink"), "Replaced variable name")
})
