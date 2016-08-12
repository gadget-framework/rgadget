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
})
