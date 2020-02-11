library(Rgadget)
library(unittest, quietly = TRUE)
library(magrittr)

ok_group("von_b_formula", {
    ok(ut_cmp_identical(von_b_formula('argle'),
        "(* #Linf (- 1 (exp (* (* (- 1) (* 0.001 #k)) (- #argle (+ 1 (/ (log (- 1 (/ #recl #Linf))) (* 0.001 #k))))))))"), "Generated Vb with default variable names")
    ok(ut_cmp_identical(von_b_formula('argle', linf='elll', k='kay', recl='rrr'),
        "(* #elll (- 1 (exp (* (* (- 1) (* 0.001 #kay)) (- #argle (+ 1 (/ (log (- 1 (/ #rrr #elll))) (* 0.001 #kay))))))))"), "Generated Vb with custom variable names")
    ok(ut_cmp_identical(von_b_formula(c("a","b","c")), c(
        "(* #Linf (- 1 (exp (* (* (- 1) (* 0.001 #k)) (- #a (+ 1 (/ (log (- 1 (/ #recl #Linf))) (* 0.001 #k))))))))",
        "(* #Linf (- 1 (exp (* (* (- 1) (* 0.001 #k)) (- #b (+ 1 (/ (log (- 1 (/ #recl #Linf))) (* 0.001 #k))))))))",
        "(* #Linf (- 1 (exp (* (* (- 1) (* 0.001 #k)) (- #c (+ 1 (/ (log (- 1 (/ #recl #Linf))) (* 0.001 #k))))))))",
        NULL)), "Generated multiple formulae per a")
    ok(ut_cmp_identical(von_b_formula(10),
                        "(* #Linf (- 1 (exp (* (* (- 1) (* 0.001 #k)) (- 10 (+ 1 (/ (log (- 1 (/ #recl #Linf))) (* 0.001 #k))))))))"), 
       "Generated Vb with default variable names and age as numeric")
    
})
