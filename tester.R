library(TMB)
compile('test.cpp')
dyn.load(dynlib("test"))

obj <- MakeADFun(list(blu=1),list(recruitment=rep(1e6,10))
obj$hessian <- TRUE
opt <- do.call("optim",obj)
opt
opt$hessian ## <-- FD hessian from optim
obj$he() ## <-- Analytical hessian
sdreport(obj)
