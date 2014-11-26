library(TMB)
compile('gadgetLite.cpp')
dyn.load(dynlib("gadgetLite"))

#compile('gadgetLite.cpp',"-g -O0")
#dyn.load(dynlib("gadgetLite"))

if(FALSE){
source('stock.R')
source('gadgetClass.R')
source('function.R')
source('gadgetoptions.R')
source('gadgetMethods.R')
source('gadgetfunctions.R')
source('gadgetFileIO.R')
source('summaryFunc.R')
library(dplyr)


sim.dl <- 10

opt <- gadget.options('simple2stock')
opt$stocks$imm$n <- 1e6*exp(rnorm(20))
opt$stocks$imm$dl <- sim.dl
opt$stocks$mat$dl <- sim.dl
opt$stocks$imm$growth['binn'] <- 2
opt$stocks$mat$growth['binn'] <- 2

gm <- gadget.skeleton(time=opt$time,area=opt$area,stock=opt$stocks,opt$fleets)
sim <- gadget.simulate(gm)
simdat <- toDataFrame(sim)

SI <- acast(survey.index(subset(simdat$stocks,step==2),
                         split=(25*0:4)),
            year~SIgroup)
ldistSI <- acast(ldist(subset(simdat$fleets,fleet=='surv'&step==2)),#,dl=sim.dl),
                 lgroup~year,value.var='p')

ldistComm <- acast(ldist(subset(simdat$fleets,fleet=='comm')),#,dl=sim.dl),
                   lgroup~year~step,value.var='p')

aldistSI <- acast(aldist(subset(simdat$fleets,fleet=='surv'&step==2)),#,dl=sim.dl),
                 age~lgroup~year,value.var='p')

aldistComm <- acast(aldist(subset(simdat$fleets,fleet=='comm')),#,dl=sim.dl),
                   age~lgroup~year~step,value.var='p')
fleetCatches <- acast(ddply(simdat$fleets,~year+step,summarise,
                            catch=sum(num*10^(-5)*length^3)),
                      year~step)

data <- list(SI=t(SI),
             ldistSI=ldistSI,
             ldistComm=ldistComm,
             aldistSI=aldistSI,
             aldistComm=aldistComm,
             fleetCatches=fleetCatches,
             SIlgroups=1:4*ceiling(25/sim.dl),
             firstyear=1,
             lastyear=20,
             maxlgr=ceiling(15/sim.dl),
             minage=1,
             maxage=10,
             minlength=ceiling(5/sim.dl),
             maxlength=ceiling(90/sim.dl),
             M=rep(0.2,10),
             initSigma=c(2.2472, 2.8982, 4.0705, 4.9276,
               5.5404, 5.8072, 6.0233, 8, 9, 9),
             wa=c(10^(-5),3),
             compW=rep(1,5),
             dl=sim.dl)
             

parameters <- list(recruits=gm@stocks$imm@renewal.data$number*1e-5,
                   recl=9.897914,
                   recsd=2.2472,
                   initial=10*exp(-0.2*1:10),
                   aComm=-8.2,
                   bComm=0.22,
                   aSurv=-4.5,
                   bSurv=0.3,
                   k=log(0.09),
                   linf=115,
                   beta=log(200),
                   SIa=rep(0,4),
                   meanrec=mean(gm@stocks$imm@renewal.data$number*1e-5),
                   log_sigma = 1)

save(sim,simdat,parameters,data,file='runDat_v2.RData')
}
load('runDat_v2.RData')
map <- list(#recruits=factor(rep(NA,20)),
            k=factor(NA),
            initial=factor(rep(NA,10)),
            linf=factor(NA), bComm=factor(NA), bSurv=factor(NA))

obj <- MakeADFun(data,parameters)#,random='recruits',map=map)
obj$hessian <- TRUE
sim.dl <- 10
tmp <- obj$report()
dimnames(tmp$stkArr) <- list(age=1:10,length=1:(90/sim.dl),year=1:20,step=1:4)
dimnames(tmp$commArr) <- list(age=1:10,length=1:(90/sim.dl),year=1:20,step=1:4)
stkdat <- as.data.frame.table(tmp$stkArr,responseName='num')
commdat <- as.data.frame.table(tmp$commArr,responseName='num')
stkdat$length <- as.numeric(stkdat$length)*sim.dl

opt <- do.call("optim",obj)
opt <- nlminb(obj$par,obj$fn,obj$gr)
opt
opt$hessian ## <-- FD hessian from optim
obj$he() ## <-- Analytical hessian

