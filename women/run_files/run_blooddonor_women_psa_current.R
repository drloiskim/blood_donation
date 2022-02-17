library(readstata13); library(MASS); library(flexsurv); library(simsurv)

overwrite <- "TRUE"
setwd("c:/")
source("./blooddonor_des.R")



psa<-"TRUE"
source("./women/input/blooddonor_params_women_current.R")

###############
#### WOMEN ####
###############

## Read-in covariate distribution
covs<-read.dta13("./women/input/covs_women.dta")
agedist<-covs$age ## baseline age category from COMPARE
exagedist<-covs$exactage ## baseline exact age from COMPARE
hbdist<-covs$hb ## baseline hb from COMPARE
blooddist<-covs$blood # baseline blood group from COMPARE
ethdist<-covs$eth # baseline ethnicity (white v non-white) from COMPARE
prevdefdist<-covs$prevdef # baseline previous low hb deferrals (past 2y)
prevdondist<-covs$prevdon
blupdist<-covs$blup
blupsedist<-covs$blupse
fixedse0dist<-covs$fixedse0
fixedse1dist<-covs$fixedse1
fixedse2dist<-covs$fixedse2
fixedse3dist<-covs$fixedse3
fixedse4dist<-covs$fixedse4
fixedse5dist<-covs$fixedse5
fixedse6dist<-covs$fixedse6
fixedse7dist<-covs$fixedse7
fixedse8dist<-covs$fixedse8
fixedse9dist<-covs$fixedse9
fixedse10dist<-covs$fixedse10
fixedse11dist<-covs$fixedse11
fixedse12dist<-covs$fixedse12
fixedse13dist<-covs$fixedse13
fixedse14dist<-covs$fixedse14
fixedse15dist<-covs$fixedse15
fixedse16dist<-covs$fixedse16
fixedse17dist<-covs$fixedse17
fixedse18dist<-covs$fixedse18
fixedse19dist<-covs$fixedse19
fixedse20dist<-covs$fixedse20
fixedse21dist<-covs$fixedse21
fixedse22dist<-covs$fixedse22
fixedse23dist<-covs$fixedse23
fixedse24dist<-covs$fixedse24
fixedse25dist<-covs$fixedse25
fixedse26dist<-covs$fixedse26
fixedse27dist<-covs$fixedse27
fixedse28dist<-covs$fixedse28
fixedse29dist<-covs$fixedse29
fixedse30dist<-covs$fixedse30
fixedse31dist<-covs$fixedse31
fixedse32dist<-covs$fixedse32
fixedse33dist<-covs$fixedse33
fixedse34dist<-covs$fixedse34
fixedse35dist<-covs$fixedse35
fixedse36dist<-covs$fixedse36
fixedse37dist<-covs$fixedse37
fixedse38dist<-covs$fixedse38
fixedse39dist<-covs$fixedse39
fixedse40dist<-covs$fixedse40
fixedse41dist<-covs$fixedse41
fixedse42dist<-covs$fixedse42
fixedse43dist<-covs$fixedse43
fixedse44dist<-covs$fixedse44
fixedse45dist<-covs$fixedse45
fixedse46dist<-covs$fixedse46
fixedse47dist<-covs$fixedse47
fixedse48dist<-covs$fixedse48
fixedse49dist<-covs$fixedse49
fixedse50dist<-covs$fixedse50
fixedse51dist<-covs$fixedse51
fixedse52dist<-covs$fixedse52
blups<-read.dta13("./women/input/blups_women.dta")

## fitting flexible parametric model to observed data; used to inform simulated time to return 
## importing stata data
stpmwomen<-read.dta13("./women/input/stpm_women.dta")

# Define a function returning the log cum hazard at time t: used to inform simulated time to return
logcumhaz <- function(t, x, betas, knots) {
  
  basis <- flexsurv::basis(knots, log(t))
  res <- 
    betas[["gamma0"]] * basis[[1]] + 
    betas[["gamma1"]] * basis[[2]] +
    betas[["gamma2"]] * basis[[3]] +
    betas[["gamma3"]] * basis[[4]] +
    betas[["eth1"]] * x[["eth1"]] + 
    betas[["age1"]] * x[["age1"]] + 
    betas[["age2"]] * x[["age2"]] + 
    betas[["age3"]] * x[["age3"]] + 
    betas[["age4"]] * x[["age4"]] + 
    betas[["age5"]] * x[["age5"]] + 
    betas[["blood2"]] * x[["blood2"]] + 
    betas[["blood3"]] * x[["blood3"]] + 
    betas[["blood4"]] * x[["blood4"]] + 
    betas[["blood5"]] * x[["blood5"]] + 
    betas[["blood6"]] * x[["blood6"]] + 
    betas[["blood7"]] * x[["blood7"]] + 
    betas[["blood8"]] * x[["blood8"]] + 
    betas[["nlow2"]] * x[["nlow2"]] + 
    betas[["nlow3"]] * x[["nlow3"]] + 
    betas[["ndon2"]] * x[["ndon2"]] + 
    betas[["ndon3"]] * x[["ndon3"]] 
  
  res
}
# gamma0 = cons
# gamma1 = shape
# gamma2, etc = additional cubic spline params

fpmod <- flexsurv::flexsurvspline(Surv(newdiffv_hours, return) ~ eth1+
                                       age1+age2+age3+age4+age5+
                                       blood2+blood3+blood4+blood5+blood6+blood7+blood8+
                                       nlow2+nlow3+
                                       ndon2+ndon3, 
                                     data = stpmwomen, k = 2)


## MODEL

###########################################################################################################

## STRATEGY PARAMS:
## current: indicator for implementing current strategy (1=yes, 0=no)
## optimal: indicator for implementing alternative strategy
## atrisk: indicator for strategy with on-site testing for those with medium certainty of being over the threshold to donate
## indemand: indicator for strategy preferentially recalling donors with in-demand blood groups
## atrisk.thresh: threshold for those with on-site testing (only used if atrisk=1)
## indemand.thresh: threshold for recalling donors with in-demand blood groups (only used if indemand=1)
## lowhbdef.recall: wait time after low hb deferral
## vlowhbdef.recall: wait time after very low hb deferral
## othdef.recall: wait time after other deferral
## prob.thresh: threshold of probability over threshold to donate used for recalling 
## min.recall: earliest permitted recall time
## curr.recall: current earliest permitted recall time (only used if current=1)
## thresh: hb threshold for donation
## hb.min: min hb in observed data (modelled data rounded to this value if lower, for purpose of drawing BLUPs)
## hb.max: max hb in observed data (modelled data rounded to this value if higher, for purpose of drawing BLUPs)




## CURRENT STRATEGY 
## NOTE: with 16 week minimum recall
set.seed(9049)
params=list("hb"=c(visit.coef=-0.5723, rettime.coef=0.0118, age1.coef=0.0674, age2.coef=-0.0546, age3.coef=0.1116, age4.coef=0.1888, age5.coef=0.1733, eth.coef=-0.4193, blood2.coef=0.0098, blood3.coef=0.0963, blood4.coef=0.0879, blood5.coef=0.0776, blood6.coef=0.0182, blood7.coef=0.0732, blood8.coef=0.2769, cons=13.3709), ## hb recovery
            "dropout"=c(cons=-2.2176),
            "defer"=c(cons=-0.5287, hb1.coef=0, hb2.coef=0, hb3.coef=0),
            "othdefer"=c(cons=-2.8513), 
            "strategy"=c(current=1, optimal=0, atrisk=0, indemand=0, atrisk.thresh=0.9, indemand.thresh=0.7, lowhbdef.recall=12, vlowhbdef.recall=52, othdef.recall=4, prob.thresh=0.9, min.recall=16, curr.recall=16, length=52, thresh=12.5, hb.min=8.7, hb.max=20.8) # indicators for recall strategy type
)



if(psa){
  set.seed(541)
  psa.hb<-psa.des(B=5, N=1000, params.b=psa.b.params, params.v=psa.v.params, agedist=agedist, exagedist=exagedist, hbdist=hbdist, ethdist=ethdist, blooddist=blooddist, prevdefdist=prevdefdist, prevdondist=prevdondist, blupdist=blupdist, blupsedist=blupsedist, recall=recall.curr, attend=attend.dist,
                  fixedse0dist=fixedse0dist,
                  fixedse1dist=fixedse1dist,
                  fixedse2dist=fixedse2dist,
                  fixedse3dist=fixedse3dist,
                  fixedse4dist=fixedse4dist,
                  fixedse5dist=fixedse5dist,
                  fixedse6dist=fixedse6dist,
                  fixedse7dist=fixedse7dist,
                  fixedse8dist=fixedse8dist,
                  fixedse9dist=fixedse9dist,
                  fixedse10dist=fixedse10dist,
                  fixedse11dist=fixedse11dist,
                  fixedse12dist=fixedse12dist,
                  fixedse13dist=fixedse13dist,
                  fixedse14dist=fixedse14dist,
                  fixedse15dist=fixedse15dist,
                  fixedse16dist=fixedse16dist,
                  fixedse17dist=fixedse17dist,
                  fixedse18dist=fixedse18dist,
                  fixedse19dist=fixedse19dist,
                  fixedse20dist=fixedse20dist,
                  fixedse21dist=fixedse21dist,
                  fixedse22dist=fixedse22dist,
                  fixedse23dist=fixedse23dist,
                  fixedse24dist=fixedse24dist,
                  fixedse25dist=fixedse25dist,
                  fixedse26dist=fixedse26dist,
                  fixedse27dist=fixedse27dist,
                  fixedse28dist=fixedse28dist,
                  fixedse29dist=fixedse29dist,
                  fixedse30dist=fixedse30dist,
                  fixedse31dist=fixedse31dist,
                  fixedse32dist=fixedse32dist,
                  fixedse33dist=fixedse33dist,
                  fixedse34dist=fixedse34dist,
                  fixedse35dist=fixedse35dist,
                  fixedse36dist=fixedse36dist,
                  fixedse37dist=fixedse37dist,
                  fixedse38dist=fixedse38dist,
                  fixedse39dist=fixedse39dist,
                  fixedse40dist=fixedse40dist,
                  fixedse41dist=fixedse41dist,
                  fixedse42dist=fixedse42dist,
                  fixedse43dist=fixedse43dist,
                  fixedse44dist=fixedse44dist,
                  fixedse45dist=fixedse45dist,
                  fixedse46dist=fixedse46dist,
                  fixedse47dist=fixedse47dist,
                  fixedse48dist=fixedse48dist,
                  fixedse49dist=fixedse49dist,
                  fixedse50dist=fixedse50dist,
                  fixedse51dist=fixedse51dist,
                  fixedse52dist=fixedse52dist)
  if(overwrite)
  save.dta13(psa.hb$psasummary,("./women/output/women_current_psa.dta"))
}
