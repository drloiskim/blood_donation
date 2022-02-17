##### HB OVER TIME: DES MODEL #####

## 1. draw individual from population
## 2. set initial values, e.g. t=0 (time since last donation)
## 3. calculate hb & p>thresh at each t, given covs (should this simulate from the distribution of BLUPS, given covs?)
## 4. for each policy: calculate recall time (given p-cut and recall min for optimal policy)
## 5. draw p(attend) and return delay time (can we build in association with hb (joint model)?)
## 6. draw p(defer | under thresh) [for combined policy (predicted + haemocue for high risk) and status quo, draw p(defer | under thresh) (I think currently assuming all deferrals are under threshold)]
## 7. record donation / deferral and bleed under threshold indicators at time t 
## 8. reset t=0 if donation 
## 9. re-run steps (3-8) if donation

## SET UP ##
# default = current
des<-function(N,params,agedist,exagedist,hbdist,ethdist,blooddist,prevdefdist,prevdondist,blupdist,blupsedist,recall,attend, 
              fixedse0dist,
              fixedse1dist,
              fixedse2dist,
              fixedse3dist,
              fixedse4dist,
              fixedse5dist,
              fixedse6dist,
              fixedse7dist,
              fixedse8dist,
              fixedse9dist,
              fixedse10dist,
              fixedse11dist,
              fixedse12dist,
              fixedse13dist,
              fixedse14dist,
              fixedse15dist,
              fixedse16dist,
              fixedse17dist,
              fixedse18dist,
              fixedse19dist,
              fixedse20dist,
              fixedse21dist,
              fixedse22dist,
              fixedse23dist,
              fixedse24dist,
              fixedse25dist,
              fixedse26dist,
              fixedse27dist,
              fixedse28dist,
              fixedse29dist,
              fixedse30dist,
              fixedse31dist,
              fixedse32dist,
              fixedse33dist,
              fixedse34dist,
              fixedse35dist,
              fixedse36dist,
              fixedse37dist,
              fixedse38dist,
              fixedse39dist,
              fixedse40dist,
              fixedse41dist,
              fixedse42dist,
              fixedse43dist,
              fixedse44dist,
              fixedse45dist,
              fixedse46dist,
              fixedse47dist,
              fixedse48dist,
              fixedse49dist,
              fixedse50dist,
              fixedse51dist,
              fixedse52dist
              ){
 
  for(i in names(params)){
  params[[i]]<-params[[i]][sort(names(params[[i]]))]
  }
  
  ## check that length of agedist, sexdist and aaadiam are the same, or that the length is one (e.g everyone has the same age)
  if(length(hbdist)!=length(agedist)){
    stop(paste0("Length of agedist, hbdist must be the same"))
  }
  
  allevents<-matrix(nrow=N,ncol=9) 
  colnames(allevents)<-c("baseline","attend","test","recall","donate","defer lowhb","bleed under threshold","dropout","defer other")
  eventhistory<-data.frame(event=NULL,time=NULL)

  ind<-sample.int(length(agedist),size=N,replace = T)
  age<-agedist[ind]
  exage<-exagedist[ind]
  hb1<-hbdist[ind]
  eth<-ethdist[ind]
  blood<-blooddist[ind]
  prevdef<-prevdefdist[ind]
  prevdon<-prevdondist[ind]
  blup<-blupdist[ind]
  blupse<-blupsedist[ind]

  fixedse0<-fixedse0dist[ind]
  fixedse1<-fixedse1dist[ind]
  fixedse2<-fixedse2dist[ind]
  fixedse3<-fixedse3dist[ind]
  fixedse4<-fixedse4dist[ind]
  fixedse5<-fixedse5dist[ind]
  fixedse6<-fixedse6dist[ind]
  fixedse7<-fixedse7dist[ind]
  fixedse8<-fixedse8dist[ind]
  fixedse9<-fixedse9dist[ind]
  fixedse10<-fixedse10dist[ind]
  fixedse11<-fixedse11dist[ind]
  fixedse12<-fixedse12dist[ind]
  fixedse13<-fixedse13dist[ind]
  fixedse14<-fixedse14dist[ind]
  fixedse15<-fixedse15dist[ind]
  fixedse16<-fixedse16dist[ind]
  fixedse17<-fixedse17dist[ind]
  fixedse18<-fixedse18dist[ind]
  fixedse19<-fixedse19dist[ind]
  fixedse20<-fixedse20dist[ind]
  fixedse21<-fixedse21dist[ind]
  fixedse22<-fixedse22dist[ind]
  fixedse23<-fixedse23dist[ind]
  fixedse24<-fixedse24dist[ind]
  fixedse25<-fixedse25dist[ind]
  fixedse26<-fixedse26dist[ind]
  fixedse27<-fixedse27dist[ind]
  fixedse28<-fixedse28dist[ind]
  fixedse29<-fixedse29dist[ind]
  fixedse30<-fixedse30dist[ind]
  fixedse31<-fixedse31dist[ind]
  fixedse32<-fixedse32dist[ind]
  fixedse33<-fixedse33dist[ind]
  fixedse34<-fixedse34dist[ind]
  fixedse35<-fixedse35dist[ind]
  fixedse36<-fixedse36dist[ind]
  fixedse37<-fixedse37dist[ind]
  fixedse38<-fixedse38dist[ind]
  fixedse39<-fixedse39dist[ind]
  fixedse40<-fixedse40dist[ind]
  fixedse41<-fixedse41dist[ind]
  fixedse42<-fixedse42dist[ind]
  fixedse43<-fixedse43dist[ind]
  fixedse44<-fixedse44dist[ind]
  fixedse45<-fixedse45dist[ind]
  fixedse46<-fixedse46dist[ind]
  fixedse47<-fixedse47dist[ind]
  fixedse48<-fixedse48dist[ind]
  fixedse49<-fixedse49dist[ind]
  fixedse50<-fixedse50dist[ind]
  fixedse51<-fixedse51dist[ind]
  fixedse52<-fixedse52dist[ind]
  
  baseline.covariates<-as.matrix(data.frame(age=age,exage=exage,hb1=hb1,eth=eth,blood=blood,prevdef=prevdef,prevdon=prevdon,blup=blup,blupse=blupse,
                                            fixedse0=fixedse0,
                                            fixedse1=fixedse1,
                                            fixedse2=fixedse2,
                                            fixedse3=fixedse3,
                                            fixedse4=fixedse4,
                                            fixedse5=fixedse5,
                                            fixedse6=fixedse6,
                                            fixedse7=fixedse7,
                                            fixedse8=fixedse8,
                                            fixedse9=fixedse9,
                                            fixedse10=fixedse10,
                                            fixedse11=fixedse11,
                                            fixedse12=fixedse12,
                                            fixedse13=fixedse13,
                                            fixedse14=fixedse14,
                                            fixedse15=fixedse15,
                                            fixedse16=fixedse16,
                                            fixedse17=fixedse17,
                                            fixedse18=fixedse18,
                                            fixedse19=fixedse19,
                                            fixedse20=fixedse20,
                                            fixedse21=fixedse21,
                                            fixedse22=fixedse22,
                                            fixedse23=fixedse23,
                                            fixedse24=fixedse24,
                                            fixedse25=fixedse25,
                                            fixedse26=fixedse26,
                                            fixedse27=fixedse27,
                                            fixedse28=fixedse28,
                                            fixedse29=fixedse29,
                                            fixedse30=fixedse30,
                                            fixedse31=fixedse31,
                                            fixedse32=fixedse32,
                                            fixedse33=fixedse33,
                                            fixedse34=fixedse34,
                                            fixedse35=fixedse35,
                                            fixedse36=fixedse36,
                                            fixedse37=fixedse37,
                                            fixedse38=fixedse38,
                                            fixedse39=fixedse39,
                                            fixedse40=fixedse40,
                                            fixedse41=fixedse41,
                                            fixedse42=fixedse42,
                                            fixedse43=fixedse43,
                                            fixedse44=fixedse44,
                                            fixedse45=fixedse45,
                                            fixedse46=fixedse46,
                                            fixedse47=fixedse47,
                                            fixedse48=fixedse48,
                                            fixedse49=fixedse49,
                                            fixedse50=fixedse50,
                                            fixedse51=fixedse51,
                                            fixedse52=fixedse52
                                            ))
  
  result<-lapply(1:N,des.donor,params,baseline.covariates,recall,attend)
  eventlist<-sapply(result,"[","eventlist")
  timelist<-sapply(result,"[","timelist")
  donornumlist<-sapply(result,"[","donornumlist")
  agelist<-sapply(result,"[","agelist")
  exagelist<-sapply(result,"[","exagelist")
  ethlist<-sapply(result,"[","ethlist")
  bloodlist<-sapply(result,"[","bloodlist")
  prevdeflist<-sapply(result,"[","prevdeflist")
  prevdonlist<-sapply(result,"[","prevdonlist")
  hblist<-sapply(result,"[","hblist")
  hbcurrlist<-sapply(result,"[","hbcurrlist")
  eventhistory<-data.frame(donornum=unlist(donornumlist), event=unlist(eventlist),time=unlist(timelist),
                           hb1=unlist(hblist),age=unlist(agelist),exage=unlist(exagelist),blood=unlist(bloodlist),eth=unlist(ethlist),prevdef=unlist(prevdeflist),prevdon=unlist(prevdonlist),hbcurr=unlist(hbcurrlist))
  
  allevents<-t(sapply(eventlist,function(x){table(factor(x,levels=colnames(allevents)))}))
  eventhistory$event<-factor(eventhistory$event,levels=colnames(allevents))
  return(list(eventhistory=eventhistory,allevents=allevents,baseline.covariates=baseline.covariates)) 
  
  
}


## INDIVIDUAL SIMULATION ##

## INITIAL VISIT
## 1. draw individual from population
des.donor<-function(i,params,baseline.covariates,recall,attend){

  eventlist<-vector()
  timelist<-vector()
  nexttime<-vector()
  agelist<-vector()
  exagelist<-vector()
  ethlist<-vector()
  bloodlist<-vector()
  prevdeflist<-vector()
  prevdonlist<-vector()
  hblist<-vector()
  hbcurrlist<-vector()
  donornumlist<-vector()
  j<-0
  currenttime<-0
  
## 2. set initial values
  last.donation<-0
  last.attend<-0
  timesince.donation<-0
  initial.donation<-1

# deferral & donation events cannot occur until recall
  nexttime["attend"]<-Inf
  nexttime["test"]<-Inf
  nexttime["donate"]<-Inf
  nexttime["defer lowhb"]<-Inf
  nexttime["bleed under threshold"]<-Inf  
  nexttime["dropout"]<-Inf
  nexttime["defer other"]<-Inf
  
  covariates.i<-baseline.covariates[i,]
  agei<-covariates.i["age"]
  exagei<-covariates.i["exage"]
  hb1i<-covariates.i["hb1"]
  ethi<-covariates.i["eth"]
  bloodi<-covariates.i["blood"]
  blupi<-covariates.i["blup"]
  blupsei<-covariates.i["blupse"]
  prevdefi<-covariates.i["prevdef"]
  prevdoni<-covariates.i["prevdon"]
  donornum<-i
  
  fixedse0.i<-covariates.i["fixedse0"]
  fixedse1.i<-covariates.i["fixedse1"]
  fixedse2.i<-covariates.i["fixedse2"]
  fixedse3.i<-covariates.i["fixedse3"]
  fixedse4.i<-covariates.i["fixedse4"]
  fixedse5.i<-covariates.i["fixedse5"]
  fixedse6.i<-covariates.i["fixedse6"]
  fixedse7.i<-covariates.i["fixedse7"]
  fixedse8.i<-covariates.i["fixedse8"]
  fixedse9.i<-covariates.i["fixedse9"]
  fixedse10.i<-covariates.i["fixedse10"]
  fixedse11.i<-covariates.i["fixedse11"]
  fixedse12.i<-covariates.i["fixedse12"]
  fixedse13.i<-covariates.i["fixedse13"]
  fixedse14.i<-covariates.i["fixedse14"]
  fixedse15.i<-covariates.i["fixedse15"]
  fixedse16.i<-covariates.i["fixedse16"]
  fixedse17.i<-covariates.i["fixedse17"]
  fixedse18.i<-covariates.i["fixedse18"]
  fixedse19.i<-covariates.i["fixedse19"]
  fixedse20.i<-covariates.i["fixedse20"]
  fixedse21.i<-covariates.i["fixedse21"]
  fixedse22.i<-covariates.i["fixedse22"]
  fixedse23.i<-covariates.i["fixedse23"]
  fixedse24.i<-covariates.i["fixedse24"]
  fixedse25.i<-covariates.i["fixedse25"]
  fixedse26.i<-covariates.i["fixedse26"]
  fixedse27.i<-covariates.i["fixedse27"]
  fixedse28.i<-covariates.i["fixedse28"]
  fixedse29.i<-covariates.i["fixedse29"]
  fixedse30.i<-covariates.i["fixedse30"]
  fixedse31.i<-covariates.i["fixedse31"]
  fixedse32.i<-covariates.i["fixedse32"]
  fixedse33.i<-covariates.i["fixedse33"]
  fixedse34.i<-covariates.i["fixedse34"]
  fixedse35.i<-covariates.i["fixedse35"]
  fixedse36.i<-covariates.i["fixedse36"]
  fixedse37.i<-covariates.i["fixedse37"]
  fixedse38.i<-covariates.i["fixedse38"]
  fixedse39.i<-covariates.i["fixedse39"]
  fixedse40.i<-covariates.i["fixedse40"]
  fixedse41.i<-covariates.i["fixedse41"]
  fixedse42.i<-covariates.i["fixedse42"]
  fixedse43.i<-covariates.i["fixedse43"]
  fixedse44.i<-covariates.i["fixedse44"]
  fixedse45.i<-covariates.i["fixedse45"]
  fixedse46.i<-covariates.i["fixedse46"]
  fixedse47.i<-covariates.i["fixedse47"]
  fixedse48.i<-covariates.i["fixedse48"]
  fixedse49.i<-covariates.i["fixedse49"]
  fixedse50.i<-covariates.i["fixedse50"]
  fixedse51.i<-covariates.i["fixedse51"]
  fixedse52.i<-covariates.i["fixedse52"]
  
  age0<-0
  age1<-0
  age2<-0
  age3<-0
  age4<-0
  age5<-0

  if(exagei<25){
    age0<-1
  }
  if(exagei<35 & exagei>=25){
    age1<-1
  }
  if(exagei<45 & exagei>=35){
    age2<-1
  }
  if(exagei<55 & exagei>=45){
    age3<-1
  }
  if(exagei<65 & exagei>=55){
    age4<-1
  }
  if(exagei>=65){
    age5<-1
  }

  blood1<-0
  blood2<-0
  blood3<-0
  blood4<-0
  blood5<-0
  blood6<-0
  blood7<-0
  blood8<-0
  
  if(bloodi==1){
    blood1<-1
  }
  if(bloodi==2){
    blood2<-1
  }
  if(bloodi==3){
    blood3<-1
  }
  if(bloodi==4){
    blood4<-1
  }
  if(bloodi==5){
    blood5<-1
  }
  if(bloodi==6){
    blood6<-1
  }
  if(bloodi==7){
    blood7<-1
  }
  if(bloodi==8){
    blood8<-1
  }
  
  
  nlow1<-0
  nlow2<-0
  nlow3<-0
  
  if(prevdefi==0){
    nlow1<-1
  }
  if(prevdefi==1){
    nlow2<-1
  }
  if(prevdefi==2){
    nlow3<-1
  }
  
  ndon1<-0
  ndon2<-0
  ndon3<-0
  
  if(prevdoni==0){
    ndon1<-1
  }
  if(prevdoni==1){
    ndon2<-1
  }
  if(prevdoni==2){
    ndon3<-1
  }
  
  eth1<-ethi
  
  covi<-data.frame(eth1,age0,age1,age2,age3,age4,age5,blood1,blood2,blood3,blood4,blood5,blood6,blood7,blood8,nlow1,nlow2,nlow3,ndon1,ndon2,ndon3)
  
## 3. calculate hb and p>thresh at each t, given covs
  hb.last<-hb1i
  blupdraw<-mvrnorm(1,blupi,blupsei)
  blupdiff<-blupdraw-blupi

  result<-data.frame(matrix(nrow=52,ncol=3))
  colnames(result)<-c("t", "hb", "prob")
  for(t in 1:52) {
  hb.time<-(params$hb["cons"])+(params$hb["visit.coef"])+((params$hb["rettime.coef"])*t)+((params$hb["age1.coef"])*age1)+((params$hb["age2.coef"])*age2)+((params$hb["age3.coef"])*age3)+((params$hb["age4.coef"])*age4)+((params$hb["age5.coef"])*age5)+((params$hb["eth.coef"])*ethi)+((params$hb["blood2.coef"])*blood2)+((params$hb["blood3.coef"])*blood3)+((params$hb["blood4.coef"])*blood4)+((params$hb["blood5.coef"])*blood5)+((params$hb["blood6.coef"])*blood6)+((params$hb["blood7.coef"])*blood7)+((params$hb["blood8.coef"])*blood8)+blupi
  fixedse.t.nam <- paste("fixedse", t, ".i", sep = "")
  fixedse.t<-get(fixedse.t.nam)
  allse.t<-fixedse.t+blupsei
  prob.time<-1-pnorm(params$strategy["thresh"], mean = hb.time, sd = allse.t)
  result[t,1]<-t
  result[t,2]<-hb.time
  result[t,3]<-prob.time
  }
  
  if(params$strategy["indemand"]==0){
    ## WHAT HAPPENS IF IN 52W PROB NEVER REACHES PROB.THRESH? x is not being calulated, but is still x=params$othdef (being used elsewhere)
  x<-result$t[result$prob>params$strategy["prob.thresh"]]
  y<-min(x)
  z<-max(params$strategy["min.recall"],y)
  prob.z<-result$prob[result$t==z]
  }
  
  # in-demand groups for early recall defined as B- (blood=4) & O- (blood=6)
  if(params$strategy["indemand"]==1){
    if(bloodi==4 | bloodi==6){
      x<-result$t[result$prob>params$strategy["indemand.thresh"]]
      y<-min(x)
    } 
    else{
      x<-result$t[result$prob>params$strategy["prob.thresh"]]
      y<-min(x)
    }
  z<-max(params$strategy["min.recall"],y)
  prob.z<-result$prob[result$t==z]
  }
  
  if(result$prob[result$t==52]<params$strategy["prob.thresh"]) {
  x<-1000
  y<1000
  z<1000
  prob.z<-1
  }

## 4. calculate next recall time
  nexttime["recall"]<-recall(currenttime,eventlist,timelist,last.attend,z)

  # store initial history for this donor
  # baseline
  j<-1
  timelist[j]<-0 
  agelist[j]<-agei
  exagelist[j]<-exagei
  hblist[j]<-hb1i
  ethlist[j]<-ethi
  bloodlist[j]<-bloodi
  prevdeflist[j]<-prevdefi
  prevdonlist[j]<-prevdoni
  event<-"baseline"
  eventlist[j]<-event
  donornumlist[j]<-i
  hbcurrlist[j]<-hb1i
  
  # initial donation
  j<-2
  timelist[j]<-0.01 
  agelist[j]<-agei
  exagelist[j]<-exagei+0.01
  hblist[j]<-hb1i
  ethlist[j]<-ethi
  bloodlist[j]<-bloodi
  prevdeflist[j]<-prevdefi
  prevdonlist[j]<-prevdoni
  event<-"donate"
  eventlist[j]<-event
  donornumlist[j]<-i
  # modelled hb after initial donation (ie not p>thresh)
    # fixed part
    hb.curr.fixed<-(params$hb["cons"])+(params$hb["visit.coef"])+((params$hb["rettime.coef"])*0.01)+((params$hb["age1.coef"])*age1)+((params$hb["age2.coef"])*age2)+((params$hb["age3.coef"])*age3)+((params$hb["age4.coef"])*age4)+((params$hb["age5.coef"])*age5)+((params$hb["eth.coef"])*ethi)+((params$hb["blood2.coef"])*blood2)+((params$hb["blood3.coef"])*blood3)+((params$hb["blood4.coef"])*blood4)+((params$hb["blood5.coef"])*blood5)+((params$hb["blood6.coef"])*blood6)+((params$hb["blood7.coef"])*blood7)+((params$hb["blood8.coef"])*blood8)
    # uncertainty about fixed part: draw from z = #SEs from mean
    hb.curr.draw<-mvrnorm(1,0,1)
    # fixed mean + (z*SE = value +/- from fixed mean), then add blup
    hb.curr<-hb.curr.fixed+(hb.curr.draw*fixedse1.i)+(blupi+blupdiff)
  hbcurrlist[j]<-hb.curr
  
## SUBSEQUENT VISITS
repeat {

  j<-j+1

  ## generate next event and event time as minimum of all next event times
  event<-names(which.min(nexttime)) 
  time<-nexttime[which.min(nexttime)]

  ## Update current time to (next) event time
  currenttime<-time
  
  # end if time>52w (1y)
  if(currenttime>(params$strategy["length"])){
    break
  }
  
  # update age
  exagei<-covariates.i["exage"]+(currenttime/52)
  
  age0<-0
  age1<-0
  age2<-0
  age3<-0
  age4<-0
  age5<-0
  
  if(exagei<25){
    age0<-1
    agecat<-0
  }
  if(exagei<35 & exagei>=25){
    age1<-1
    agecat<-1
  }
  if(exagei<45 & exagei>=35){
    age2<-1
    agecat<-2
  }
  if(exagei<55 & exagei>=45){
    age3<-1
    agecat<-3
  }
  if(exagei<65 & exagei>=55){
    age4<-1
    agecat<-4
  }
  if(exagei>=65){
    age5<-1
    agecat<-5
  }
  


  # calculate pre-donation hb
  if(event=="donate" | event=="bleed under threshold"){
    hb.last<-hbcurrlist[j-1]
    last.donation <- currenttime
    initial.donation<-0
  }
  # update deferral count
  if(event=="defer lowhb") {
    prevdefi<-prevdefi+1
  }
  
## 8. reset t=0 if donation
  timesince.donation <- currenttime-last.donation

  # BLUPs based on last donation
  hb.last.r<-round(hb.last,digits=1)
  # database does not hold values for hb<obs min or hb>obs max
  if(hb.last.r<params$strategy["hb.min"]) {
    hb.last.r<-params$strategy["hb.min"]
  }
  if(hb.last.r>params$strategy["hb.max"]){
    hb.last.r<-params$strategy["hb.max"]
  }
  
  #browser()
  hb.last.d<-as.data.frame(hb.last.r)
  lookup<-merge(blups,hb.last.d)
  age.d<-as.data.frame(agecat)
  lookup2<-merge(lookup,age.d)
  eth.d<-as.data.frame(ethi)
  lookup3<-merge(lookup2,eth.d)
  blood.d<-as.data.frame(bloodi)
  lookup4<-merge(lookup3,blood.d)
  newblupi<-lookup4$blup[lookup4$hb>(lookup4$hb.last.r-0.02) & lookup4$hb<(lookup4$hb.last.r+0.02) & lookup4$age==lookup4$agecat & lookup4$eth==lookup4$ethi & lookup4$blood==lookup4$bloodi]
  # noting all blupses are the same, so do not need to re-lookup
  #newblupse<-lookup2$blupse[lookup2$hb>(lookup2$hb.last.r-0.02) & lookup2$hb<(lookup2$hb.last.r+0.02) & lookup2$age==lookup2$agecat]
  # retaining diff from mean from original draw: blupdraw<-newblup+blupdiff, and applying to new mean 
  # otherwise use newblup (instead of blupdraw) in calc of hb.curr to just fit mean values
  # retain original blupdraw following inital donation
  if(initial.donation==0){
  blupi<-newblupi
  }
  
  # calculate current/post-donation hb
  # using newblupi after 2nd donation
  hb.curr.fixed<-(params$hb["cons"])+(params$hb["visit.coef"])+((params$hb["rettime.coef"])*timesince.donation)+((params$hb["age1.coef"])*age1)+((params$hb["age2.coef"])*age2)+((params$hb["age3.coef"])*age3)+((params$hb["age4.coef"])*age4)+((params$hb["age5.coef"])*age5)+((params$hb["eth.coef"])*ethi)+((params$hb["blood2.coef"])*blood2)+((params$hb["blood3.coef"])*blood3)+((params$hb["blood4.coef"])*blood4)+((params$hb["blood5.coef"])*blood5)+((params$hb["blood6.coef"])*blood6)+((params$hb["blood7.coef"])*blood7)+((params$hb["blood8.coef"])*blood8)
  timesince.donation.r<-round(timesince.donation,digits=0)
  
  # exiting follow-up if it has been >1y since last donation
  if(timesince.donation.r>52){
    break
  }
  
  fixedse.t.nam <- paste("fixedse", timesince.donation.r, ".i", sep = "")
  fixedse.t<-get(fixedse.t.nam)
  hb.curr<-hb.curr.fixed+(hb.curr.draw*(fixedse.t))+(blupi+blupdiff)

  ## remove eventtime from nexttime list
  nexttime<-subset(nexttime, !(names(nexttime) %in% event))
  
## 5. outcome at attendance
  if(event=="recall"){
  
    #x<-params$dropout["cons"]
    prob.dropout<-exp(params$dropout["cons"])/(1+exp(params$dropout["cons"]))
    dropout<-rbinom(1,1,prob.dropout)
    
    nexttime["attend"]<-attend(currenttime,eventlist,timelist,covi,fpmod,logcumhaz)
    
    if(dropout==1){
    nexttime["attend"]<-Inf
    nexttime["dropout"]=currenttime+0.01
    }
    
    nexttime["recall"]<-Inf
  }

## 6. draw p(defer | under thresh) 
## 7. record donation / deferral and bleed under threshold indicators 
  if(event=="attend"){
    last.attend<-currenttime
    x<-params$othdefer["cons"]
    prob.othdefer<-exp(x)/(1+exp(x))
    defer.other<-rbinom(1,1,prob.othdefer)
    
    if(defer.other==1){
    nexttime["defer other"]<-currenttime+0.01  
    }
    
    else{
    # create additional on-site test event for current strategy AND optimal + test at risk strategy
    if(params$strategy["current"]==1){
    nexttime["test"]<-currenttime+0.01
    }
  
    else if(params$strategy["atrisk"]==1 & prob.z<params$strategy["atrisk.thresh"]){
  
      nexttime["test"]<-currenttime+0.01  
    }
    # optimal strategy
    else {
      if(hb.curr<params$strategy["thresh"]){
      nexttime["bleed under threshold"]<-currenttime+0.01  
      }
      else {
      nexttime["donate"]<-currenttime+0.01  
      }
    }
    }
  }
  
  if(event=="test"){
    nexttime["test"]<-Inf
    # for those under threshold: defer or donate  
    if(hb.curr<params$strategy["thresh"]) {
      if(prevdefi>1){
      prevdef2<-1  
      }
      else{
      prevdef2<-0  
      }
      if(prevdefi==1){
      prevdef1<-1  
      }
      else{
      prevdef1<-0  
      }
      
      if(hb.curr<12.5 & hb.curr>=12.3){
      hb0=1  
      }
      else {
      hb0=0  
      }
      if(hb.curr<12.3 & hb.curr>=12){
        hb1=1  
      }
      else {
        hb1=0  
      }
      if(hb.curr<12 & hb.curr>=11.8){
        hb2=1  
      }
      else {
        hb2=0  
      }
      if(hb.curr<11.8){
        hb3=1  
      }
      else {
        hb3=0
      }
      
    xb=params$defer["cons"]+(params$defer["hb1.coef"]*hb1)+(params$defer["hb2.coef"]*hb2)+(params$defer["hb3.coef"]*hb3)
    pdefer=exp(xb)/(1+(exp(xb)))
    d<-runif(1,0,1)
      if(d<=pdefer){
        nexttime["defer lowhb"]<-currenttime+0.01
      }
      if(d>pdefer) {
        nexttime["bleed under threshold"]<-currenttime+0.01
      }
    }
    # for those over the threshold: donate
    else {
      nexttime["donate"]<-currenttime+0.01
    }
  }
  
##  CALCULATE NEXT RECALL TIME
## 3. calculate hb and p>thresh at each t, given covs
## 4. calculate next recall time
  if(event=="donate" | event=="bleed under threshold" | event=="defer lowhb" | event=="defer other") {

  result<-data.frame(matrix(nrow=52,ncol=3))
  colnames(result)<-c("t", "hb", "prob")
  for(t in 1:52) {
    hb.time<-(params$hb["cons"])+(params$hb["visit.coef"])+((params$hb["rettime.coef"])*t)+((params$hb["age1.coef"])*age1)+((params$hb["age2.coef"])*age2)+((params$hb["age3.coef"])*age3)+((params$hb["age4.coef"])*age4)+((params$hb["age5.coef"])*age5)+((params$hb["eth.coef"])*ethi)+((params$hb["blood2.coef"])*blood2)+((params$hb["blood3.coef"])*blood3)+((params$hb["blood4.coef"])*blood4)+((params$hb["blood5.coef"])*blood5)+((params$hb["blood6.coef"])*blood6)+((params$hb["blood7.coef"])*blood7)+((params$hb["blood8.coef"])*blood8)+blupi
    fixedse.t.nam <- paste("fixedse", t, ".i", sep = "")
    fixedse.t<-get(fixedse.t.nam)
    allse.t<-fixedse.t+blupsei
    prob.time<-1-pnorm(params$strategy["thresh"],mean = hb.time,sd = allse.t)
    result[t,1]<-t
    result[t,2]<-hb.time
    result[t,3]<-prob.time
    
  }

  if(params$strategy["indemand"]==0){ 
  x<-result$t[result$prob>params$strategy["prob.thresh"]]
  y<-min(x)
  z<-max(params$strategy["min.recall"],y)
  prob.z<-result$prob[result$t==z]
  }
  
  # in-demand groups for early recall defined as B- (blood=4) & O- (blood=6)
  if(params$strategy["indemand"]==1){
    if(bloodi==4 | bloodi==6){
      x<-result$t[result$prob>params$strategy["indemand.thresh"]]
      y<-min(x)
    } 
    else{
      x<-result$t[result$prob>params$strategy["prob.thresh"]]
      y<-min(x)
    }
    z<-max(params$strategy["min.recall"],y)
    prob.z<-result$prob[result$t==z]
  }
  
  if(result$prob[result$t==52]<params$strategy["prob.thresh"]) {
    x<-1000
    y<-1000
    z<-1000
    prob.z<-1
  }

  nexttime["recall"]<-recall(currenttime,eventlist,timelist,last.attend,z)
  nexttime["attend"]<-Inf
  }
  
  # recall time for low hb deferrals 
  if(event=="defer lowhb"){
  nexttime["recall"]<-last.attend+params$strategy["lowhbdef.recall"] 
    # longer deferral if <11.5 (women) / <12.5 (men)
    if(hb.curr<params$strategy["thresh"]-1) {
    nexttime["recall"]<-last.attend+params$strategy["vlowhbdef.recall"] 
    }
  }
  
  # recall time for other deferrals 
  if(event=="defer other"){
    nexttime["recall"]<-last.attend+params$strategy["othdef.recall"] 
  }
  
  # store history for this donor
  timelist[j]<-time 
  agelist[j]<-covariates.i["age"]
  exagelist[j]<-exagei
  hblist[j]<-covariates.i["hb1"]
  ethlist[j]<-covariates.i["eth"]
  bloodlist[j]<-covariates.i["blood"]
  prevdeflist[j]<-covariates.i["prevdef"]
  prevdonlist[j]<-covariates.i["prevdon"]
  eventlist[j]<-event
  donornumlist[j]<-i
  hbcurrlist[j]<-hb.curr
}


return(list(donornumlist=donornumlist,eventlist=eventlist,timelist=timelist,currenttime=currenttime, agelist=agelist, exagelist=exagelist,hblist=hblist, ethlist=ethlist,bloodlist=bloodlist,prevdeflist=prevdeflist,prevdonlist=prevdonlist,hbcurrlist=hbcurrlist))
}


### FUNCTIONS ###
# time of next recall: fixed 16w recall (current)
recall.curr<-function(currenttime,eventlist,timelist,last.attend,z){
  
  timeto.nextrecall <- params$strategy["curr.recall"]
  last.attend+timeto.nextrecall
}


# time of next recall: based on threshold probabilty / min recall params
recall.thresh<-function(currenttime,eventlist,timelist,last.attend,z){
  timeto.nextrecall <- z
  last.attend+timeto.nextrecall
}

# time of next recall: exact time (for INTERVAL validation)
recall.interval<-function(currenttime,eventlist,timelist,last.attend,z){
  timeto.nextrecall<-params$strategy["min.recall"]
  last.attend+timeto.nextrecall
}

# time between recall and attendance: from flexible parametric model
attend.dist<-function(currenttime,eventlist,timelist,covi,fpmod,logcumhaz){

   attenddelay<-tryCatch({
    simsurv(betas = fpmod$coefficients,
            x = covi,             
            knots = fpmod$knots,    
            logcumhazard = logcumhaz,  
            maxt = 100000,               
            interval = c(1e-8, 100001))
     simsurv(betas = fpmod$coefficients,
             x = covi,             
             knots = fpmod$knots,    
             logcumhazard = logcumhaz,  
             maxt = 100000,               
             interval = c(1e-8, 100001))
  }, error=function(e) 0)

  attenddelay<-tryCatch({
    ((attenddelay$eventtime/24)/7)
  }, error=function(e) 0)
  
    
    delay<-(max((params$strategy["curr.recall"]-12), attenddelay))-(max((params$strategy["curr.recall"])-12))
  currenttime+delay
}



# PSA
### Probabilistic sensitivity analysis wrapper function
psa.des<-function(B, N, params.b, params.v, agedist, exagedist, hbdist, ethdist, blooddist, prevdefdist, prevdondist, blupdist, blupsedist, recall, attend,
                  fixedse0dist,
                  fixedse1dist,
                  fixedse2dist,
                  fixedse3dist,
                  fixedse4dist,
                  fixedse5dist,
                  fixedse6dist,
                  fixedse7dist,
                  fixedse8dist,
                  fixedse9dist,
                  fixedse10dist,
                  fixedse11dist,
                  fixedse12dist,
                  fixedse13dist,
                  fixedse14dist,
                  fixedse15dist,
                  fixedse16dist,
                  fixedse17dist,
                  fixedse18dist,
                  fixedse19dist,
                  fixedse20dist,
                  fixedse21dist,
                  fixedse22dist,
                  fixedse23dist,
                  fixedse24dist,
                  fixedse25dist,
                  fixedse26dist,
                  fixedse27dist,
                  fixedse28dist,
                  fixedse29dist,
                  fixedse30dist,
                  fixedse31dist,
                  fixedse32dist,
                  fixedse33dist,
                  fixedse34dist,
                  fixedse35dist,
                  fixedse36dist,
                  fixedse37dist,
                  fixedse38dist,
                  fixedse39dist,
                  fixedse40dist,
                  fixedse41dist,
                  fixedse42dist,
                  fixedse43dist,
                  fixedse44dist,
                  fixedse45dist,
                  fixedse46dist,
                  fixedse47dist,
                  fixedse48dist,
                  fixedse49dist,
                  fixedse50dist,
                  fixedse51dist,
                  fixedse52dist){
  params<-list() ## initialise params list
  output<-list() ## initialise output list
 
  ## First create B copies of the parameters using multivariate normal sampling  
  for(i in names(params.b)){
    params[[i]]<-mvrnorm(B,params.b[[i]],params.v[[i]], tol=1e-1)
  }
  
  ## Now loop over B PSA replications
  for(b in 1:B){
    cat(b,"\n")
    pars<-lapply(params,FUN=function(x){x[b,]})
    
    if(b==1){
    psahb<-data.frame(b, pars$hb)
    psadropout<-data.frame(pars$dropout)
    psadefer<-data.frame(b,pars$defer)
    psaothdefer<-data.frame(b,pars$othdefer)
      
    psahb<-data.frame(coefname=rownames(psahb),b, pars$hb)
    psadropout<-data.frame(coefname=rownames(psadropout),b,pars$dropout)
    psadefer<-data.frame(coefname=rownames(psadefer),b,pars$defer)
    psaothdefer<-data.frame(coefname=rownames(psaothdefer),b,pars$othdefer)
    }
    
    else {
      temphb<-data.frame(b, pars$hb)
      tempdropout<-data.frame(b,pars$dropout)
      tempdefer<-data.frame(b,pars$defer)
      tempothdefer<-data.frame(b,pars$othdefer)
      
      temphb<-data.frame(coefname=rownames(temphb),b, pars$hb)
      tempdropout<-data.frame(coefname=rownames(tempdropout),b,pars$dropout)
      tempdefer<-data.frame(coefname=rownames(tempdefer),b,pars$defer)
      tempothdefer<-data.frame(coefname=rownames(tempothdefer),b,pars$othdefer)
      
      psahb<-rbind(psahb,temphb)
      psadropout<-rbind(psadropout,tempdropout)
      psadefer<-rbind(psadefer,tempdefer)
      psaothdefer<-rbind(psaothdefer,tempothdefer)
    }
    
    
    ## Run DES model
    des.output<-model.hb<-des(N=N, params=pars, agedist=agedist, exagedist=exagedist, hbdist=hbdist, ethdist=ethdist, blooddist=blooddist, prevdefdist=prevdefdist, prevdondist=prevdondist, blupdist=blupdist, blupsedist=blupsedist, recall, attend=attend.dist,
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
      
    ## Summarise output
    ## Number of event of each type
    #output$events<-rbind(output$events,apply(des.output$allevents,2,sum))
    ## Mean costs
    #output$mean.costs<-c(output$mean.costs,des.output$results$mean.costs)
    
  ## event summaries
  dropoutsumm<-sum(model.hb$eventhistory["event"]=="dropout")  
  attendsum<-sum(model.hb$eventhistory["event"]=="attend")
  testsum<-sum(model.hb$eventhistory["event"]=="test")
  donatesum<-sum(model.hb$eventhistory["event"]=="donate")
  undersum<-sum(model.hb$eventhistory["event"]=="bleed under threshold")
  raresum<-sum(model.hb$eventhistory["event"]=="donate" & model.hb$eventhistory["blood"]==4)+sum(model.hb$eventhistory["event"]=="donate" & model.hb$eventhistory["blood"]==6)+sum(model.hb$eventhistory["event"]=="bleed under threshold" & model.hb$eventhistory["blood"]==4)+sum(model.hb$eventhistory["event"]=="bleed under threshold" & model.hb$eventhistory["blood"]==6)
  deferlowsum<-sum(model.hb$eventhistory["event"]=="defer lowhb")
  deferothsum<-sum(model.hb$eventhistory["event"]=="defer other")
  
  if(b==1){
  psasummary<-data.frame(b,dropoutsumm,attendsum,testsum,donatesum,undersum,raresum,deferlowsum,deferothsum)
  }
  else {
  temp<-data.frame(b,dropoutsumm,attendsum,testsum,donatesum,undersum,raresum,deferlowsum,deferothsum)
  psasummary<-rbind(psasummary,temp)
  }
  
  ## Saving individual event histories, by bs sample  (if using, needs also to add psaeventhistory=psaeventhistory to return command in line #1057)
  #if(b==1){
  #  psaeventhistory<-cbind(b,model.hb$eventhistory)
  #}
  #else {
  #  temp<-cbind(b,model.hb$eventhistory)
  #  psaeventhistory<-rbind(psaeventhistory,temp)
  #}  
    
  }
  
  return(list(params=pars,output=output, psahb=psahb, psadropout=psadropout, psadefer=psadefer, psaothdefer=psaothdefer, psasummary=psasummary))
}