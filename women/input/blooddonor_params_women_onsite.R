## WOMEN
## Beta co-efficients
hb.b<-c(visit.coef=-0.5723, rettime.coef=0.0118, age1.coef=0.0674, age2.coef=-0.0546, age3.coef=0.1116, age4.coef=0.1888, age5.coef=0.1733, eth.coef=-0.4193, blood2.coef=0.0098, blood3.coef=0.0963, blood4.coef=0.0879, blood5.coef=0.0776, blood6.coef=0.0182, blood7.coef=0.0732, blood8.coef=0.2769, cons=13.3709)
dropout.b<-c(cons=-2.2176)
defer.b<-c(cons=-0.5287, hb1.coef=0, hb2.coef=0, hb3.coef=0)
othdefer.b<-c(cons=-2.8513)


# STRATEGY (MODEL) CO-EFFICIENTS
strategy.b<-c(current=0, optimal=1, atrisk=1, indemand=0, atrisk.thresh=0.9, indemand.thresh=0.7, lowhbdef.recall=12, vlowhbdef.recall=52, othdef.recall=4, prob.thresh=0.7, min.recall=16, curr.recall=16, length=52, thresh=12.5, hb.min=8.7, hb.max=20.8) # indicators for recall strategy type


psa.b.params=list("hb"=hb.b,
                  "dropout"=dropout.b, 
                  "defer"=defer.b, 
                  "othdefer"=othdefer.b,
                  "strategy"=strategy.b
)


## Variance-covariance matrices (lower triangular part)
varcovs<-list(
  hb.v=cbind(visit.coef=c(0.00249, -0.000136, -0.00000477, -0.000000870, 0.000000430, -0.00000587, -0.00000106, -0.0000237, -0.00000482, -0.00000673, -0.0000128, 0.000000276, 0.00000650, -0.00000302, -0.0000321, -0.0000257), 
             rettime.coef=c(NA, 0.00000774, 0.0000000989, -0.000000243, -0.000000250, -0.0000000156, -0.000000308, 0.00000150, 0.000000218, 0.000000404, 0.000000761, 0.0000000197, -0.000000363, 0.000000305, 0.00000195, 0.0000000643), 
             age1.coef=c(NA, NA, 0.00192, 0.00125, 0.00126, 0.00126, 0.00126, 0.000106, -0.00000597, 0.00000525, -0.00000579, 0.000000952, 0.000000494, 0.0000206, 0.0000529, -0.00126), 
             age2.coef=c(NA, NA, NA, 0.00176, 0.00126, 0.00126, 0.00126, 0.000195, -0.0000143, -0.0000000479, 0.00000102, -0.0000000840, -0.0000228, 0.0000263, 0.0000527, -0.00126), 
             age3.coef=c(NA, NA, NA, NA, 0.00162, 0.00126, 0.00126, 0.000199, -0.00000285, -0.00000264, 0.0000148, -0.00000625, -0.0000259, 0.0000424, 0.0000671, -0.00126), 
             age4.coef=c(NA, NA, NA, NA, NA, 0.00171, 0.00126, 0.000221, -0.0000214, -0.00000190, 0.0000186, -0.00000804, -0.0000298, 0.0000438, 0.0000550, -0.00125), 
             age5.coef=c(NA, NA, NA, NA, NA, NA, 0.00229, 0.000274, -0.00000332, -0.00000926, 0.0000326, -0.00000444, -0.0000168, 0.0000442, 0.0000381, -0.00126), 
             eth.coef=c(NA, NA, NA, NA, NA, NA, NA, 0.00706, 0.0000140, -0.000228, -0.000114, -0.0000848, 0.0000110, -0.000229, 0.0000605, -0.000234), 
             blood2.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, 0.00129, 0.000329, 0.000329, 0.000329, 0.000330, 0.000328, 0.000329, -0.000320), 
             blood3.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00170, 0.000333, 0.000332, 0.000329, 0.000337, 0.000328, -0.000326), 
             blood4.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00365, 0.000330, 0.000328, 0.000335, 0.000329, -0.000339), 
             blood5.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.000618, 0.000329, 0.000332, 0.000328, -0.000325), 
             blood6.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.000990, 0.000327, 0.000328, -0.000310), 
             blood7.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00772, 0.000329, -0.000361), 
             blood8.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.0115, -0.000382),
             cons=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00150)),
 
  dropout.v=cbind(cons=c(0.00121)), 
  defer.v=cbind(cons=c(0.00292,0,0,0), 
                hb1.coef=c(NA,0,0,0), 
                hb2.coef=c(NA, NA, 0, 0), 
                hb3.coef=c(NA, NA, NA, 0)),  
  othdefer.v=cbind(cons=c(0.00229)),
  
  
  strategy.v=cbind(current=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   optimal=c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   atrisk=c(NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   indemand=c(NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   atrisk.thresh=c(NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   indemand.thresh=c(NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   lowhb.recall=c(NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   vlowhb.recall=c(NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                   othdef.recall=c(NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0, 0), 
                   length=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0, 0), 
                   prob.thresh=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, 0), 
                   min.recall=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0), 
                   curr.recall=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0), 
                   thresh=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0),
                   hb.min=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0), 
                   hb.max=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
)

for(i in names(varcovs)){
  assign(i,varcovs[[i]])
  eval(parse(text=paste0(i,"[upper.tri(",i,")] <- t(",i,")[upper.tri(",i,")]")))
  eval(parse(text=paste0("rownames(",i,")<-colnames(",i,")")))
}
psa.v.params=list("hb"=hb.v,
                  "dropout"=dropout.v, 
                  "defer"=defer.v, 
                  "othdefer"=othdefer.v, 
                  "strategy"=strategy.v
)





