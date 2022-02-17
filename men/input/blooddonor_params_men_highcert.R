## MEN
## Beta co-efficients
hb.b<-c(visit.coef=-0.4092, rettime.coef=0.0038, age1.coef=-0.0396, age2.coef=-0.1823, age3.coef=-0.2209, age4.coef=-0.3627, age5.coef=-0.4945, eth.coef=-0.3791, blood2.coef=0.0278, blood3.coef=0.1211, blood4.coef=0.1500, blood5.coef=0.0815, blood6.coef=0.1277, blood7.coef=0.0946, blood8.coef=0.0866, cons=15.2794)
dropout.b<-c(cons=-2.6449)
defer.b<-c(cons=-0.9027, hb1.coef=0, hb2.coef=0, hb3.coef=0)
othdefer.b<-c(cons=-3.1916)


# STRATEGY (MODEL) CO-EFFICIENTS
strategy.b<-c(current=0, optimal=1, atrisk=0, indemand=0, atrisk.thresh=0.9, indemand.thresh=0.7, lowhbdef.recall=12, vlowhbdef.recall=52, othdef.recall=4, prob.thresh=0.9, min.recall=12, curr.recall=12, length=52, thresh=13.5, hb.min=8.6, hb.max=21.1) # hb.min/max from BLUPS file

psa.b.params=list("hb"=hb.b,
                  "dropout"=dropout.b, 
                  "defer"=defer.b, 
                  "othdefer"=othdefer.b,
                  "strategy"=strategy.b
)


## Variance-covariance matrices (lower triangular part)
varcovs<-list(
  hb.v=cbind(visit.coef=c(0.00210, -0.000119, -0.0000205, -0.0000310, -0.0000193, -0.0000341, -0.0000421, -0.0000181, -0.00000157, -0.00000763, 0.0000138, -0.00000269, -0.00000194, -0.0000149, 0.0000000497, -0.00000337), 
             rettime.coef=c(NA, 0.00000701, 0.000000871, 0.00000146, 0.000000692, 0.00000154, 0.00000198, 0.00000113, 0.000000112, 0.000000423, -0.000000780, 0.000000173, 0.000000103, 0.000000911, 0.0000000842, -0.00000134), 
             age1.coef=c(NA, NA, 0.00340, 0.00231, 0.00231, 0.00231, 0.00231, -0.0000442, -0.0000395, 0.0000257, -0.0000450, -0.0000115, -0.00000293, -0.0000649, -0.000123, -0.00230), 
             age2.coef=c(NA, NA, NA, 0.00311, 0.00231, 0.00231, 0.00231, 0.00000208, -0.0000178, 0.0000330, -0.0000125, 0.00000574, -0.0000280, -0.0000192, -0.0000992, -0.00230), 
             age3.coef=c(NA, NA, NA, NA, 0.00272, 0.00231, 0.00231, 0.0000767, -0.0000144, 0.0000372, -0.0000545, 0.00000559, -0.00000884, 0.0000386, -0.0000532, -0.00231), 
             age4.coef=c(NA, NA, NA, NA, NA, 0.00274, 0.00231, 0.000135, -0.0000145, 0.000019, 0.00000860, 0.00000870, -0.0000113, 0.0000279, -0.0000899, -0.00231), 
             age5.coef=c(NA, NA, NA, NA, NA, NA, 0.00314, 0.000151, 0.0000177, 0.0000326, 0.00000600, -0.00000459, -0.00000812, 0.0000443, -0.0000779, -0.00231), 
             eth.coef=c(NA, NA, NA, NA, NA, NA, NA, 0.00706, 0.0000784, -0.000335, -0.00000880, -0.0000769, 0.0000393, -0.000234, 0.0000757, -0.000151), 
             blood2.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, 0.00178, 0.000375, 0.000379, 0.000377, 0.000378, 0.000378, 0.000380, -0.000367), 
             blood3.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00209, 0.000377, 0.000382, 0.000376, 0.000391, 0.000374, -0.000402), 
             blood4.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00605, 0.000378, 0.000378, 0.000380, 0.000376, -0.000359), 
             blood5.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.000707, 0.000377, 0.000382, 0.000378, -0.000380), 
             blood6.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00127, 0.000377, 0.000379, -0.000367), 
             blood7.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00436, 0.000380, -0.000392), 
             blood8.coef=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.0146, -0.000302),
             cons=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.00258)),

  dropout.v=cbind(cons=c(0.00203)), 
  
  defer.v=cbind(cons=c(0.00790,0,0,0), 
                hb1.coef=c(NA,0,0,0), 
                hb2.coef=c(NA, NA, 0, 0), 
                hb3.coef=c(NA, NA, NA, 0)),  
  
  othdefer.v=cbind(cons=c(0.00356)),
  
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




