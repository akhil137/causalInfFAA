#fit propensity score model single AP at a time

library(twang)

#read in dataset
#file dir: /Users/ashah/NoBackup/code/nasa/src/causalInfFAA
twang_taf.jfk<-read.csv("twang_taf_jfk_noMissing.csv")
ps.jfk<-ps(status_JFK ~ cross_JFK_1 + vis_JFK_1 + snow_JFK_1 + TS_JFK_1 + rain_JFK_1 + A_JFK,data=twang_taf.jfk,estimand="ATT")
ps.ewr<-ps(status_EWR ~ cross_EWR_1 + vis_EWR_1 + snow_EWR_1 + TS_EWR_1 + rain_EWR_1 + A_EWR,data=twang_taf.ewr,estimand="ATT")
ps.lga<-ps(status_LGA ~ cross_LGAa_1 + cross_LGAb_1 + vis_LGA_1 + snow_LGA_1 + TS_LGA_1 + rain_LGA_1 + A_LGA,data=twang_taf.lga,estimand="ATT")
#gbm iter plot
plot(ps.jfk,plots=1)

#prop-score distribution plot among treat/control groups
plot(ps.jfk,plots=2)

#T-test p-val before after weighting
plot(ps.jfk,plots=4)

#KS test p-val
plot(ps.jfk,plots=5)

#weight distribution
plot(ps.jfk,plots=6)

#create balance tables
bal.jfk<-bal.table(ps.jfk)

#create the markdown versions to tex later:
#kable(bal.jfk$unw,format="markdown")
#kable(bal.jfk$ks.mean.ATT,format="markdown")
#kable(bal.jfk$es.mean.ATT,format="markdown")

#print imbalance tables
pretty.tab <- bal.jfk$unw[,c("tx.mn","ct.mn","ks.pval")]
#pretty.tab <- cbind(pretty.tab, bal.jfk$unw[,"ct.mn"])
names(pretty.tab) <- c("E(Y1|t=1)","E(Y0|t=0)","p-value")
xtable(pretty.tab,
caption = "Imbalance of the treatment and comparison groups for various weather and traffic (arrivals count) features (covariates)",
label = "tab:imbalance",
digits = c(0, 2, 2, 2),
align=c("l","r","r","r"))

#pretty print balance tables after weigtting
library(xtable)
pretty.tab <- bal.jfk$ks.mean.ATT[,c("tx.mn","ct.mn","ks.pval")]
pretty.tab <- cbind(pretty.tab, bal.jfk$unw[,"ct.mn"])
names(pretty.tab) <- c("E(Y1|t=1)","E(Y0|t=1)","p-value","E(Y0|t=0)")
xtable(pretty.tab,
caption = "Simulated counterfactual E(Y0|t=1) and assessment of balance achieved by propensity score weighting the treatment and comparison groups using IPTW",
label = "tab:balance",
digits = c(0, 2, 2, 2, 2),
align=c("l","r","r","r","r"))


#Outcome Analysis

#get weights and create survey design
library(survey)
#add weights to dataset frame used in creating ps model
twang_taf.jfk$w<-get.weights(ps.jfk,stop.method="es.mean")

#create survey design object with above amended ps dataset
design.jfk<-svydesign(ids=~1, weights=~w, data=twang_taf.jfk)

#analyze on weighted data
glm1<-svyglm(AirDelay_JFK ~ status_JFK, design=design.jfk)

#perform 'doubly-robust' estimation with relevant covariates
#"In addition to potential bias reduction, the inclusion of 
#additional covariates can reduce the standard error of the treatment 
#effect if some of the covariates are strongly related to the outcome.""

glm2<-svyglm(AirDelay_JFK ~ status_JFK + cross_JFK_1 + vis_JFK_1 + snow_JFK_1 + TS_JFK_1 + rain_JFK_1 + A_JFK, design=design.jfk)




#std linear regression
glm4<-lm(AirDelay_JFK ~ status_JFK + cross_JFK_1 + vis_JFK_1 + snow_JFK_1 + TS_JFK_1 + rain_JFK_1 + A_JFK,data=twang_taf.jfk)



#do it for ewr
twang_taf.ewr$w<-get.weights(ps.ewr,stop.method="es.mean")
design.ewr<-svydesign(ids=~1, weights=~w, data=twang_taf.ewr)
glm1.ewr<-svyglm(AirDelay_EWR ~ status_EWR, design=design.ewr)
glm4.ewr<-lm(AirDelay_EWR ~ status_EWR + cross_EWR_1 + vis_EWR_1 + snow_EWR_1 + TS_EWR_1 + rain_EWR_1 + A_EWR,data=twang_taf.ewr)


#and for lga
twang_taf.lga$w<-get.weights(ps.lga,stop.method="es.mean")
design.lga<-svydesign(ids=~1, weights=~w, data=twang_taf.lga)
glm1.lga<-svyglm(AirDelay_LGA ~ status_LGA, design=design.lga)
glm4.lga<-lm(AirDelay_LGA ~ status_LGA + cross_LGAa_1 + cross_LGAb_1+ vis_LGA_1 + snow_LGA_1 + TS_LGA_1 + rain_LGA_1 + A_LGA,data=twang_taf.lga)
