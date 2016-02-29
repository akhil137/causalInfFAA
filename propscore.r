#fit propensity score model single AP at a time

library(twang)

#read in dataset
#file dir: /Users/ashah/NoBackup/code/nasa/src/causalInfFAA
twang_taf.jfk<-read.csv("twang_taf_jfk_noMissing.csv")
ps.jfk<-ps(status_JFK ~ cross_JFK_1 + vis_JFK_1 + snow_JFK_1 + TS_JFK_1 + rain_JFK_1 + A_JFK,data=twang_taf.jfk,estimand="ATT")

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
