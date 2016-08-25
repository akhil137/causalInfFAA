#use the following df from btsUnimpededTaxiTimes.r
df = bts.merged
#there are still NA's
pretreatFeats<-c("status","crosswind_ASPM", "vis_ASPM", "ceil_ASPM", "windspeed_ASPM", 
	"TS", "rain", "A_JFK","DISTANCE","CRS_ELAPSED_TIME")
df.pretreat<-df[,pretreatFeats]

#ps analysis
library(twang)
#note there are no instances of snow  or fog, but perhaps include 
#also, note: outcome var may involve abdelay subtractin buffers
#that were computed on subsets of the population (carrier-route)
#should we fit ps on those subsets?

#ps.bts<-ps(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM +
	#TS + rain + A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=df.pretreat,estimand="ATT")


#try running with more obs per gbm leaf node --- balance is not very good
ps.bts<-ps(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM + 
	A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=df.pretreat,
	estimand="ATT",stop.method=c("es.mean","ks.max"),n.minobsinnode=35)


#try CBPS
library(CBPS)
ps.cbps<-CBPS(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM +
 	 A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=df.pretreat)


#diagnose weights using twang (exclude status, TS, and rain)
#note that cbps weights for treatment group are constant (e.g. ATT for sure)
#but not unity, rather it w_T = 1/N_T 
bal.cbps<-dx.wts(x=ps.cbps$weights,data=df.pretreat,
	vars=colnames(df.pretreat)[c(-1,-6,-7)],treat.var="status",estimand="ATT")

#create a balance table 
bal.table(bal.cbps)

#better to just pass in cbps weights, gbm doesn't improve on it much
ps.bts<-ps(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM + 
	A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=df.pretreat,
	estimand="ATT",stop.method=c("es.mean","ks.max"),n.minobsinnode=35,
	sampw=ps.cbps$weights)
#plot relative influence
summary(ps.bts$gbm.obj)

#try logit for weights
ps.logit<-glm(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM + 
	A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=df.pretreat,family=binomial)

logitw<-rep(1,nrow(df.pretreat))
logitw[df.pretreat$status==0] <- exp(predict(ps.logit,subset(df.pretreat,status==0)))

#try xgboost for weights


#pretty print balance tables after weigtting
library(xtable)
bal.jfk<-bal.table(ps.bts)
pretty.tab <- bal.jfk$ks.max.ATT[,c("tx.mn","ct.mn","ks.pval")]
pretty.tab <- cbind(pretty.tab, bal.jfk$unw[,"ct.mn"])
names(pretty.tab) <- c("E(Y1|t=1)","E(Y0|t=1)","p-value","E(Y0|t=0)")
pretty.tab

#for latex:
xtable(pretty.tab,
caption = "Simulated counterfactual E(Y0|t=1) and assessment of balance achieved by propensity score weighting the treatment and comparison groups using IPTW",
label = "tab:balance",
digits = c(0, 2, 2, 2, 2),
align=c("l","r","r","r","r"))



#see plots of gbm iterations and balance plots
