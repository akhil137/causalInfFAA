#use the following df from btsUnimpededTaxiTimes.r
df = bts.merged
#there are still NA's
pretreatFeats<-c("status","crosswind_ASPM", "vis_ASPM", "ceil_ASPM", "windspeed_ASPM", 
	"TS", "rain", "A_JFK","DISTANCE","CRS_ELAPSED_TIME","ARRDEMAND","EFFARR")
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

#outcome analysis
df.pretreat$cbps<-ps.cbps$weights
df.pretreat$ABDelay<-df$AIR_TIME-df$nomAbTimes
#outcomes for treatment and control before weighting with ATT
ggplot(df.pretreat,aes(ABDelay)) + geom_histogram(binwidth=1) +facet_grid(status ~ .,scales = "free_y")


library(survey)
design.bts<-svydesign(ids=~1, weights=~cbps, data=df.pretreat)
glm.simple<-svyglm(ABDelay ~ status, design=design.bts)

#basic linreg
glm.lm<-lm(ABDelay~ status + crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM + 
	A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=df.pretreat)

#doubly-robust
glm.dr<-svyglm(ABDelay ~ status + crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM + 
+ 	A_JFK + DISTANCE + CRS_ELAPSED_TIME, design=design.bts)

#could also try outcome as Veloc above
#can also try on a single route-carrier
lax.aal<-df.pretreat[(df$UNIQUE_CARRIER=="AAL") & (df$ORIGIN=="LAX"),]
design.bts<-svydesign(ids=~1, weights=~cbps, data=lax.aal)
glm.simple<-svyglm(ABDelay ~ status, design=design.bts)
summary(glm.simple)

#some notes on choosing the right outcome
#seems that abdelay during gdp is statistically larger than w/o gdp
#even for those non-gdp flights who have a large ps score (e.g. control units that we want to weight up)
#have a smaller ab delay...so there is no way that att will result in reduction in abdelay w/ gdp
#since counterfactual (control units with large ps score effectively) has lower abdelay.

#consider M. ball's paper "Ground Delay Programs: Optimizing over the Included Flight Set Based on Distance"
#he explains that of all the flights eligible for EDCT under a GDP, only some are assigned it.
#furthermore, the idea of a gdp is to transfer airborne delay to ground delay.
#also note that sec 4.1 says "When an imbalance between demand and capacity takes place, 
# the total amount of delay required to balance demand and capacity, 
# i.e. the sum of airborne delay and ground delay, is constant. 
# The total delay depends only on the AAR values and the flight demand 
# at the airport. Therefore, the ground delay and the airborne delay 
# are complementary with respect to total delay (see Figure 2)."

#One way to find flights that with high likelihood did not recieve EDCT (i.e. non-gdp flights)
#use stats of Percent of Flights that reieve EDCT for a given arrival hour

#for bts data; ts = hour based on CRS Scheduled arrival hour
#for aspm data (e.g. edct and TAF); ts = hour at arrival airport
#if a flight gets a edct, it's actuall arrival hour = wheels on time
#for ps score modeling of a given flight; 
#consider weather/traffic at arrival airport at scheduled arrival hour
#to determine if that flight actually recieved edct, 
#use Percent.EDCT at actual arrival hour of that flight
#and check if CRS scheduled arrival time falls within NTML GDP start to stop time, 
#which is what merging with 'status' already does.  

#note actual wheels on time = actual gate in time - taxi-in 
#							= schedule arrival + arrival delay - taxi-in

df$ts_WHON<-strptime(df$WHEELS_ON,"%Y-%m-%d %H",tz="America/New_York")
df$ts_WHON<-as.POSIXct(df$ts_WHON,tz="America/New_York")
edct.rel<-twang.edct.effarr[,c(1,20,21,22,24,26,28,29,30)]
colnames(edct.rel)[1]<-"ts_WHON"
df.q<-merge(df,edct.rel,by.x="ts_WHON",all.x=TRUE)

#now try to determine a threshold for Percent.EDCT and Subset.Avg.EDCT 
#that would identify flights that actually recieved such

ggplot(df.q,aes(Subset.Avg.EDCT)) + geom_histogram(binwidth=1) +facet_grid(status ~ .,scales = "free_y")
