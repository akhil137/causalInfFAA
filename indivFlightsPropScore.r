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


#use 30min EDCT as cutoff for gdp flights
flights.gdp<-df.q[df.q$status==1 & df.q$DEP_DELAY>30,]
flights.nogdp<-df.q[df.q$status==0 & df.q$DEP_DELAY<=0,]
flights.gdp$ABDelay<-flights.gdp$AIR_TIME-flights.gdp$nomAbTimes
flights.nogdp$ABDelay<-flights.nogdp$AIR_TIME-flights.nogdp$nomAbTimes
flights<-rbind(flights.gdp,flights.nogdp)

#calculate AB delay using scheduled air time ~ CRS Elapsed time - Unimpeded TAxi times 
schedAirTime<-(flights$CRS_ELAPSED_TIME-flights$UnimpededTaxiIn-flights$UnimpededTaxiOut)
obsAirTime<-flights$AIR_TIME[!is.na(schedAirTime)]
schedAirTime<-schedAirTime[!is.na(schedAirTime)]
schedABD<-obsAirTime-schedAirTime
summary(schedABD)
#It's all negative!!  becaused of schedule padding
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-52.350 -17.900 -10.750 -10.450  -3.987  79.300 
#filter for pretreat covars for ps fitting
flights.gdp.pretreat<-flights.gdp[,pretreatFeats]
flights.nogdp.pretreat<-flights.nogdp[,pretreatFeats]
flights.pretreat<-rbind(flights.gdp.pretreat,flights.nogdp.pretreat)
ps.cbps<-CBPS(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM +
  	 A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=flights.pretreat)
bal.cbps<-dx.wts(x=ps.cbps$weights,data=flights.pretreat,
 	vars=colnames(flights.pretreat)[c(-1,-6,-7,-11,-12)],treat.var="status",estimand="ATT")
#pretty good balance after weighting
bal.table(bal.cbps)
#now do outcome analysis
flights$cbps<-ps.cbps$weights
library(survey)
design.bts<-svydesign(ids=~1, weights=~cbps, data=flights)
glm.simple<-svyglm(ABDelay*EFFARR/ARRDEMAND ~ status, design=design.bts)

glm.lm<-lm(ABDelay*EFFARR/ARRDEMAND~ status + crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM + 
	A_JFK + DISTANCE + CRS_ELAPSED_TIME, data=flights)

#a nice outcome metric! efficiency adjust abdelay :)
summary(flights[flights$status==1,]$ABDelay*flights[flights$status==1,]$EFFARR/flights[flights$status==1,]$ARRDEMAND)
summary(flights[flights$status==0,]$ABDelay*flights[flights$status==0,]$EFFARR/flights[flights$status==0,]$ARRDEMAND)


#let's redo this individual flights analysis using data from ASPM module
aspmFlight<-read.csv("/Users/ashah/NoBackup/code/nasa/data/aspmShah/aspmJFKIndivFlights.csv")
aspmFlight$CRSARRTIME<-strptime(paste(aspmFlight$arrv.date,aspmFlight$arrv.hour),"%m/%d/%y %H", tz="America/New_York")
save(aspmFlights,file="~/NoBackup/code/nasa/src/causalInfFAA/data/JFK_aspmIndivFlightsWithPretreat.Rdata")

#overlap between 'status' from pretreat (NTML) and edct wheels off in aspm indiv flights
#pretty good overlap
table(aspmFlights[aspmFlights$EDCTWoff=="",]$status)
#above results in small false positive (gdp when flight isn't)
#    0     1 
# 12359   169 
table(aspmFlights[!aspmFlights$EDCTWoff=="",]$status)
#above results in 1/8th false negatives (no gdp when flight is)
#   0    1 
# 512 4229 
#summar of ab delay

summary(aspmFlights[!aspmFlights$EDCTWoff=="",]$AirborneDiff)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -296.00   -5.00    3.00    2.09   10.00  437.00 
summary(aspmFlights[aspmFlights$EDCTWoff=="",]$AirborneDiff)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -946.000   -7.000    0.000   -5.239    6.000  540.000 

won<-strptime(paste(aspmFlight$actual.wheels.on),"%H:%M", tz="America/New_York")
woff<-strptime(paste(aspmFlight$actual.wheels.off),"%H:%M", tz="America/New_York")
#just use the correct data downloaded again
if1<-read.csv("/Users/ashah/NoBackup/code/nasa/data/aspmShah/ASPMWithABDelay070114To071514.csv")
if2<-read.csv("/Users/ashah/NoBackup/code/nasa/data/aspmShah/ASPMWithABDelay071614To073014.csv")
indivFlights<-rbind(if1,if2)
#get rid of NA cases
#can inspect colwise NA counts using plyr
#library(plyr)
#nmissing<-function(x) sum(is.na(x))
#colwise(nmissing)(indivFlights)
aspmComp<-indivFlights[!is.na(indivFlights$DepHour),]
#add arrival hour
aspmComp$ts<-strptime(paste(aspmComp$ArrvDate,aspmComp$ArrvHour),"%m/%d/%y %H", tz="America/New_York")
#conver this to posixCT for easy merging
aspmComp$ts<-as.POSIXct(aspmComp$ts,tz="America/New_York")
#merge with TAF weather and EFFARR data from original weather+EFFARR sources
load("~/NoBackup/code/nasa/src/causalInfFAA/data/JFK_pretreat.Rdata")
aspmFlights<-merge(aspmComp,pretreat)
#add treatment variable
aspmFlights$treat=as.integer(!aspmFlights$EDCTWoff=="")
#easy now to note confusion matrix with 'status' 
table(aspmFlights$status,aspmFlights$treat)
#estimate ps weights with CBPS
ps.cbps.aspm<-CBPS(treat ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM +
 	 A_JFK + ETE, data=aspmFlights)
#assess balance
pretreatFeats<-c("treat","crosswind_ASPM", "vis_ASPM", "ceil_ASPM", "windspeed_ASPM", 
	"A_JFK","ETE")
library(twang)
bal.cbps.aspm<-dx.wts(x=ps.cbps.aspm$weights,data=aspmFlights[,pretreatFeats],
 	vars=pretreatFeats[-1],treat.var="treat",estimand="ATT")
#shows much better balance after weighting
bal.table(bal.cbps.aspm)
#outcome analysis
aspmFlights$cbps<-ps.cbps.aspm$weights
library(survey)
design.aspm<-svydesign(ids=~1, weights=~cbps, data=aspmFlights)
glm.simple.aspm<-svyglm(AirborneDiff*EFFARR/ARRDEMAND ~ treat, design=design.aspm)

#results
#w/ ps; mean diff of cap-adj abdelay is 3.9mins (w/o ps it is 0.5-(-5.5)~6mins)
#w/o ps; mean diff of AirborneDiff is +2-(-5)~7mins
#w/ ps; mean diff of same is 4.2mins

#using ASPM definition of delay
aspmFlights$ABDelay<-ifelse(aspmFlights$AirborneDiff>0,aspmFlights$AirborneDiff,0)
#difference w/o ps is 2.6 mins while w/ ps is 1.2 mins 
#weighted outcomes; below definition of weights works as checked below
nt<-table(aspmFlights$treat)[["1"]]
nc<-table(aspmFlights$treat)[["0"]]
w<-aspmFlights$cbps*(aspmFlights$treat*nt + nc*(1-aspmFlights$treat))

aspmFlights$WeightedABDelay<-w*aspmFlights$ABDelay