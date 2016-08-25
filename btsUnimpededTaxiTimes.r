#GOAL: estimate airborne delay by computing 'nominal' airborne time
#either subtract unimpeded taxi times from scheduled elapsed (block) time
#or use 10th percentile (see Skaltas reference below) of actual airborned time
#as nominal airborne time


#get bts frame created by dataSetCreator.r
df = bts.merged
#relevel
df$DEST<-factor(df$DEST)
df$UNIQUE_CARRIER<-factor(df$UNIQUE_CARRIER)
df$ORIGIN<-factor(df$ORIGIN)

#drop origin airports from BTS df not in ASPM taxi-times db
btsdrops<-setdiff(levels(df$ORIGIN),levels(taxiFull$Airport))
idx<-df$ORIGIN %in% btsdrops
df<-df[!idx,]
df$ORIGIN<-factor(df$ORIGIN)
#test to make sure now all BTS ORIGIN in taxi-times
setdiff(levels(dfsub$ORIGIN),levels(taxi$Airport))

#rename levels in BTS to match ASPM carrier codes 
#(note MQ is Envoy Air which is a regional provider of Americal Airlines)
#easiest to use plyr
library(plyr)
btsCarriers<-c("AA","B6","DL","EV","HA","MQ","UA","US","VX")
#see http://aspmhelp.faa.gov/index.php/ASPM_Carriers\
#also note MQ is Envoy Air which is a regional provider of Americal Airlines)
aspmCarriers<-c("AAL","JBU","DAL","BTA","HAL","AAL","UAL","USA","VRD")
df$UNIQUE_CARRIER<-mapvalues(df$UNIQUE_CARRIER,from=btsCarriers,to=aspmCarriers)

#now drop carriers 
#note that we don't seem to have taxi times for BTA i.e. expressjet, although a ASPM carrier
btsdrops<-setdiff(levels(df$UNIQUE_CARRIER),levels(taxiFull$Carrier))
idx<-df$UNIQUE_CARRIER %in% btsdrops
df<-df[!idx,]
df$UNIQUE_CARRIER<-factor(df$UNIQUE_CARRIER)

taxitimes<-function(i){
	#extract vars
	carrier = as.character(df$UNIQUE_CARRIER[i])
	orig = as.character(df$ORIGIN[i])
	#only one destination, JFK, which we have, so all taxiIn times should exist
	dest = as.character(df$DEST[i])
	#average over years 
	taxiIn = mean(taxi[(taxi$Airport==dest) & (taxi$Carrier==carrier),"UnimpededIn"])
	taxiOut = mean(taxi[(taxi$Airport==orig) & (taxi$Carrier==carrier),"UnimpededOut"])
	list(taxiIn,taxiOut)
}
#get taxi-times
tt<-sapply(1:dim(df)[1],taxitimes)
ttin<-unlist(tt[1,])
ttout<-unlist(tt[2,])

#add to df as UnimpededTaxiOut and UnimpededTaxiIn
df$UnimpededTaxiIn<-ttin
df$UnimpededTaxiOut<-ttout

#we can check difference between unimpeded and observed in BTS
# > summary(abs(df$UnimpededTaxiIn-df$TAXI_IN))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   1.200   2.650   4.229   4.800  82.850      31 
# > summary(abs(df$UnimpededTaxiOut-df$TAXI_OUT))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   1.400   3.400   6.303   7.750 124.000      57 
#also check ratio of sum of unimpeded to total elapsed
# > summary(abs(df$UnimpededTaxiOut+df$UnimpededTaxiOut)/df$CRS_ELAPSED_TIME)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.03391 0.08500 0.11390 0.14190 0.16820 0.44260      57 

#compute 'unimpeded' or nominal airborne times (expected enroute times)
#note however that the scheduled elapsed time includes a buffer
unimpab<-df$CRS_ELAPSED_TIME-(df$UnimpededTaxiOut+df$UnimpededTaxiOut)

#compute airborne delay 
#(observed-scheduled; negative delays imply less ab time than expected)
abdelay<-df$AIR_TIME-unimpab

#note it seems like most of the delay is in taxiing (and gate push) not airborne time
# > summary(abdelay)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -47.100 -13.500  -6.100  -5.672   1.000  92.100      57 
# > summary(df$TAXI_IN-df$UnimpededTaxiIn)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -6.650  -1.500   0.850   2.516   4.350  82.850      31 
# > summary(df$TAXI_OUT-df$UnimpededTaxiOut)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -9.600  -0.100   2.950   5.403   7.750 124.000      57 
# > summary(df$ARR_DELAY)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  -57.00  -13.00   -2.00   16.61   22.00  899.00 

#can plot abdelay dist as function of gdp status (many not actually be a gdp flight though)
library(ggplot2)
ggplot(df,aes(ABDelay)) + geom_histogram(binwidth=1) +facet_grid(status ~ .,scales = "free_y")


#From the Skaltas thesis, determine buffer (schedule padding)
#buffer = schduled block time - actual block time 
#where block is elapsed time in BTS (e.g. taxi out + air time + taxi in)
#nominal air time is defined as the 10th percentile of actual air time
#These should be determined per carrier and per route (origin-dest) pair
#For example let's try UAL from lax to jfk
#we can see all posisbilities of carrier and orig-dest pairs via
table(df$ORIGIN,df$UNIQUE_CARRIER)
lax.ual<-df[df$UNIQUE_CARRIER=="UAL",]
lax.ual<-lax.ual[lax.ual$ORIGIN=="LAX",]
#observed, actual block time
lax.ual$bt<-lax.ual$AIR_TIME+lax.ual$TAXI_OUT+lax.ual$TAXI_IN
#nominal block time - notice these are all constants for a fixed carrier-route pair
lax.ual$nbt<-lax.ual$UnimpededTaxiIn+lax.ual$UnimpededTaxiOut+quantile(lax.ual$AIR_TIME,0.1,names=FALSE)
#The buffer is then
lax.ual$buffer<-lax.ual$bt-lax.ual$nbt
ggplot(lax.ual,aes(ABDelay+buffer)) + geom_histogram(binwidth=1) +facet_grid(status ~ .,scales = "free_y")
#however we should really use a mean buffer time, but above is has sd ~ mean
