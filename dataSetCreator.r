#Create timestamped datasets used for propensity score and other types of analysis

#Queue Length and ASPM weather data (only available for JFK)
#saved "qdat.hourly" dataframe
#as "/Users/ashah/NoBackup/code/nasa/src/causalInfFAA/data/JFK_ASPMQueueLength_timestamped_data.Rdata"
#saved "hourly.weather" as JFK_ASPMWeather_timestamped_data.Rdata
source("./amydata.r")

#TAF at JFK: saved "df_out" data frame 
#as /Users/ashah/NoBackup/code/nasa/src/causalInfFAA/data/JFK_TAF_timestamped_data.Rdata
source("./feature_select_TAF_akhil.r")

#ASPM at JFK
#saved "aspm" dataframe
#as "JFK_ASPMAirborneDelay_timestamped_data.Rdata"
source("./aspmCreator.r")

#METAR at JFK
#saved "metar_deduped" dataframe
#as JFK_METAR_timestamped_data.Rdata
source("./metarCreator.r")

#GDP positive status creator (ensure you have ap set to JFK in the file)
#saved "df3" dataframe
#as JFK_GDPStatus_timestamped_data.Rdata
#NOTE: these are only the instances when there is a GDP
source("./gdpCreator.r")


#TODO - use only complete cases since date ranges for vairious
#datasets only have partial overlap
#merge taf and aspm
taf.aspm<-merge(df_out,aspm,by="timestamp",all.x=TRUE,sort=TRUE)

#merge taf + aspm + amy (qlength)
taf.aspm.amy<-merge(taf.aspm,qdat.hourly,by="timestamp",all.x=TRUE,sort=TRUE)

#merge with tfmi (gdp status)
twangdf<-merge(taf.aspm.amy,df3,by="timestamp",all.x=TRUE,sort=TRUE)
#The hours with NA's should be replaced with the "NO GDP" level
twangdf$status[is.na(twangdf$status)]<-0

#note that there are alot of NA for some of the TAF features
library(plyr)
nmissing <- function(x) sum(is.na(x))
colwise(nmissing)(twangdf)

#keep only some of the columns that seem relevant
twangdf<-twangdf[,c(-2,-3,-4,-5,-6,-8,-9)]

#convert all the various types to numeric
twang.jfk<-colwise(as.numeric)(twangdf[,-1])
twang.jfk$timestamp<-twangdf$timestamp
save(twang.jfk,file=paste(feature_dir,"JFK_twang_timestamped_data.Rdata",sep=""))

#merge with aspm weather
twang.jfk.aspmweather<-merge(twang.jfk,hourly.weather,by="timestamp",all.x=TRUE,sort=TRUE)
save(twang.jfk.aspmweather,file=paste(feature_dir,"JFK_twang_withASPMweather_timestamped_data.Rdata",sep=""))


#only keep non-NA records
twang.complete<-twang.jfk.aspmweather[complete.cases(twang.jfk.aspmweather[,-1]),]
#this messes up the status code
twang.complete$status<-twang.complete$status-1
#Note this results in 7102 observations across 325 days between 9/27/13-8/31/14
#checked with dates<-unique(as.Date(twang.complete$timestamp))
save(twang.complete,file=paste(feature_dir,"JFK_twang_withASPMweather_noNA_timestamped_data.Rdata",sep=""))

#add edct data
edct<-read.csv("./data/aspmEDCTReport.csv")
#the last rows are garbage (could cut out of csv also)
edct<-edct[1:8136,]
edct$timestamp<-strptime(paste(edct$Date,edct$Hour),"%m/%d/%y %H", tz="America/New_York")
#better to use POSIXct for merging
twang.complete$ts<-as.POSIXct(twang.complete$timestamp,tz="America/New_York")
edct$ts<-as.POSIXct(edct$timestamp,tz="America/New_York")
twang.edct<-merge(twang.complete,edct,by="ts",all.x=TRUE)
#get rid of extraneous timestamp columns post merge
twang.edct<-twang.edct[,c(-2,-30)]
save(twang.edct,file=paste("./data/","JFK_twang_withEDCTandASPMweather_noNA_timestamped_data.Rdata",sep=""))

#also added ARRDEMAND and EFFDEMAND from amy's data-set, saved as:
save(twang.edct,file=paste("./data/","JFK_twang_withEDCTandASPMweather_noNA_timestamped_data.Rdata",sep=""))


#BTS data
#manually read in bts data as csv files and filter destination to e.g. JFK
bts<-read.csv("~/Downloads/btsflightlevel/920137515_T_ONTIME.csv")
bts.jfk<-bts[bts$DEST=="JFK",]
#fix scheduled arrival timestamps to be full 24 hour clock
#notice all leading zeros seem to be stripped, so five mins past midnight is "5"

timeFix<-function(idx){
	arrvTime=bts.jfk$CRS_ARR_TIME[idx]
	ncharInArrvTime=nchar(arrvTime)
	numZerosToPaste=4-ncharInArrvTime
	ifelse(ncharInArrvTime<4,paste(paste(rep(0,numZerosToPaste),collapse=""),arrvTime,sep=""),
		arrvTime)
}

bts.jfk$fixedCRSARRTIME<-unlist(sapply(1:nrow(bts.jfk),timeFix))

bts.jfk$CRSARRTIME<-strptime(paste(bts.jfk$FL_DATE,bts.jfk$fixedCRSARRTIME),"%Y-%m-%d %H%M", tz="America/New_York")
#create POSIX-lt timestamp based on scheduled arrivals; only keeping the hour!
bts.jfk$ts<-strptime(paste(bts.jfk$FL_DATE,bts.jfk$fixedCRSARRTIME),"%Y-%m-%d %H", tz="America/New_York")
#conver this to posixCT for easy merging
bts.jfk$ts<-as.POSIXct(bts.jfk$ts,tz="America/New_York")
#we only need releavnt columns (note with scheduled arrival time and the various deltas we can calc all else)
#bts.jfk.rel<-bts.jfk[,c("ts","CRSARRTIME","TAIL_NUM",'UNIQUE_CARRIER', "ORIGIN","DEST","DEP_DELAY","TAXI_OUT","TAXI_IN","ARR_DELAY","AIR_TIME","CRS_ELAPSED_TIME","DISTANCE","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY")]
bts.jfk.rel<-bts.jfk[,c("ts","CRSARRTIME","TAIL_NUM",'UNIQUE_CARRIER', "ORIGIN","DEST","DEP_DELAY","TAXI_OUT","TAXI_IN","ARR_DELAY","AIR_TIME","CRS_ELAPSED_TIME","DISTANCE")]
#add wheels on time to later merge with ASPM hourly data such as arrivals, edct, airborne delay
bts.jfk.rel$WHEELS_ON<-bts.jfk.rel$CRSARRTIME+60*(bts.jfk.rel$ARR_DELAY-bts.jfk.rel$TAXI_IN)
#only keep those that touched down
bts.jfk.rel<-bts.jfk.rel[!is.na(bts.jfk.rel$WHEELS_ON),]

#merge with pretreat covariates (weather and scheduled arrivals)
pretreat<-twang.edct.effarr[,c(1:8,10,13,14:19)]
bts.jfk.rel.pretreat<-merge(bts.jfk.rel,pretreat,by="ts",all.x=TRUE)
#only keep complete cases
bts.merged<-na.omit(bts.jfk.rel.pretreat)
save(bts.merged,file=paste("~/NoBackup/code/nasa/src/causalInfFAA/data/","JFK_twang_withBTSandASPMweather_noNA_timestamped_data.Rdata",sep=""))

#let's keep the ones we think of as gdp flights
bts.gdp<-bts.jfk.rel.pretreat.nona[(bts.jfk.rel.pretreat.nona$DEP_DELAY>0),]
bts.gdp<-bts.gdp[bts.gdp$status==1,]

#add unimpeded taxi times
taxi<-read.csv("~/Downloads/unimpededtaxisubset.csv")
#our bts is only for july 2014 so only keep that summer month season
taxi<-taxi[taxi$Season==3,]

#Example merge metar with aspm, so that perhaps later we can merge with twangdf above
#merge with metar determining which rows survive and sort on timestamp
#metar has less rows 
metar_aspm<-merge(metar_deduped,aspm_deduped,by="timestamp",all.x=TRUE,sort=TRUE)
