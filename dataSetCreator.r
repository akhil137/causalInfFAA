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



#Example merge metar with aspm, so that perhaps later we can merge with twangdf above
#merge with metar determining which rows survive and sort on timestamp
#metar has less rows 
metar_aspm<-merge(metar_deduped,aspm_deduped,by="timestamp",all.x=TRUE,sort=TRUE)
