#pick an airport in ZNY; usually {JFK,LGA, EWR})
#NOTE: we use NY timezones explicitly in conversions below!!!
ap<-"JFK"

#-----------------------------------------------------#
#----------READ IN METAR AND ASPM---------------------#
#-----------------------------------------------------#
metarFile<-paste(ap,"_metar.csv",sep="")
aspmFile<-paste(ap,"_aspm.csv",sep="")
metar<-read.csv(metarFile)
aspm<-read.csv(aspmFile)
#create a time-stamp col combining date and hour
metar$timestamp<-strptime(paste(metar$date,metar$Hour),"%Y-%m-%d %H",tz="America/New_York")
#drop duplicated hour metar obs
metar_deduped<-metar[!duplicated(metar$timestamp),]

aspm$timestamp<-strptime(paste(aspm$Date,aspm$Hour),"%m/%d/%Y %H", tz="America/New_York")
aspm_deduped<-aspm[!duplicated(aspm$timestamp),]
#merge with metar determining which rows survive and sort on timestamp
#metar has less rows 
metar_aspm<-merge(metar_deduped,aspm_deduped,by="timestamp",all.x=TRUE,sort=TRUE)

#drop na rows
metar_aspm<-na.omit(metar_aspm)

#keep only certain columns
keeps=c("timestamp","VisibilityMPH","Wind_SpeedMPH","PrecipitationIn","Conditions","A","AirDelay")
metar_aspm<-metar_aspm[keeps]


#-----------------------------------------------------#
#--------DETERMINE GDP START/END TIMES----------------#
#-----------------------------------------------------#

#helper function
getGDP<-function(filename,apZone){
	dat<-read.csv(filename)

	#enumerate columns we care about
	gdpcols<-c("AdvisoryDate.UTC",
	"SendDate.Time.UTC",
	"AdvisoryType",
	"Derived.BgnDate.Time.UTC",
	"Derived.EndDate.Time.UTC",
	"ControlElement",
	"Average.Delay",
	"Maximum.Delay",
	"Delay.Asgmt.Mode",
	"RootAdvisoryDate.UTC",
	"RootAdvisoryNumber",
	"Is.RootAdvisory"
	)

	#subset on these columns
	dat.gdpcols<-dat[,gdpcols]
	
	#subset on "GDP" and "GDP CNX" 
	#derived by unique(dat.gdpcols$AdvisoryType)
	dat.gdp<-dat.gdpcols[dat.gdpcols$AdvisoryType == "GDP" | dat.gdpcols$AdvisoryType == "GDP CNX",]

	#subset on airport/zone
	#for example "EWR/ZNY"
	dat.gdp.ap<-dat.gdp[dat.gdp$ControlElement==apZone,]

	#create vectors of timestamps and GDP values
	gdp.adv.date<-as.POSIXct(dat.gdp.ap$AdvisoryDate.UTC,tz="UTC")
	gdp.send.time<-as.POSIXct(dat.gdp.ap$SendDate.Time.UTC,tz="UTC")
	gdp.start.time<-as.POSIXct(dat.gdp.ap$Derived.BgnDate.Time.UTC,tz="UTC")
	gdp.status<-factor(dat.gdp.ap$AdvisoryType,levels=c("GDP CNX","GDP"),labels=c("CNX","GDP"))
	gdp.end.time<-as.POSIXct(dat.gdp.ap$Derived.EndDate.Time.UTC,tz="UTC")
	gdp.RootAdvisoryNumber<-dat.gdp.ap$RootAdvisoryNumber
	gdp.RootAdvisory<-factor(dat.gdp.ap$Is.RootAdvisory,levels=c("No","Yes"),labels=c(0,1))
	#output a list of these objects
	list("advisoryDate"=gdp.adv.date,
		"sendTime"=gdp.send.time,
		"startTime"=gdp.start.time,
		"endTime"=gdp.end.time,
		"status"=gdp.status,
		"advNum"=gdp.RootAdvisoryNumber,
		"rootAdv"=gdp.RootAdvisory
		)
}


#loop over all advisories for a given airport
airport<-paste(ap,"/ZNY",sep="")

advy.path<-"~/NoBackup/code/nasa/data/advy_2010_2014"

filelist<-dir(advy.path,pattern="*.csv")
filepaths<-unlist(lapply(filelist,function(x){file.path(advy.path,x)}))

gdp.ewr.list<-lapply(c(1:20),function(x){data.frame(getGDP(filepaths[x],airport))})
gdp.ewr<-do.call(rbind,gdp.ewr.list)

#here's the series to examine and pick out begin/end times
#gdp.ewr[,c("advNum","rootAdv","status","endTime")]

#for every root-advisory:
#look to the next root-advisories previous row
#check if that row is a CNX, and if so, take it's begin time
#but if a GDP

#TO partially test the correctness of this
#compare output to gdp.ewr[gdp.ewr$status=="CNX","startTime"]
#this should match all the root-advisories that are actually CNX'd
#and then one can manually check against the 'hanging chads'
calc_gdp_end_times<-function(gdpDF){
	root_adv_idx<-which(gdpDF$rootAdv==1)
	calc_end_times_for_root_advisories<-function(i){
		#if the GDP does get cancelled
		#use start of cancellation time as end of GDP
		if (gdpDF$status[root_adv_idx[i+1]-1]=="CNX") {
			gdpDF$startTime[root_adv_idx[i+1]-1] #start of CNX time is end of GDP
		}
		#otherwise the initial GDP never got cancelled
		#and use the end time of the latest modification 
		#which would still be a "GDP" status  
		else { 
			gdpDF$endTime[root_adv_idx[i+1]-1]
		}
	}
	#use do.call to unlist nested output from lapply but still
	#preserve POSIXct formatting of timestamps		
	gdp.endtimes<-do.call(c,
		lapply(seq(1:(length(root_adv_idx)-1)),calc_end_times_for_root_advisories))
	
	#add the last root-advisory's end time by checking 
	#the last advisory's status:
	#if it's a CNX or GDP (hanging chad) and taking
	#resp. startTime or endTime
	
	if (tail(gdpDF$status,n=1)=="CNX"){
		.POSIXct(c(gdp.endtimes,tail(gdpDF$startTime,n=1)),tz="UTC")
	}
	else {
		.POSIXct(c(gdp.endtimes,tail(gdpDF$endTime,n=1)),tz="UTC")
	}
	

}
#--------------------------------------------------------------------
#DO NOT DO THIS for calculating start times of GDP
#It seems that modifications of GDP do not affect the start time
#but only the end times
#--------------------------------------------------------------------
# calc_gdp_start_times<-function(gdpDF){
# 	root_adv_idx<-which(gdpDF$rootAdv==1)
# 	calc_start_times_for_root_advisories<-function(i){
# 		#if the GDP gets eventually cancelled
# 		#use start time of last modification as start time
# 		if (gdpDF$status[root_adv_idx[i+1]-1]=="CNX") {
# 			gdpDF$startTime[root_adv_idx[i+1]-2] #starttime of last mod
# 		}
# 		#if the GDP doesn't get cancelled either it gets modified
# 		#or doesn't, but in either case
# 		#use the start time of the advisory right before the next root 
# 		else { 
# 			gdpDF$startTime[root_adv_idx[i+1]-1]
# 		}
# 	}
# 	#use do.call to unlist nested output from lapply but still
# 	#preserve POSIXct formatting of timestamps		
# 	gdp.starttimes<-do.call(c,
# 		lapply(seq(1:(length(root_adv_idx)-1)),calc_start_times_for_root_advisories))
	
# 	#add the last root-advisory's begin time by checking 
# 	#the last advisory's status:
# 	#if its a cancellation, take the penultimate start time (assuming it'll be a GDP)
# 	#edge case: two CNX's in a row (from say an error in TMI report) would mess this up
# 	if (tail(gdpDF$status,n=1)=="CNX"){
# 		.POSIXct(c(gdp.starttimes,head(tail(gdpDF$startTime,n=2),n=1)),tz="UTC")
# 	}
# 	else {
# 		.POSIXct(c(gdp.starttimes,tail(gdpDF$startTime,n=1)),tz="UTC")
# 	}
	
# }
#--------------------------------------------------------------------

#actual end time of a GDP
#assume the begin.date.time of the last mod (hanging chad) or CNX is the actual end time of a GDP
gdp.end.times<-calc_gdp_end_times(gdp.ewr)


#actual begin time of a GDP
#assume its the begin time of the initial GDP
gdp.start.times<-gdp.ewr$startTime[which(gdp.ewr$rootAdv==1)]


#note: negative duration is a sign of early cancellation
gdp.duration<-gdp.end.times-gdp.start.times




makeGDPTimeSeries<-function(gdp){
	
	#create a list of xts objects created above
	#each entry of the list correponds to an xts object
	#and there are as many entries as there are 
	#gdp.time/gdp.end.time values (start/stop times)
	gdplist<-list()

	gdplist<-lapply(seq(length(gdp$startTime)),
					function(x){gdpseq(gdp$startTime[x],
							gdp$endTime[x],
							gdp$status[x])}
					)

	#now when we rbind to get a single time series
	do.call(rbind,gdplist)
}

#helper function
#will return a xts object filling in (by repeating)
#the gdp status for every hour between start/stop times
gdpseq<-function(beginTime,endTime,statusFactor){
	library(xts)	
	tmp<-seq(beginTime,endTime,"hour")
	xts(rep(as.factor(statusFactor),length(tmp)),tmp)
}

#create a dataframe of the begin and end times for each GDP, and "1" indicating
#advisory status = GDP
df<-data.frame(round(gdp.start.times,"hour"),round(gdp.end.times,"hour"),1)
colnames(df)<-c("startTime","endTime","status")

#sometimes gdp.duration < 0: as the GDP was terminated prior to initiation
#thus end time will be earlier than start time
#need to get rid of this before creating an xts time series below
df.posDuration<-df[!(gdp.duration<0),]

#Now create an xts object with intervining hours filled in when GDP enforced
df2<-makeGDPTimeSeries(df.posDuration)

#necessary to turn rownames to timestamp column for latter merging
df3<-data.frame(timestamp=index(df2),status=coredata(df2))


#colnames(df3)<-c("timestamp","status")
#Let's turn these into UTC timestamps then convert to New York timezone
df3$timestamp<-as.POSIXct(df3$timestamp,tz="UTC")
df3$timestamp<-format(df3$timestamp,tz="America/New_York")
#let's create another factor level which will indicate "NO GDP"
df3$status<-factor(df3$status,levels=c(0,1))

#now merge this with the weather and aspm data
#Note METAR is limited to 2013
twangdf<-merge(metar_aspm,df3,all.x=TRUE)
#The hours with NA's should be replaced with the "NO GDP" level
twangdf$status[is.na(twangdf$status)]<-0


#merge with aspm
twang_taf_aspm<-merge(twang_taf,aspm,all.x=TRUE)

#the above data.frame contains lists for various columns so use
twang_taf_aspm_nolists<-data.frame(lapply(twang_taf_aspm,as.character),stringsAsFactors=FALSE)

#extract out individual ap w/o timestamps for easy ps fitting
twang_taf.jfk<-twang_taf_aspm_nolists[,grep("JFK",colnames(twang_taf_aspm_nolists))]
twang_taf.ewr<-twang_taf_aspm_nolists[,grep("EWR",colnames(twang_taf_aspm_nolists))]
twang_taf.lga<-twang_taf_aspm_nolists[,grep("LGA",colnames(twang_taf_aspm_nolists))]

#the above can be written to csv but there are various NA
#we convert to numeric and factor vectors and get rid of records with NA
twang_taf.jfk[,c(1,2,7,8)]<-sapply(twang_taf.jfk[,c(1,2,7,8)],as.numeric)
#below does not work to conver these columns to factors
#twang_taf.jfk[,c(3,4,5,6)]<-sapply(twang_taf.jfk[,c(3,4,5,6)],as.factor)
#use instead
library(plyr)
twang_taf.jfk[,c(3,4,5,6)]<-colwise(as.factor,c(3,4,5,6))(twang_taf.jfk)
#only keep non-NA records
twang_taf.jfk<-twang_taf.jfk[complete.cases(twang_taf.jfk),]
#must convert treatment to numeric for ps() to work in twang
twang_taf.jfk$status_JFK<-as.numeric(twang_taf.jfk$status_JFK)-1

twang_taf.ewr[,c(1,2,7,8)]<-sapply(twang_taf.ewr[,c(1,2,7,8)],as.numeric)
twang_taf.ewr[,c(3,4,5,6)]<-colwise(as.factor,c(3,4,5,6))(twang_taf.ewr)
twang_taf.ewr<-twang_taf.ewr[complete.cases(twang_taf.ewr),]
twang_taf.ewr$status_EWR<-as.numeric(twang_taf.ewr$status_EWR)-1

twang_taf.lga[,c(1,2,3,8,9)]<-sapply(twang_taf.lga[,c(1,2,3,8,9)],as.numeric)
twang_taf.lga[,c(4,5,6,7)]<-colwise(as.factor,c(4,5,6,7))(twang_taf.lga)
twang_taf.lga<-twang_taf.lga[complete.cases(twang_taf.lga),]
twang_taf.lga$status_LGA<-as.numeric(twang_taf.lga$status_LGA)-1