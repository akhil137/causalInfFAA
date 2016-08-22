#The qtr-hour file below most likely comes from teh ASPM data download module
#http://aspmhelp.faa.gov/index.php/ASPM_Data_Download:_Definitions_of_Variables
#/Users/ashah/NoBackup/code/nasa/data/fromAmy/JFK 201405-201408.csv
#after reading in that file as 'hansen', let's parse the following relevant columns
#note the fileds are defined in 'ASPM Airport Quarter Hour Data Dictionary'
#aspmdetdwn<-hansen[,c(2,3,4,5,6,7,55,56,57,44,46,49,50,62,64,66,68,69,70,71,72,73,74,75,77,79,81,83)]
#Whereas, this file, also qtr-hour but less fields, may come from ASPM efficiency module?
amy<-read.csv("../../data/fromAmy/JFK.csv")
amy$timestamp<-strptime(paste(as.character(amy$YYYYMM),as.character(amy$DAY),as.character(amy$HOUR),sep=":"),format="%Y%m:%d:%H",tz="America/New_York")
#amy<-amy[,c("timestamp","EFFARR","ARRDEMAND","CEILING","VISIBILITY","TEMP","WINDSPEED")]

qdat<-amy[,c("timestamp","QTR","EFFARR","ARRDEMAND")]
#here's some bullshit to aggregate to hour level
library(reshape)

mdat<-melt(qdat,id=c("timestamp","QTR"))
#cast doesn't like actual posix time stamps, so we need to convert to factor
tmp<-as.character(mdat$timestamp)
tmp2<-as.factor(tmp)
mdat$fac<-tmp2
hourly<-cast(mdat,fac~variable,sum)
hourly$qlength<-(hourly$ARRDEMAND-hourly$EFFARR)
hourly$timestamp<-strptime(hourly$fac,format="%Y-%m-%d %H:%M:%S",tz="America/New_York")
#plot the qlength histogram
library(ggplot2)
ggplot(hourly,aes(x=qlength))+geom_histogram(binwidth=1)+coord_cartesian(xlim=c(1,100),ylim=c(0,10e3))


qdat.hourly<-hourly[,c(4,5)]
feature_dir = "/Users/ashah/NoBackup/code/nasa/src/causalInfFAA/data/"
save(qdat.hourly,file=paste(feature_dir,"JFK_ASPMQueueLength_timestamped_data.Rdata",sep=""))

amy.weather<-amy[,c("VISIBILITY","TEMP","WINDSPEED","WINDANGLE")]
library(plyr)
#to conver factors properly to numeric must convert to char first
amy.weather<-colwise(as.character)(amy.weather)
amy.weather<-colwise(as.numeric)(amy.weather)
amy.weather$CEILING<-amy$CEILING
amy.weather$QTR<-amy$QTR
amy.weather$timestamp<-amy$timestamp

#now get the hourly weather by averaging over quarters
mdat2<-melt(amy.weather,id=c("timestamp","QTR"))
tmp<-as.character(mdat2$timestamp)
tmp2<-as.factor(tmp)
mdat2$fac<-tmp2
hourly.weather<-cast(mdat2,fac~variable,mean)
hourly.weather$timestamp<-strptime(hourly.weather$fac,format="%Y-%m-%d %H:%M:%S",tz="America/New_York")
hourly.weather<-hourly.weather[,-1]

#calculate cross WINDSPEED
cwindfunc<-function(angle,speed){
	if (is.na(angle)) {crosswind=NA}
	else if (angle < 131) {crosswind = sin((130-angle)*(pi/180))*speed}
	else if (angle > 130 & angle < 221) {crosswind = sin((angle-130)*(pi/180))*speed} 
	else if (angle > 220 & angle < 311) {crosswind = sin((310-angle)*(pi/180))*speed}
	else if (angle > 310) {crosswind = sin((angle-310)*(pi/180))*speed}
	round(crosswind,digits=1)
}
hourly.weather$crosswind<-unlist(lapply(seq(length(hourly.weather$WINDANGLE)),
	function(x){cwindfunc(hourly.weather$WINDANGLE[x],hourly.weather$WINDSPEED[x])}))
colnames(hourly.weather)<-c("vis_ASPM","temp_ASPM","windspeed_ASPM","windangle_ASPM","ceil_ASPM","timestamp","crosswind_ASPM")
save(hourly.weather,file=paste(feature_dir,"JFK_ASPMWeather_timestamped_data.Rdata",sep=""))

