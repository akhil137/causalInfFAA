#load data


#read only clean data by getting rid of last rows (see tail for exact cutoff row numbers)
if1<-read.csv("/Users/ashah/NoBackup/code/nasa/data/aspmShah/ASPMWithABDelay070114To071514.csv",nrow=9085)
if2<-read.csv("/Users/ashah/NoBackup/code/nasa/data/aspmShah/ASPMWithABDelay071614To073014.csv",nrow=8839)

indivFlights<-rbind(if1,if2)

#detail download data from aspm
amy<-read.csv("/Users/ashah/NoBackup/code/nasa/data/fromAmy/JFK.csv")
amy$timestamp<-strptime(paste(as.character(amy$YYYYMM),
	as.character(amy$DAY),as.character(amy$HOUR),
	as.character(15*(amy$QTR-1)),sep=":"),
format="%Y%m:%d:%H:%M",tz="America/New_York")

amy$timestamp<-as.POSIXct(amy$timestamp,tz="America/New_York")

#get only the study period of july 2014 
library(lubridate)
studyperiod<-interval(mdy("07-01-2014",tz="America/New_York"),
	mdy_hms("07-31-2014 23:59:00",tz="America/New_York"))
overlaps<-amy$timestamp %within% studyperiod

amysub<-amy[which(overlaps),]

#calculate crosswind


cwindfunc<-function(angle,speed){
	if (is.na(angle)) {crosswind=NA}
	else if (angle < 131) {crosswind = sin((130-angle)*(pi/180))*speed}
	else if (angle > 130 & angle < 221) {crosswind = sin((angle-130)*(pi/180))*speed} 
	else if (angle > 220 & angle < 311) {crosswind = sin((310-angle)*(pi/180))*speed}
	else if (angle > 310) {crosswind = sin((angle-310)*(pi/180))*speed}
	round(crosswind,digits=1)
}

ws<-as.numeric(as.character(amysub$WINDSPEED))
wa<-as.numeric(as.character(amysub$WINDANGLE))


crosswind<-unlist(lapply(seq(length(wa)),
	function(x){cwindfunc(wa[x],ws[x])}))

#get rid of incompletes (not too many ~ 12)
aspmComp<-indivFlights[!is.na(indivFlights$DepHour),]

#actual wheels on timestamp
wonts<-strptime(paste(aspmComp$ArrvDate,aspmComp$ActWon),
	"%m/%d/%y %H:%M", tz="America/New_York")

#conver this to posixCT for easy merging
wonts<-as.POSIXct(wonts,tz="America/New_York")

#scheduled arrival timestamp
arrvts<-strptime(paste(aspmComp$ArrvDate,aspmComp$ArrvHour),"%m/%d/%y %H", tz="America/New_York")
#conver this to posixCT for easy merging
arrvts<-as.POSIXct(arrvts,tz="America/New_York")

#scheduled departure timestamp
depts<-strptime(paste(aspmComp$DepDate,aspmComp$DepHour),"%m/%d/%y %H", tz="America/New_York")
#conver this to posixCT for easy merging
depts<-as.POSIXct(depts,tz="America/New_York")

#add treatment status: if EDCTWoff is non-empty, then it recieved EDCT (either due to GDP or AFP)
aspmComp$treat=as.integer(!aspmComp$EDCTWoff=="")

#calculate nominable airborne times (copied from btsUnimpededTaxiTimes.r)
library(reshape)
airtime<-aspmComp[,c("Carrier","Dep","ActAirborne","treat")]
mdat<-melt(airtime,id=c("Carrier","Dep","treat"))
#get the 10th percentile for nominal ab time
percentile=0.1 
castAbt<-cast(mdat,Carrier + Dep + treat ~variable,function(x){quantile(x,percentile,names=FALSE)})
#only use non-GDP airborne times to compute stats
castAbt<-castAbt[castAbt$treat==0,]
#since we're dropping edct flights, we lose some carriers and dep airports
#all of these flights recieved edct, and thus we don't know their nomAbtimes
drop1<-which(!is.element(as.character(aspmComp$Carrier),castAbt$Carrier))
drop2<-which(!is.element(as.character(aspmComp$Dep),castAbt$Dep))
drop<-c(drop1,drop2)

aspmComp<-aspmComp[-drop,]

nomAbTimes<-function(i){
	#extract vars
	carrier = as.character(aspmComp$Carrier[i])
	orig = as.character(aspmComp$Dep[i])
	nomAb=ifelse(is.element(carrier,castAbt$Carrier) & is.element(orig,castAbt$Dep),castAbt[(castAbt$Dep==orig) & (castAbt$Carrier==carrier),]$ActAirborne, NA)
}



#compute nominal airborne times
nomAbt<-sapply(1:nrow(aspmComp),nomAbTimes)

abdelay<-aspmComp$ActAirborne-nomAbt


#to access arrival airport features (weather/rwy/demand), use flight departure time 
#and some duration before
featdur=duration(hours=-2)
#create intervals for each flight based on featdur before departure timestamps
featint<-lapply(seq(length(depts)),function(x){int_flip(as.interval(featdur,depts[x]))})
#for each interval in list above, grab rows of amysub which are 
rowidx<-lapply(featint,function(x){which(amysub$timestamp %within% x)})

#capture extremes of features in each 2hr (featdur) interval prior to departure of each flight
#min ceiling
minceil<-sapply(rowidx,function(x){min(amysub$CEILING[unlist(x)])}) 
#min vis
vis<-as.numeric(as.character(amysub$VISIBILITY))
minvis<-sapply(rowidx,function(x){min(vis[unlist(x)])}) 
#max crosswind
maxcw<-sapply(rowidx,function(x){max(crosswind[unlist(x)])}) 
#max wind
maxwind<-sapply(rowidx,function(x){max(ws[unlist(x)])}) 
#max ARRDEMAND
maxarrdem<-sapply(rowidx,function(x){max(amysub$ARRDEMAND[unlist(x)])}) 
#max DEPDEMAND
maxdepdem<-sapply(rowidx,function(x){max(amysub$DEPDEMAND[unlist(x)])}) 
#max (ARRDEMAND-EFFARR)
maxarrq<-sapply(rowidx,function(x){max(amysub$ARRDEMAND[unlist(x)]-amysub$EFFARR[unlist(x)])}) 
#max (DEPDEMAND - EFFDEP)
maxdepq<-sapply(rowidx,function(x){max(amysub$DEPDEMAND[unlist(x)]-amysub$EFFDEP[unlist(x)])}) 
#num of QTR hours of "I" in MC (instrument flight rules)
numQtrI<-sapply(rowidx,function(x){sum(amysub$MC[unlist(x)]=="I")}) 

#other feature that could be used to predict EDCT treatment
#carrier; ETE; arrival and departure hours; dep (origin)

#now compute arrival queue 1hr before wheels on time
outcomedur=duration(hours=-1)
#create intervals for each flight based on featdur before departure timestamps
outcomeint<-lapply(seq(length(wonts)),function(x){int_flip(as.interval(outcomedur,wonts[x]))})
#for each interval in list above, grab rows of amysub which are 
outrowidx<-lapply(outcomeint,function(x){which(amysub$timestamp %within% x)})
maxarrqWON<-sapply(outrowidx,function(x){max(amysub$ARRDEMAND[unlist(x)]-amysub$EFFARR[unlist(x)])}) 
#above difference could be considered the "L" in little's law
#can we use to estimate rho = lambda/mu?
#use quarterly ARRDEMAND to estimate mean arrival rate (ARRDEMAND flights per hour)
#use quarterly EFFARR to estimate mean service rate 


