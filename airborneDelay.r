setwd("~/NoBackup/code/nasa/src/causalInfFAA/data")

ap<-"JFK"
aspmFile<-paste(ap,"_aspm.csv",sep="")
aspm<-read.csv(aspmFile)
aspm2014<-read.csv(paste(ap,"_aspm_2014.csv",sep=""))
aspm<-rbind(aspm,aspm2014)
#aspm<-aspm[order(aspm$Date,aspm$Hour),]
aspm$timestamp<-strptime(paste(aspm$Date,aspm$Hour),"%m/%d/%Y %H", tz="America/New_York")
aspm<-aspm[order(as.POSIXct(aspm$timestamp)),]
keeps=c("timestamp","A","AirDelay")
aspm<-aspm[keeps]

#verfiy time ordering of aspm graphically
#library(ggplot2)
#ggplot(aspm,aes(seq(1,nrow(aspm)),timestamp)) + geom_line()

#summary(aspm$AirDelay)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-927.000    0.000    2.310    2.407    4.880  115.000 

#there are 315 (of approx 30k) instances of larger than 20 mins airborne delay
idx_largeDelay20mins<-which(aspm$AirDelay>20)

subdf_largeDelay20mins<-lapply(seq(1:length(idx_largeDelay20mins)),function(x){aspm[seq(idx_largeDelay20mins[x]-5,idx_largeDelay20mins[x]+5),]})

#extract reason for advisory, e.g. impacting condition
airport<-paste(ap,"/ZNY",sep="")

advy.path<-"~/NoBackup/code/nasa/data/advy_2010_2014"

filelist<-dir(advy.path,pattern="*.csv")
filepaths<-unlist(lapply(filelist,function(x){file.path(advy.path,x)}))

#below gdp.ewr is badly named!!! can be any ap defined above
#also need to source twangDataCreation.r or copy and paste getGDP func from it
source("~/NoBackup/code/nasa/src/causalInfFAA/getGDP.r")
gdp.ewr.list<-lapply(c(1:20),function(x){data.frame(getGDP(filepaths[x],airport))})
gdp.ewr<-do.call(rbind,gdp.ewr.list)

gdponly<-gdp.ewr[gdp.ewr$status=="GDP",]

#extract reasons for issuing GDP "impacting condition" category and detail
#not right way to do it:
#extractor<-function(x){s<-gdponly$reason[x]; regmatches(s,gregexpr("(?<=CONDITION\\:).*(?=\\|C{1})",s,perl=TRUE))}
#yup*
extractor<-function(x){s<-gdponly$reason[x]; regmatches(s,gregexpr("(?<=CONDITION\\:)[^\\|]+",s,perl=TRUE))}

reasons.all<-unlist(lapply(seq(1:length(gdponly$reason)),extractor))

#category and details of reasons
reasons.all.cat<-unlist(lapply(seq(1,length(reasons.all)),function(x){strsplit(reasons.all[x],"/")[[1]][1]}))
reasons.all.detail<-unlist(lapply(seq(1,length(reasons.all)),function(x){strsplit(reasons.all[x],"/")[[1]][2]}))


#lets select only root adv*
gdponlyroot<-gdponly[gdponly$rootAdv==1,]


#some histogram plotting
library(ggplot2)
gdponlyroot$backdated<-as.factor(gdponlyroot$backdated
rootTimeliness<-ggplot(gdponlyroot,aes(as.numeric(startTime-sendTime)))

rootTimeliness + xlim(c(-500,1000)) + geom_histogram(binwidth=100) 

rootTimeliness + xlim(c(-500,1000)) + geom_histogram(binwidth=100) + facet_grid(backdated ~ .) +  labs(title="Distribution of BackDated GDP Root Advisories",x="Start-Send time in Minutes")


#consider those root adv that start before the adv is sent!!*
gdp.backdated<-gdponlyroot[gdponlyroot$startTime-gdponlyroot$sendTime<0,]

extractor<-function(x){s<-gdp.backdated$reason[x]; regmatches(s,gregexpr("(?<=CONDITION\\:)[^\\|]+",s,perl=TRUE))}

reasons<-unlist(lapply(seq(1:length(gdp.backdated$reason)),extractor)) 

gdp.backdated$reasonCat<-unlist(lapply(seq(1,length(reasons)),function(x){strsplit(reasons[x],"/")[[1]][1]}))

gdp.backdated$reasonsDetail<-reasons.detail<-unlist(lapply(seq(1,length(reasons)),function(x){strsplit(reasons[x],"/")[[1]][2]}))
#get rid of that bulky full text advisory
gdp.bd<-gdp.backdated[,-8]

#round to nearest hour then convert to nY time*
gdp.bd$startHour<-round(gdp.bd$startTime,"hour") 
#after formatting, the result is a character*
gdp.bd$NYstartHour<-format(as.POSIXct(gdp.bd$startHour),tz="America/New_York")

#to extract aspm index compare correct tz*
#we're going to cycle thru each gdp.bd start hour in NY time zone
#and find the the aspm index which corresponds to it by matchgin the 
#aspm timestamp
aspm_time_idx<-unlist(lapply(seq(1, length(gdp.bd$NYstartHour)),function(x){
	which(aspm$timestamp==as.POSIXlt(gdp.bd$NYstartHour[x],tz="America/New_York"))}))
#note it works for the first startHour*
gdp.bd$NYstartHour[1]
#[1] "2010-01-08 16:00:00"
aspm$timestamp[160]
#[1] "2010-01-08 16:00:00 EST"

#do pm 5 hours from*
pm=5
airdelay<-lapply(seq(1:length(aspm_time_idx)), function(x){
	aspm[seq(aspm_time_idx[x]-pm,aspm_time_idx[x]+pm),]$AirDelay})

library(reshape)
ad<-melt.list(airdelay)
ad$time<-rep(seq(-pm,pm),length(airdelay))
ggplot(ad,aes(x=time,y=value))+geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1)
ggplot(ad,aes(x=time,y=value,group=L1))+geom_line()
ggplot(ad,aes(x=time,y=value,group=L1))+geom_point(alpha=0.3)



ggplot(ad[ad$L1<20,],aes(x=time,y=value,group=L1))+geom_smooth(alpha=0, size=.5,aes(colour=factor(L1)))


ggplot(ad[(ad$L1>5) & (ad$L1<10),],aes(x=time,y=value,group=L1))+geom_smooth(alpha=0.1, size=.5,aes(colour=factor(L1))) + geom_point(aes(colour=factor(L1)))

ggplot(ad[(ad$L1>5) & (ad$L1<10),],aes(x=time,y=value,group=L1))+geom_smooth(alpha=0.1, size=.5,aes(colour=factor(L1))) + geom_point(aes(colour=factor(L1)))


