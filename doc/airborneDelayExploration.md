explore the reasons for large airborne delay
aspm.lga<-read.csv("./data/LGA_aspm.csv")
metar.lga<-read.csv("./data/LGA_metar.csv")

*from twangDataCreation.r*
metar$timestamp<-strptime(paste(metar$date,metar$Hour),"%Y-%m-%d %H",tz="America/New_York")
drop duplicated hour metar obs
metar_deduped<-metar[!duplicated(metar$timestamp),]

aspm$timestamp<-strptime(paste(aspm$Date,aspm$Hour),"%m/%d/%Y %H", tz="America/New_York")
aspm_deduped<-aspm[!duplicated(aspm$timestamp),]

metar_aspm<-merge(metar_deduped,aspm_deduped,by="timestamp",all.x=TRUE,sort=TRUE)

metar_aspm<-na.omit(metar_aspm)

keeps=c("timestamp","VisibilityMPH","Wind_SpeedMPH","PrecipitationIn","Conditions","A","AirDelay")
metar_aspm<-metar_aspm[keeps]
*---*

*get pm 5 hours from largest airborne delays (in 3rd quartile)*

> summary(metar_aspm$AirDelay)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-927.000    0.000    2.310    2.407    4.880  115.000 

*there are 315 (of approx 30k) instances of larger than 20 mins airborne delay*
idx_largeDelay20mins<-which(metar_aspm$AirDelay>20)

subdf_largeDelay20mins<-lapply(seq(1:length(idx_largeDelay20mins)),function(x){metar_aspm[seq(idx_largeDelay20mins[x]-5,idx_largeDelay20mins[x]+5),]})

*extract reason for advisory, e.g. impacting condition*

ap<-"JFK"
airport<-paste(ap,"/ZNY",sep="")

advy.path<-"~/NoBackup/code/nasa/data/advy_2010_2014"

filelist<-dir(advy.path,pattern="*.csv")
filepaths<-unlist(lapply(filelist,function(x){file.path(advy.path,x)}))

gdp.ewr.list<-lapply(c(1:20),function(x){data.frame(getGDP(filepaths[x],airport))})
gdp.ewr<-do.call(rbind,gdp.ewr.list)

gdponly<-gdp.ewr[gdp.ewr$status=="GDP",]
*nope*
extractor<-function(x){s<-gdponly$reason[x]; regmatches(s,gregexpr("(?<=CONDITION\\:).*(?=\\|C{1})",s,perl=TRUE))}
*yup*
extractor<-function(x){s<-gdponly$reason[x]; regmatches(s,gregexpr("(?<=CONDITION\\:)[^\\|]+",s,perl=TRUE))}

reasons<-unlist(lapply(seq(1:length(gdponly$reason)),extractor))

*category and details of reasons*
reasons.cat<-unlist(lapply(seq(1,length(reasons)),function(x){strsplit(reasons[x],"/")[[1]][1]}))
reasons.detail<-unlist(lapply(seq(1,length(reasons)),function(x){strsplit(reasons[x],"/")[[1]][2]}))

*table these*

for lga
% latex table generated in R 3.1.0 by xtable 1.7-4 package
% Wed Mar  2 16:37:28 2016
\begin{table}[ht]
\centering
\begin{tabular}{rr}
  \hline
 & reasons.detail \\ 
  \hline
 AIR SHOW &   1 \\ 
   COMPACTED DEMAND &   5 \\ 
   CONSTRUCTION &   8 \\ 
   DISABLED AIRCRAFT &   1 \\ 
   EMERGENCY &   2 \\ 
   FOG &   4 \\ 
   LOW CEILINGS & 376 \\ 
   LOW VISIBILITY &  33 \\ 
   MAINTENANCE &   2 \\ 
   MULTI-TAXI &   1 \\ 
   OTHER &  10 \\ 
   OUTAGE &  12 \\ 
   RAIN &   9 \\ 
   RUNWAY TREATMENT &   3 \\ 
   SNOW-ICE &  49 \\ 
   THUNDERSTORMS & 305 \\ 
   TORNADO-HURRICANE &   3 \\ 
   VOLUME &  11 \\ 
   WIND & 288 \\ 
   \hline
\end{tabular}
\end{table}

% latex table generated in R 3.1.0 by xtable 1.7-4 package
% Wed Mar  2 16:38:08 2016
\begin{table}[ht]
\centering
\begin{tabular}{rr}
  \hline
 & reasons.cat \\ 
  \hline
 EQUIPMENT  &  12 \\ 
   OTHER  &  13 \\ 
   RWY-TAXI  &  11 \\ 
   VOLUME  &  17 \\ 
   WEATHER &   1 \\ 
   WEATHER  & 1069 \\ 
   \hline
\end{tabular}
\end{table}

*lets select only root adv*
gdponlyroot<-gdponly[gdponly$rootAdv==1,]
*consider those root adv that start before the adv is sent!!*
gdp.backdated<-gdponlyroot[gdponlyroot$startTime-gdponlyroot$sendTime<0,]



extractor<-function(x){s<-gdp.backdated$reason[x]; regmatches(s,gregexpr("(?<=CONDITION\\:)[^\\|]+",s,perl=TRUE))}

reasons<-unlist(lapply(seq(1:length(gdp.backdated$reason)),extractor)) 

gdp.backdated$reasonCat<-unlist(lapply(seq(1,length(reasons)),function(x){strsplit(reasons[x],"/")[[1]][1]}))

gdp.backdated$reasonsDetail<-reasons.detail<-unlist(lapply(seq(1,length(reasons)),function(x){strsplit(reasons[x],"/")[[1]][2]}))

gdp.bd<-gdp.backdated[,-8]

*round to nearest hour then convert to nY time*
gdp.bd$startHour<-round(gdp.bd$startTime,"hour") 
*after formatting, the result is a character*
gdp.bd$NYstartHour<-format(as.POSIXct(gdp.bd$startHour),tz="America/New_York")

*to extract aspm index compare correct tz*
aspm_time_idx<-which(aspm$timestamp==as.POSIXlt(gdp.bd$NYstartHour[1],tz="America/New_York"))

*note it works for the first startHour*
> gdp.bd$NYstartHour[1]
[1] "2010-01-08 16:00:00"
> aspm$timestamp[160]
[1] "2010-01-08 16:00:00 EST"

*--------------*
*--------------*





