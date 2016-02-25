#TAF date overlap with TFMI data is only for:
#9/27/13-12/31/14
#last date of TAF is May 22 2015

#METAR data is from 2010-2013
#TFMI data is from 2010-2014

#TODO: download ASPM data for 2014 


#From airspace report: 48 airport + 16 runway features defined
#4 blocks of time; 3 airports{JFK,EWR,LGA}; 4 runways {JFK1, EWR1, LGA1, LGA2}
# 12: the minimum forecast visibility (at the airports),
# 12: counts of the times thunderstorm activity is forecast (at the airports), 
# 12: counts of the times rain is forecast (at the airports), and
# 12: counts of the times snow is forecast (at the airports).
# 16: the maximum forecast crosswind speed, in knots, (at the runways) 

#GOAL: turn into hourly data - mapping time blocks to hours
#There are 4 time blocks each with 3 hours
#Thus number of hourly features for each AP will be 5 (and 6 for LGA)

#Ken stored data in a csv TAF expert features file
taf_feat_file<-"~/NoBackup/code/nasa/data/TAF/NY_TAF_expert.csv"
taf<-read.csv(taf_feat_file)

#The "date" column is the first followed by the 64 features above
#the time block of a column is appended by "_1" for first, etc., up to "_4"
#Here is the assumed hourly "time map" (in report it says from 10-22 GMT, so we'll say start 
#times from 10 to 21 for deriving hours)

first_start_time<-10
last_start_time<-21
num_blocks<-4

#each column is vector of sequential start times (col1 - 10,11,12)
start_times<-matrix(c(first_start_time:last_start_time),ncol=num_blocks,byrow=FALSE)


#column names extractor - argument is time block number
cn<-function(x){colnames(taf)[grep(paste("_",x,sep=""),colnames(taf))]}
#extract each time block seperately
colnames_time_block<-lapply(seq(num_blocks),cn)

#function to repeat each time-block feature for the jth day
#and store as matrix of nume_repeat-by-16 features
num_repeat<-(last_start_time - first_start_time + 1)/num_blocks
#NOT USED FOR NOW - done by single line below
#tb_mat<-function(j,x){matrix(rep(taf[j,cn(x)],3),nrow=num_repeat,byrow=TRUE)}

#create a list of tb_mat (with num_blocks elements)
#and row-bind them to create a full days (12 hours above)
#DOESN'T WORK
#day_mat<-function(j){do.call(rbind,lapply(seq(num_blocks),tb_mat))}

#create list of day_mat objects
#last day for which we have TFMI data
num_days<-which(taf$date=="12/31/2014")
#DOESN'T WORK
#days_list<-lapply(seq(num_days),day_mat)

#now row bind and make into data frame


#The single liner to do above withouth any function-arg-passing hiccups
days_mat<-do.call(rbind,lapply(seq(num_days),
	function(j){do.call(rbind,lapply(seq(num_blocks),
		function(x){matrix(rep(taf[j,cn(x)],num_repeat),nrow=num_repeat,byrow=TRUE)}))}))



days_frame<-as.data.frame(days_mat)
colnames(days_frame)<-colnames_time_block[[1]]


#create timestamps
ts_raw<-matrix(do.call(cbind,
	lapply(seq(num_days),
		function(x){paste(taf$date[x],c(first_start_time:last_start_time))})),ncol=1)
timestamp<-strptime(ts_raw,format="%m/%d/%Y %H",tz="America/New_York")

#add the timestamp to the feature frame
days_frame$timestamp<-timestamp

#source the gdp creator to make various "df3" and merge
#we can read in various df3 by airport instead

#may need to convert timestamps with as.character prior to merge

twang_taf<-merge(days_frame,df3.jfk,all.x=TRUE)
twang_taf<-merge(twang_taf,df3.ewr,all.x=TRUE)
twang_taf<-merge(twang_taf,df3.lga,all.x=TRUE)

twang_taf$status_JFK[is.na(twang_taf$status_JFK)]<-0
twang_taf$status_LGA[is.na(twang_taf$status_LGA)]<-0
twang_taf$status_EWR[is.na(twang_taf$status_EWR)]<-0
#some random rows for exploratory
twang_taf[sample(dim(twang_taf)[1],6),]
