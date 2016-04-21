# This is an R script for selecting features from TAF data from the New York area.
# Script author: Kenneth Kuhn, Akhil Shah
# Last modified: 4/16/2016
# we are not going to block summarize but extract hourly features.


# Point out the directories for the input and feature data
new_TAF_dir = "/Users/ashah/NoBackup/code/nasa/data/TAF/NY_TAF/"
feature_dir = "/Users/ashah/NoBackup/code/nasa/src/causalInfFAA/data/"

# Create blank vectors for storing key data found in TAFs.  Key data include:
# Date and time TAF issued, forecast start and end times of weather, cloud ceilings, visibility,
# wind speeds, directions, and gusts, and presence or absence of snow, thunderstorms, rain, and fog.
date_v = c()
airport_v = c()
time_v = c()
fore_time_start_v = c()
fore_time_end_v = c()
windspeed_v = c()
winddir_v = c()
windgust_v = c()
visibility_v = c()
snow_v = c()
TS_v = c()
rain_v = c()
fog_v = c()

# Cycle through all relevant years, months, and days
for (years in 2012:2015) {
	for (months in 1:12) {
		for (days in 1:31) {
			# If a file exists, open a connection
			fname = paste(new_TAF_dir,"TAF_",years,"_",months,"_",days,".txt",sep="")
			if (file.exists(fname)) {
				con = file(fname,open="r")
				# Keep track of the line type, 1-date,time, 2-TAF,KJFK,etc., 3-FM,TEMPO,etc.
				line_type = 1
				# Cycle through the lines of the file
				while (length(oneLine<-readLines(con,n=1,warn=FALSE))>0) {
					line_vector = strsplit(oneLine," ")
					line_len = length(line_vector[[1]])
					if (line_type==1) {
						line_type = 2
					} else if (line_type==2) {
						# We should be in a line that starts with TAF and includes a lot of relevant data
						if (line_len>5) {
							date_v = c(date_v,paste(months,days,years,sep="/"))
							airport_v = c(airport_v,line_vector[[1]][2])
							time_v = c(time_v,substr(line_vector[[1]][3],3,6))
							forecast_times = strsplit(line_vector[[1]][4],"/")
							fore_time_start_v = c(fore_time_start_v,forecast_times[[1]][1])
							fore_time_end_v = c(fore_time_end_v,forecast_times[[1]][2])
							if (substr(line_vector[[1]][5],1,3)=="VRB") {
								windspeed_v = c(windspeed_v,substr(line_vector[[1]][5],4,5))
								winddir_v = c(winddir_v,NA)
								if (substr(line_vector[[1]][5],6,6)=="G") {
									windgust_v = c(windgust_v,substr(line_vector[[1]][5],7,8))
								} else {
									windgust_v = c(windgust_v,NA)
								}
							} else {
								if (substr(line_vector[[1]][5],6,7)=="KT") {
									windspeed_v = c(windspeed_v,substr(line_vector[[1]][5],4,5))
									winddir_v = c(winddir_v,substr(line_vector[[1]][5],1,3))
									windgust_v = c(windgust_v,NA)
								} else if (substr(line_vector[[1]][5],6,6)=="G") {
									windspeed_v = c(windspeed_v,substr(line_vector[[1]][5],4,5))
									winddir_v = c(winddir_v,substr(line_vector[[1]][5],1,3))
									windgust_v = c(windgust_v,substr(line_vector[[1]][5],7,8))
								} else {
									windspeed_v = c(windspeed_v,NA)
									winddir_v = c(winddir_v,NA)
									windgust_v = c(windgust_v,NA)
								}
							}
							visibility_v = c(visibility_v,line_vector[[1]][6])
							if (line_len<7) {
								snow_v = c(snow_v,FALSE)
								TS_v = c(TS_v,FALSE)
								rain_v = c(rain_v,FALSE)
								fog_v = c(fog_v,FALSE)
							} else {
								how_many = sum(grepl("SN",line_vector[[1]][7:line_len]))
								if (how_many>0) {
									snow_v = c(snow_v,TRUE)
								} else {
									snow_v = c(snow_v,FALSE)
								}
								how_many = sum(grepl("TS",line_vector[[1]][7:line_len]))
								if (how_many>0) {
									TS_v = c(TS_v,TRUE)
								} else {
									TS_v = c(TS_v,FALSE)
								}
								how_many = sum(grepl("RA",line_vector[[1]][7:line_len]))
								if (how_many>0) {
									rain_v = c(rain_v,TRUE)
								} else {
									rain_v = c(rain_v,FALSE)
								}
								how_many = sum(grepl("FG",line_vector[[1]][7:line_len]))
								if (how_many>0) {
									fog_v = c(fog_v,TRUE)
								} else {
									fog_v = c(fog_v,FALSE)
								}
							}
						}
						line_type = 3
					} else if (line_type==3) {
						# We should be in a line that is either blank (separating TAFs) or contains FM, TEMPO, or BECMG data
						if(line_len==0) {
							line_type = 1
						}
					}
				}
			}
		}
	}
}

# Put the TAF data into a data frame
TAF_data = data.frame(date=date_v,airport=airport_v,time=time_v,fore_time_start=fore_time_start_v,
	fore_time_end=fore_time_end_v,windspeed=windspeed_v,winddir=winddir_v,windgust=windgust_v,
	visibility=visibility_v,snow=snow_v,TS=TS_v,rain=rain_v,fog=fog_v)
# The forecast start and end times are actually dates and hours but we already have a column keeping track of the date
TAF_data$fore_time_start = substr(as.character(TAF_data$fore_time_start),3,4)
TAF_data$fore_time_end = substr(as.character(TAF_data$fore_time_end),3,4)
# Save the TAF data
save(TAF_data,file=paste(feature_dir,"NY_TAF_data.Rdata",sep=""))
#load("/Volumes/NASA_data/features_data/NY_TAF_data.Rdata")

# Now extract the features from the TAF data frame
# For each airport, during each of 4 blocks of time per day, note: min visibility, presence/absence of snow, TS, and rain.
# For each key runway, during each of 4 blocks of time per day, note: max crosswind.

# Reformat the data
TAF_data$fore_time_start = as.numeric(as.character(TAF_data$fore_time_start))
TAF_data$fore_time_end = as.numeric(as.character(TAF_data$fore_time_end))
TAF_data$windspeed = as.numeric(as.character(TAF_data$windspeed))
TAF_data$windgust = as.numeric(as.character(TAF_data$windgust))
TAF_data$winddir = as.numeric(as.character(TAF_data$winddir))
TAF_data$visibility = as.character(TAF_data$visibility)
TAF_data$visibility[TAF_data$visibility=="1/2SM"] = "0.5"
TAF_data$visibility[TAF_data$visibility=="1/4SM"] = "0.25"
TAF_data$visibility[TAF_data$visibility=="1SM"] = "1"
TAF_data$visibility[TAF_data$visibility=="2SM"] = "2"
TAF_data$visibility[TAF_data$visibility=="3/4SM"] = "0.75"
TAF_data$visibility[TAF_data$visibility=="3SM"] = "3"
TAF_data$visibility[TAF_data$visibility=="4SM"] = "4"
TAF_data$visibility[TAF_data$visibility=="5SM"] = "5"
TAF_data$visibility[TAF_data$visibility=="6SM"] = "6"
TAF_data$visibility[TAF_data$visibility=="P6SM"] = "10"
TAF_data$visibility = as.numeric(TAF_data$visibility)



# # For winds, first transform the wind data to get the crosswind at each runway type
# # (all key NY area airports are either 13/31 or 4/22)
max_wind = apply(cbind(TAF_data.jfk$windspeed,TAF_data.jfk$windgust),1,max,na.rm=TRUE)
# rwy4_cross = c(rep(-99,length(TAF_data.jfk$windspeed)))
rwy13_cross = c(rep(-99,length(TAF_data.jfk$windspeed)))
cur_dir = is.na(TAF_data.jfk$winddir)
# rwy4_cross[cur_dir] = NA
rwy13_cross[cur_dir] = NA
cur_dir = which(TAF_data.jfk$winddir<41)
# rwy4_cross[cur_dir] = sin((40-TAF_data.jfk$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((130-TAF_data.jfk$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data.jfk$winddir>40 & TAF_data.jfk$winddir<131)
# rwy4_cross[cur_dir] = sin((TAF_data.jfk$winddir[cur_dir]-40)*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((130-TAF_data.jfk$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data.jfk$winddir>130 & TAF_data.jfk$winddir<221)
# rwy4_cross[cur_dir] = sin((220-TAF_data.jfk$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((TAF_data.jfk$winddir[cur_dir]-130)*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data.jfk$winddir>220 & TAF_data.jfk$winddir<311)
# rwy4_cross[cur_dir] = sin((TAF_data.jfk$winddir[cur_dir]-220)*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((310-TAF_data.jfk$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data.jfk$winddir>310)
# rwy4_cross[cur_dir] = sin((TAF_data.jfk$winddir[cur_dir]-220)*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((TAF_data.jfk$winddir[cur_dir]-310)*(pi/180))*max_wind[cur_dir]

#Add crosswinds to our reduced JFK data
TAF_data.jfk$crosswinds<-rwy13_cross


#create unique entries for forecasts ending at 6,12,18,24 hours
rel_dates = unique(TAF_data.jfk$date)
n_dates = length(rel_dates)
TAF_data.jfk.6h<-data.frame()
TAF_data.jfk.12h<-data.frame()
TAF_data.jfk.18h<-data.frame()
TAF_data.jfk.24h<-data.frame()
for (date_ind in 1:n_dates){TAF_data.jfk.6h<-rbind(TAF_data.jfk.6h,
	unique(subset(TAF_data.jfk,date==rel_dates[date_ind] & fore_time_end==6)))}
for (date_ind in 1:n_dates){TAF_data.jfk.12h<-rbind(TAF_data.jfk.12h,
	unique(subset(TAF_data.jfk,date==rel_dates[date_ind] & fore_time_end==12)))}
for (date_ind in 1:n_dates){TAF_data.jfk.18h<-rbind(TAF_data.jfk.18h,
	unique(subset(TAF_data.jfk,date==rel_dates[date_ind] & fore_time_end==18)))}
for (date_ind in 1:n_dates){TAF_data.jfk.24h<-rbind(TAF_data.jfk.24h,
	unique(subset(TAF_data.jfk,date==rel_dates[date_ind] & fore_time_end==24)))}

#collapse into a single dataframe
TAF_data.jfk.unique<-rbind(TAF_data.jfk.6h,TAF_data.jfk.12h,TAF_data.jfk.18h,TAF_data.jfk.24h)
#sort by date, then by fore_time_end, then by fore_time_start
TAF_data.jfk.unique$date<-as.Date(TAF_data.jfk.unique$date,"%m/%d/%Y")
TAF_data.jfk.unique.sorted<-TAF_data.jfk.unique[order(TAF_data.jfk.unique$date,
	TAF_data.jfk.unique$fore_time_end,TAF_data.jfk.unique$fore_time_start),]

#repeat TAF to get forecasts for each hour



df_out=data.frame()
tafseries<-function(df,row){

	if (row < (dim(df)[1])) {
		if (df[row,]$fore_time_end==df[row+1,]$fore_time_end){
			last_hour=min(df[row,]$fore_time_end,df[row+1,]$fore_time_start)-1
		}
		else{last_hour=df[row,]$fore_time_end}
	}
	else{last_hour=df[row,]$fore_time_end}
	
	hours=seq(df[row,]$fore_time_start,last_hour-1)
	df_repeated=do.call(rbind,replicate(length(hours),df[row,],simplify=FALSE))
	df_repeated$hours<-hours
	df_repeated$timestamp<-strptime(paste(rep(df_repeated[row,]$date,length(hours)),hours),format="%Y-%m-%d %H",tz="America/New_York")
	df_repeated
}
numrows=dim(TAF_data.jfk.unique.sorted)[1]
repList<-lapply(seq(numrows),function(x){tafseries(TAF_data.jfk.unique.sorted,x)})
df_out=do.call(rbind,repList)
ts<-strptime(paste(df_out$date,df_out$hours),format="%Y-%m-%d %H",tz="America/New_York")
df_out$timestamp<-ts
save(df_out,file=paste(feature_dir,"JFK_TAF_timestamped_data.Rdata",sep=""))