# This is an R script for selecting features from TAF data from the New York area.
# Script author: Kenneth Kuhn
# Last modified: 6/9/2015


# Point out the directories for the input and feature data
new_TAF_dir = "/Volumes/NASA_data/data_raw/airport_weather/TAF/NY_TAF/"
feature_dir = "/Volumes/NASA_data/features_data/"

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
# Set up blank matrices for visibility, snow, TS, and rain data
rel_dates = unique(TAF_data$date)
n_dates = length(rel_dates)
vis_mat = matrix(0,nrow=n_dates,ncol=12)
snow_mat = matrix(FALSE,nrow=n_dates,ncol=12)
TS_mat = matrix(FALSE,nrow=n_dates,ncol=12)
rain_mat = matrix(FALSE,nrow=n_dates,ncol=12)
# Cycle through each date and fill in the features
for (date_ind in 1:n_dates) {
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KJFK" & TAF_data$fore_time_start<13.5 & TAF_data$fore_time_end>9.5)
	if (length(focus)>0) {
		vis_mat[date_ind,1] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,1] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,1] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,1] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KJFK" & TAF_data$fore_time_start<16.5 & TAF_data$fore_time_end>12.5)
	if (length(focus)>0) {
		vis_mat[date_ind,2] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,2] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,2] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,2] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KJFK" & TAF_data$fore_time_start<19.5 & TAF_data$fore_time_end>15.5)
	if (length(focus)>0) {
		vis_mat[date_ind,3] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,3] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,3] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,3] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KJFK" & TAF_data$fore_time_start<22.5 & TAF_data$fore_time_end>18.5)
	if (length(focus)>0) {
		vis_mat[date_ind,4] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,4] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,4] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,4] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KEWR" & TAF_data$fore_time_start<13.5 & TAF_data$fore_time_end>9.5)
	if (length(focus)>0) {
		vis_mat[date_ind,5] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,5] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,5] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,5] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KEWR" & TAF_data$fore_time_start<16.5 & TAF_data$fore_time_end>12.5)
	if (length(focus)>0) {
		vis_mat[date_ind,6] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,6] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,6] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,6] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KEWR" & TAF_data$fore_time_start<19.5 & TAF_data$fore_time_end>15.5)
	if (length(focus)>0) {
		vis_mat[date_ind,7] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,7] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,7] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,7] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KEWR" & TAF_data$fore_time_start<22.5 & TAF_data$fore_time_end>18.5)
	if (length(focus)>0) {
		vis_mat[date_ind,8] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,8] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,8] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,8] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KLGA" & TAF_data$fore_time_start<13.5 & TAF_data$fore_time_end>9.5)
	if (length(focus)>0) {
		vis_mat[date_ind,9] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,9] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,9] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,9] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KLGA" & TAF_data$fore_time_start<16.5 & TAF_data$fore_time_end>12.5)
	if (length(focus)>0) {
		vis_mat[date_ind,10] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,10] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,10] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,10] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KLGA" & TAF_data$fore_time_start<19.5 & TAF_data$fore_time_end>15.5)
	if (length(focus)>0) {
		vis_mat[date_ind,11] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,11] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,11] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,11] = sum(TAF_data$rain[focus])>0
	}
	focus = which(TAF_data$date==rel_dates[date_ind] & TAF_data$airport=="KLGA" & TAF_data$fore_time_start<22.5 & TAF_data$fore_time_end>18.5)
	if (length(focus)>0) {
		vis_mat[date_ind,12] = min(TAF_data$visibility[focus])
		snow_mat[date_ind,12] = sum(TAF_data$snow[focus])>0
		TS_mat[date_ind,12] = sum(TAF_data$TS[focus])>0
		rain_mat[date_ind,12] = sum(TAF_data$rain[focus])>0
	}
}
# For winds, first transform the wind data to get the crosswind at each runway type
# (all key NY area airports are either 13/31 or 4/22)
max_wind = apply(cbind(TAF_data$windspeed,TAF_data$windgust),1,max,na.rm=TRUE)
rwy4_cross = c(rep(-99,length(TAF_data$windspeed)))
rwy13_cross = c(rep(-99,length(TAF_data$windspeed)))
cur_dir = is.na(TAF_data$winddir)
rwy4_cross[cur_dir] = NA
rwy13_cross[cur_dir] = NA
cur_dir = which(TAF_data$winddir<41)
rwy4_cross[cur_dir] = sin((40-TAF_data$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((130-TAF_data$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data$winddir>40 & TAF_data$winddir<131)
rwy4_cross[cur_dir] = sin((TAF_data$winddir[cur_dir]-40)*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((130-TAF_data$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data$winddir>130 & TAF_data$winddir<221)
rwy4_cross[cur_dir] = sin((220-TAF_data$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((TAF_data$winddir[cur_dir]-130)*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data$winddir>220 & TAF_data$winddir<311)
rwy4_cross[cur_dir] = sin((TAF_data$winddir[cur_dir]-220)*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((310-TAF_data$winddir[cur_dir])*(pi/180))*max_wind[cur_dir]
cur_dir = which(TAF_data$winddir>310)
rwy4_cross[cur_dir] = sin((TAF_data$winddir[cur_dir]-220)*(pi/180))*max_wind[cur_dir]
rwy13_cross[cur_dir] = sin((TAF_data$winddir[cur_dir]-310)*(pi/180))*max_wind[cur_dir]
# Now note the max at each runway during each day during each block of time
crosswinds = matrix(0,nrow=length(rel_dates),ncol=16)
# Hmm... this is a lot easier with a function.
findwind <- function(TAF_info,date_info,airport_info,time_info,wind_info) {
	focus = which(TAF_info$date==date_info & TAF_info$airport==airport_info & TAF_data$fore_time_start<time_info[2] & TAF_data$fore_time_end>time_info[1])
	if (length(focus)>0) {
		if (sum(is.na(wind_info[focus]))==length(wind_info[focus])) {
			cur_wind = NA
		} else {
			cur_wind = max(wind_info[focus],na.rm=TRUE)
		}
	} else {
		cur_wind = NA
	}
	return(cur_wind)
}
for (date_ind in 1:length(rel_dates)) {
	crosswinds[date_ind,1] = findwind(TAF_data,rel_dates[date_ind],"KJFK",c(9.5,13.5),rwy13_cross)
	crosswinds[date_ind,2] = findwind(TAF_data,rel_dates[date_ind],"KJFK",c(12.5,16.5),rwy13_cross)
	crosswinds[date_ind,3] = findwind(TAF_data,rel_dates[date_ind],"KJFK",c(15.5,19.5),rwy13_cross)
	crosswinds[date_ind,4] = findwind(TAF_data,rel_dates[date_ind],"KJFK",c(18.5,22.5),rwy13_cross)
	crosswinds[date_ind,5] = findwind(TAF_data,rel_dates[date_ind],"KEWR",c(9.5,13.5),rwy4_cross)
	crosswinds[date_ind,6] = findwind(TAF_data,rel_dates[date_ind],"KEWR",c(12.5,16.5),rwy4_cross)
	crosswinds[date_ind,7] = findwind(TAF_data,rel_dates[date_ind],"KEWR",c(15.5,19.5),rwy4_cross)
	crosswinds[date_ind,8] = findwind(TAF_data,rel_dates[date_ind],"KEWR",c(18.5,22.5),rwy4_cross)
	crosswinds[date_ind,9] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(9.5,13.5),rwy4_cross)
	crosswinds[date_ind,10] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(12.5,16.5),rwy4_cross)
	crosswinds[date_ind,11] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(15.5,19.5),rwy4_cross)
	crosswinds[date_ind,12] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(18.5,22.5),rwy4_cross)
	crosswinds[date_ind,13] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(9.5,13.5),rwy13_cross)
	crosswinds[date_ind,14] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(12.5,16.5),rwy13_cross)
	crosswinds[date_ind,15] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(15.5,19.5),rwy13_cross)
	crosswinds[date_ind,16] = findwind(TAF_data,rel_dates[date_ind],"KLGA",c(18.5,22.5),rwy13_cross)
}
# Put the resulting data in a data frame and save the data frame
TAF_features = data.frame(rel_dates,crosswinds,vis_mat,snow_mat,TS_mat,rain_mat)
colnames(TAF_features) = c("date","cross_JFK_1","cross_JFK_2","cross_JFK_3","cross_JFK_4",
	"cross_EWR_1","cross_EWR_2","cross_EWR_3","cross_EWR_4",
	"cross_LGAa_1","cross_LGAa_2","cross_LGAa_3","cross_LGAa_4",
	"cross_LGAb_1","cross_LGAb_2","cross_LGAb_3","cross_LGAb_4",
	"vis_JFK_1","vis_JFK_2","vis_JFK_3","vis_JFK_4",
	"vis_EWR_1","vis_EWR_2","vis_EWR_3","vis_EWR_4",
	"vis_LGA_1","vis_LGA_2","vis_LGA_3","vis_LGA_4",
	"snow_JFK_1","snow_JFK_2","snow_JFK_3","snow_JFK_4",
	"snow_EWR_1","snow_EWR_2","snow_EWR_3","snow_EWR_4",
	"snow_LGA_1","snow_LGA_2","snow_LGA_3","snow_LGA_4",
	"TS_JFK_1","TS_JFK_2","TS_JFK_3","TS_JFK_4",
	"TS_EWR_1","TS_EWR_2","TS_EWR_3","TS_EWR_4",
	"TS_LGA_1","TS_LGA_2","TS_LGA_3","TS_LGA_4",
	"rain_JFK_1","rain_JFK_2","rain_JFK_3","rain_JFK_4",
	"rain_EWR_1","rain_EWR_2","rain_EWR_3","rain_EWR_4",
	"rain_LGA_1","rain_LGA_2","rain_LGA_3","rain_LGA_4")
fname = paste(feature_dir,"NY_TAF_expert.csv",sep="")
write.csv(TAF_features,file=fname,row.names=FALSE)



