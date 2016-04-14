# This is an R script for picking out TAF data from the New York area from a large archive provided by NASA.
# Script author: Kenneth Kuhn
# Last modified: 6/9/2015


# Point out the directories for the raw and new data
raw_TAF_dir = "/Users/ashah/NoBackup/code/nasa/data/TAF/raw_TAF/"
new_TAF_dir = "/Users/ashah/NoBackup/code/nasa/data/TAF/NY_TAF/"

# Point out the possible endings of the raw data files
file_ends = c(".00Z.txt.gz",".06Z.txt.gz",".12Z.txt.gz",".18Z.txt.gz")

# Point ou tht relevant airports
relevant_airports = c("KJFK","KEWR","KLGA")

# Cycle through all relevant years, months, and days
for (years in 2012:2015) {
	for (months in 1:12) {
		for (days in 1:31) {
			# Make a blank variable to save the days data
			days_data = c()
			for (ending in file_ends) {
				# Note the name of the relevant TAF file
				if (months<10) {
					if (days<10) {
						fname = paste(raw_TAF_dir,years,"/0",months,"/0",days,"/taf.",years,"0",months,"0",days,ending,sep="")
					} else {
						fname = paste(raw_TAF_dir,years,"/0",months,"/",days,"/taf.",years,"0",months,days,ending,sep="")
					}
				} else {
					if (days<10) {
						fname = paste(raw_TAF_dir,years,"/",months,"/0",days,"/taf.",years,months,"0",days,ending,sep="")
					} else {
						fname = paste(raw_TAF_dir,years,"/",months,"/",days,"/taf.",years,months,days,ending,sep="")
					}
				}
				# Check if the relevant TAF file exists
				if (file.exists(fname)) {
					# If the file exists, note the lines of the file that pertain to the New York area
					con = file(fname,open="r")
					relevant_lines = c()
					is_NewYork = FALSE
					start_line = -99
					cur_line = 0
					while (length(oneLine<-readLines(con,n=1,warn=FALSE))>0) {
						cur_line = cur_line+1
						line_vector = strsplit(oneLine," ")
						if (is_NewYork) {
							if(length(line_vector[[1]])==0) {
								relevant_lines = c(relevant_lines,c(start_line:cur_line))
								is_NewYork = FALSE
							}
						} else {
							if (length(line_vector[[1]])>1) {
								if (line_vector[[1]][2] %in% relevant_airports) {
									start_line = cur_line-1
									is_NewYork = TRUE
								}
							}
						}
					}
					close(con)
					# Now put all the relevant lines of text into a new variable
					con = file(fname,open="r")
					cur_line = 0
					while (length(oneLine<-readLines(con,n=1,warn=FALSE))>0) {
						cur_line = cur_line+1
						if (cur_line %in% relevant_lines) { days_data=c(days_data,oneLine) }
					}
					close(con)
				}
			}
			# Save all the relevant data from the day in a new file
			if (length(days_data)>0) {
				fname = paste(new_TAF_dir,"TAF_",years,"_",months,"_",days,".txt",sep="")
				output_con = file(fname)
				writeLines(days_data,output_con)
				close(output_con)
			}
		}
	}
}
