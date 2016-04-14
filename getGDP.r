#-----------------------------------------------------#
#--------EXTRACT GDP Advisory data----------------#
#-----------------------------------------------------#
#Note that times below coded with POSIXct (since unix epoch time)
#which seems to do time converstions correctly
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
	"Is.RootAdvisory",
	"Advisory.Text"
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
	gdp.Advisory.Text<-dat.gdp.ap$Advisory.Text
	#output a list of these objects
	list("advisoryDate"=gdp.adv.date,
		"sendTime"=gdp.send.time,
		"startTime"=gdp.start.time,
		"endTime"=gdp.end.time,
		"status"=gdp.status,
		"advNum"=gdp.RootAdvisoryNumber,
		"rootAdv"=gdp.RootAdvisory,
		"reason"=gdp.Advisory.Text
		)
}
