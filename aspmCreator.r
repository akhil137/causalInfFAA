ap="LGA"

aspm<-read.csv(paste(ap,"_aspm.csv",sep=""))
aspm2014<-read.csv(paste(ap,"_aspm_2014.csv",sep=""))
aspm<-rbind(aspm,aspm2014)
aspm$timestamp<-strptime(paste(aspm$Date,aspm$Hour),"%m/%d/%Y %H", tz="America/New_York")
aspm_deduped<-aspm[!duplicated(aspm$timestamp),]
aspm<-aspm[c("timestamp","A","AirDelay")]
colnames(aspm)<-c("timestamp",paste("A_",ap,sep=""),paste("AirDelay_",ap,sep=""))
