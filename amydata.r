
amy<-read.csv("../../data/fromAmy/JFK.csv")
amy$ts<-strptime(paste(as.character(amy$YYYYMM),as.character(amy$DAY),as.character(amy$HOUR),sep=":"),format="%Y%m:%d:%H",tz="America/New_York")
qdat<-amy[,c("ts","QTR","EFFARR","ARRDEMAND")]


#here's some bullshit to aggregate to hour level
library(reshape)

mdat<-melt(qdat,id=c("ts","QTR"))
#cast doesn't like actual posix time stamps, so we need to convert to factor
tmp<-as.character(mdat$ts)
tmp2<-as.factor(tmp)
mdat$fac<-tmp2
hourly<-cast(mdat,fac~variable,sum)
hourly$qlength<-(hourly$ARRDEMAND-hourly$EFFARR)
#plot the qlength histogram
library(ggplot2)
ggplot(hourly,aes(x=qlength))+geom_histogram(binwidth=1)+coord_cartesian(xlim=c(1,100),ylim=c(0,10e3))



