library(ggplot2)
#catch all data frame for plots
df<-aspmFlights


#summary stats
library(dplyr)
library(magrittr)
compare_mean<-aspmFlights %>%
group_by(treat) %>%
summarise(MeanABDiff=mean(AirborneDiff),MeanABDelay=mean(ABDelay))



#faceted hitograms of ABDiff prior to ps weighting
p.abdiff.unw<-ggplot(df,aes(AirborneDiff)) + geom_histogram(binwidth=1) 


p.abdiff.unw+ggtitle("Airborne Delay for non-GDP (0) and GDP (1) Flights")+ 
xlab("(Observed - Scheduled) Airborne Time Difference [mins]") +
facet_grid(treat ~ .,scales = "free_y") + 
xlim(-100,100) +
geom_vline(data=compare_mean,aes(xintercept=MeanABDiff),linetype="dashed")


#faceted hist for ABDelay prior to weighting
p.abdiff.unw<-ggplot(df,aes(ABDelay)) + geom_histogram(binwidth=1) 


p.abdiff.unw+ggtitle("Airborne Delay for non-GDP (0) and GDP (1) Flights")+ 
xlab("Airborne Delay[mins]") +
facet_grid(treat ~ .,scales = "free_y") + 
#note xlim cutoff most flights which have negative or zero difference
xlim(0,50) +
ylim(0,1000) +
geom_vline(data=compare_mean,aes(xintercept=MeanABDelay), linetype="dashed")+
geom_text(data=compare_mean,
	aes(x=MeanABDelay, label=paste("Mean = ",round(MeanABDelay,1),"mins"), y=700), 
	colour="black", angle=90, vjust = 1.5)

#overlay histograms of abdelay unw
ggplot(aspmFlights,aes(x=ABDelay,fill=as.factor(treat))) + 
geom_histogram(position="dodge",binwidth=1)+
xlim(0,50)+ylim(0,500)+
scale_fill_discrete(name="GDP flight")+
labs(title="Airborne Delay", x="Delay [Mins]")+
theme_minimal()

#weighted outcomes; below definition of weights works as checked below
nt<-table(aspmFlights$treat)[["1"]]
nc<-table(aspmFlights$treat)[["0"]]
w<-aspmFlights$cbps*(aspmFlights$treat*nt + nc*(1-aspmFlights$treat))

aspmFlights$WeightedABDelay<-w*aspmFlights$ABDelay
compare_mean<-aspmFlights %>%
group_by(treat) %>%
summarise(MeanABDiff=mean(AirborneDiff),MeanABDelay=mean(ABDelay),
	MeanWABDelay=mean(WeightedABDelay))

#after weighting
# > head(compare_mean)
# # A tibble: 2 × 4
#   treat MeanABDiff MeanABDelay MeanWABDelay
#   <int>      <dbl>       <dbl>        <dbl>
# 1     0  -5.238506    5.145833     6.559061
# 2     1   2.089644    7.784434     7.784434

