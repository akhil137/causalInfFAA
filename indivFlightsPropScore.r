#use the following df from btsUnimpededTaxiTimes.r
df = bts.merged
#there are still NA's
pretreatFeats<-c("crosswind_ASPM", "vis_ASPM", "ceil_ASPM", "windspeed_ASPM", 
	"TS", "rain", "A_JFK")
df.pretreat<-df[,pretreatFeats]

#ps analysis
library(twang)
#note there are no instances of snow  or fog, but perhaps include 
#also, note: outcome var may involve abdelay subtractin buffers
#that were computed on subsets of the population (carrier-route)
#should we fit ps on those subsets?

ps.bts<-ps(status ~ crosswind_ASPM + vis_ASPM + ceil_ASPM + windspeed_ASPM +
	TS + rain + A_JFK , data=df,estimand="ATT")