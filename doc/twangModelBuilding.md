#METAR based data
*dataset with metar + aspm + tfmi*
To show the variety of data, we have shuffled rows.

| Hour     | VisibilityMPH| Wind_SpeedMPH| PrecipitationIn|Conditions       |  Arrivals| AirDelay|GDP |
|:-----|-------------:|-------------:|---------------:|:----------------|--:|--------:|:------|
|6837  |            10|          11.5|               0|Clear            | 10|    -6.56|0      |
|4252  |            10|           8.1|               0|Mostly Cloudy    | 34|    -0.42|0      |
|19476 |            10|           3.5|               0|Partly Cloudy    | 29|    -3.07|0      |
|17417 |             8|          11.5|               0|Mostly Cloudy    | 33|    11.66|0      |
|25382 |            10|          21.9|               0|Scattered Clouds | 32|     3.74|1      |
|22965 |            10|          11.5|               0|Scattered Clouds |  1|    -8.00|0      |

*Contingency table of Conditions vs GDP treatment indicator at JFK*
```
> setwd("~/NoBackup/code/nasa/src/causalinf/")
> source("twangDataCreation.r")
> library(twang)
> library(knitr)
> kable(table(twangdf$Conditions,twangdf$status),format="markdown")
```

Note there are 34174 records (hours from 2010 to 2013), of which only 2397 have a GDP applied (7.5%).



|      Condition               |    0|   1|
|:-----------------------------|----:|---:|
|Blowing Snow                  |    1|   0|
|Clear                         | 2636|  28|
|Fog                           |  212|  32|
|Haze                          |  201|  29|
|Heavy Rain                    |  100|  24|
|Heavy Snow                    |   11|   0|
|Heavy Thunderstorms and Rain  |   18|  12|
|Ice Pellets                   |    0|   1|
|Light Drizzle                 |  243|  53|
|Light Freezing Drizzle        |   24|   0|
|Light Freezing Rain           |   18|   0|
|Light Ice Pellets             |    8|   6|
|Light Rain                    | 1516| 402|
|Light Rain Showers            |    0|   0|
|Light Snow                    |  424|  47|
|Light Thunderstorms and Rain  |   29|  26|
|Light Thunderstorms and Snow  |    0|   0|
|Mist                          |   14|   0|
|Mostly Cloudy                 | 9954| 714|
|Overcast                      | 5457| 566|
|Partly Cloudy                 | 4328| 112|
|Patches of Fog                |    3|   0|
|Rain                          |  236|  67|
|Scattered Clouds              | 6266| 253|
|Shallow Fog                   |    2|   0|
|Smoke                         |    1|   0|
|Snow                          |   35|   1|
|Squalls                       |    0|   1|
|Thunderstorm                  |   25|  15|
|Thunderstorms and Rain        |    8|   7|
|Thunderstorms with Small Hail |    1|   0|
|Unknown                       |    6|   1|

*create a small sample of gdp/no-gdp rows to feed into twang*
200 gdp samples randomly chosen and 400 no-gdp samples randomly chosen:
twangdf.gdp<-twangdf[twangdf$status==1,]
twangdf.small.gdp<-twangdf.gdp[sample(nrow(twangdf.gdp),size=200),]

twangdf.nogdp<-twangdf[twangdf$status==0,]
twangdf.small.nogdp<-twangdf.nogdp[sample(nrow(twangdf.nogdp),size=400),]
twangdf.small<-rbind(twangdf.small.gdp,twangdf.small.nogdp)
*treatment indicator must be numeric not factor*
twangdf.small$treat<-as.numeric(twangdf.small$status)-1

*run model*
ps.twangdf.small<-ps(treat ~ Conditions + A + VisibilityMPH + Wind_SpeedMPH + PrecipitationIn,data=twangdf.small,estimand="ATT")

*gbm optimization evolution - which iteration to pick*
plot(ps.twangdf.small)

*balance plots*
plot(ps.twangdf.small,plots=1)
plot(ps.twangdf.small,plots=2)
plot(ps.twangdf.small,plots=3)
plot(ps.twangdf.small,plots=4)
plot(ps.twangdf.small,plots=5)
plot(ps.twangdf.small,plots=6)

*cont table for tawandf.small*
kable(table(twangdf.small$Conditions,twangdf.small$treat),format="markdown")

| Conditions                   |   0|  1|
|:-----------------------------|---:|--:|
|Blowing Snow                  |   0|  0|
|Clear                         |  43|  1|
|Fog                           |   2|  2|
|Haze                          |   1|  1|
|Heavy Rain                    |   2|  2|
|Heavy Snow                    |   0|  0|
|Heavy Thunderstorms and Rain  |   0|  2|
|Ice Pellets                   |   0|  0|
|Light Drizzle                 |   3|  3|
|Light Freezing Drizzle        |   0|  0|
|Light Freezing Rain           |   0|  0|
|Light Ice Pellets             |   0|  0|
|Light Rain                    |  25| 32|
|Light Rain Showers            |   0|  0|
|Light Snow                    |   4|  4|
|Light Thunderstorms and Rain  |   1|  2|
|Light Thunderstorms and Snow  |   0|  0|
|Mist                          |   0|  0|
|Mostly Cloudy                 | 119| 64|
|Overcast                      |  65| 42|
|Partly Cloudy                 |  45|  8|
|Patches of Fog                |   0|  0|
|Rain                          |   5|  7|
|Scattered Clouds              |  84| 29|
|Shallow Fog                   |   0|  0|
|Smoke                         |   0|  0|
|Snow                          |   0|  0|
|Squalls                       |   0|  0|
|Thunderstorm                  |   0|  0|
|Thunderstorms and Rain        |   1|  1|
|Thunderstorms with Small Hail |   0|  0|
|Unknown                       |   0|  0|

*create dummy vars*
library(dummies)
twangdf.small.dummy<-dummy('Conditions',data=twangdf.small)
colnames(twangdf.small.dummy)<-make.names(colnames(twangdf.small.dummy))
*dummy coded model*
ps.twangdf.small<-ps(treat ~ A + VisibilityMPH + Wind_SpeedMPH + PrecipitationIn+ConditionsClear+ConditionsMostly.Cloudy+ConditionsFog+ConditionsHaze+ConditionsHeavy.Rain+ConditionsHeavy.Thunderstorms.and.Rain+ConditionsLight.Drizzle+ConditionsLight.Rain+ConditionsLight.Snow+ConditionsLight.Thunderstorms.and.Rain+ConditionsOvercast+ConditionsPartly.Cloudy+ConditionsRain+ConditionsScattered.Clouds+ConditionsThunderstorms.and.Rain,data=twangdf.small.coded,estimand="ATT")
*relative influence plot*
summary(ps.twangdf.small$gbm.obj, n.trees=ps.twangdf.small$desc$ks.mean.ATT$n.trees,plot=TRUE)

#TAF based data

*A random sample of TAF + TFMI data

|hour |cross_JFK |vis_JFK |snow_JFK |TS_JFK |rain_JFK |status_JFK |
|:----|:-----------|:---------|:----------|:--------|:----------|:----------|
|749  |5.209445    |10        |FALSE      |FALSE    |FALSE      |0          |
|2784 |9.918584    |4         |FALSE      |FALSE    |TRUE       |1          |
|2274 |6.8944      |10        |FALSE      |FALSE    |TRUE       |0          |
|3591 |15.58846    |10        |FALSE      |FALSE    |FALSE      |0          |
|2644 |5.209445    |10        |FALSE      |FALSE    |FALSE      |0          |
|2603 |9.526279    |10        |FALSE      |FALSE    |FALSE      |0          |


|hour |cross_EWR |vis_EWR |snow_EWR |TS_EWR |rain_EWR |status_EWR |
|:----|:-----------|:---------|:----------|:--------|:----------|:----------|
|749  |28.19078    |10        |FALSE      |FALSE    |FALSE      |0          |
|2784 |23.49232    |2         |FALSE      |FALSE    |TRUE       |1          |
|2274 |5.362311    |10        |FALSE      |FALSE    |TRUE       |1          |
|3591 |6.156363    |10        |FALSE      |FALSE    |FALSE      |1          |
|2644 |27.57462    |10        |FALSE      |FALSE    |FALSE      |1          |
|2603 |11.57018    |10        |FALSE      |FALSE    |FALSE      |1          |

|hour |cross_LGAa |cross_LGAb |vis_LGA |snow_LGA |TS_LGA |rain_LGA |status_LGA |
|:----|:------------|:------------|:---------|:----------|:--------|:----------|:----------|
|749  |32.49866     |5.73039      |10        |FALSE      |FALSE    |FALSE      |0          |
|2784 |12.25671     |10.2846      |6         |FALSE      |FALSE    |FALSE      |1          |
|2274 |6.128356     |5.142301     |10        |FALSE      |FALSE    |FALSE      |1          |
|3591 |3.762222     |19           |10        |FALSE      |FALSE    |FALSE      |0          |
|2644 |24.62019     |12           |10        |FALSE      |FALSE    |FALSE      |0          |
|2603 |5.5          |9.526279     |3         |FALSE      |FALSE    |TRUE       |1          |



