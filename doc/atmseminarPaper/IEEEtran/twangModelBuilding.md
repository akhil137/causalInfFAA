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



#Prop score results
kable(bal.jfk$unw,format="markdown")


|                 |  tx.mn| tx.sd|  ct.mn| ct.sd| std.eff.sz|    stat|     p|    ks| ks.pval|
|:----------------|------:|-----:|------:|-----:|----------:|-------:|-----:|-----:|-------:|
|cross_JFK_1      | 10.635| 6.917|  8.371| 5.726|      0.327|   8.526| 0.000| 0.168|   0.000|
|vis_JFK_1        |  7.728| 3.304|  9.166| 2.257|     -0.435| -11.531| 0.000| 0.218|   0.000|
|snow_JFK_1:FALSE |  0.971| 0.168|  0.973| 0.163|     -0.010|   0.062| 0.803| 0.002|   0.803|
|snow_JFK_1:TRUE  |  0.029| 0.168|  0.027| 0.163|      0.010|      NA|    NA| 0.002|   0.803|
|TS_JFK_1:FALSE   |  1.000| 0.000|  0.999| 0.037|         NA|   1.041| 0.308| 0.001|   0.308|
|TS_JFK_1:TRUE    |  0.000| 0.000|  0.001| 0.037|         NA|      NA|    NA| 0.001|   0.308|
|rain_JFK_1:FALSE |  0.776| 0.417|  0.929| 0.257|     -0.368| 179.503| 0.000| 0.153|   0.000|
|rain_JFK_1:TRUE  |  0.224| 0.417|  0.071| 0.257|      0.368|      NA|    NA| 0.153|   0.000|
|A_JFK            | 35.964| 5.183| 31.861| 9.298|      0.792|  17.475| 0.000| 0.278|   0.000|


kable(bal.jfk$ks.mean.ATT,format="markdown")


|                 |  tx.mn| tx.sd|  ct.mn| ct.sd| std.eff.sz|   stat|     p|    ks| ks.pval|
|:----------------|------:|-----:|------:|-----:|----------:|------:|-----:|-----:|-------:|
|cross_JFK_1      | 10.635| 6.917| 10.343| 6.655|      0.042|  0.849| 0.396| 0.028|   0.820|
|vis_JFK_1        |  7.728| 3.304|  8.084| 3.096|     -0.108| -2.188| 0.029| 0.048|   0.196|
|snow_JFK_1:FALSE |  0.971| 0.168|  0.971| 0.169|      0.001|  0.001| 0.974| 0.000|   0.974|
|snow_JFK_1:TRUE  |  0.029| 0.168|  0.029| 0.169|     -0.001|     NA|    NA| 0.000|   0.974|
|TS_JFK_1:FALSE   |  1.000| 0.000|  0.998| 0.048|         NA|  3.431| 0.064| 0.002|   0.064|
|TS_JFK_1:TRUE    |  0.000| 0.000|  0.002| 0.048|         NA|     NA|    NA| 0.002|   0.064|
|rain_JFK_1:FALSE |  0.776| 0.417|  0.807| 0.394|     -0.076|  2.177| 0.140| 0.032|   0.140|
|rain_JFK_1:TRUE  |  0.224| 0.417|  0.193| 0.394|      0.076|     NA|    NA| 0.032|   0.140|
|A_JFK            | 35.964| 5.183| 35.964| 5.512|      0.000|  0.002| 0.998| 0.028|   0.803|


 kable(bal.jfk$es.mean.ATT,format="markdown")


|                 |  tx.mn| tx.sd|  ct.mn| ct.sd| std.eff.sz|   stat|     p|    ks| ks.pval|
|:----------------|------:|-----:|------:|-----:|----------:|------:|-----:|-----:|-------:|
|cross_JFK_1      | 10.635| 6.917| 10.343| 6.655|      0.042|  0.849| 0.396| 0.028|   0.820|
|vis_JFK_1        |  7.728| 3.304|  8.084| 3.096|     -0.108| -2.188| 0.029| 0.048|   0.196|
|snow_JFK_1:FALSE |  0.971| 0.168|  0.971| 0.169|      0.001|  0.001| 0.974| 0.000|   0.974|
|snow_JFK_1:TRUE  |  0.029| 0.168|  0.029| 0.169|     -0.001|     NA|    NA| 0.000|   0.974|
|TS_JFK_1:FALSE   |  1.000| 0.000|  0.998| 0.048|         NA|  3.431| 0.064| 0.002|   0.064|
|TS_JFK_1:TRUE    |  0.000| 0.000|  0.002| 0.048|         NA|     NA|    NA| 0.002|   0.064|
|rain_JFK_1:FALSE |  0.776| 0.417|  0.807| 0.394|     -0.076|  2.177| 0.140| 0.032|   0.140|
|rain_JFK_1:TRUE  |  0.224| 0.417|  0.193| 0.394|      0.076|     NA|    NA| 0.032|   0.140|
|A_JFK            | 35.964| 5.183| 35.964| 5.512|      0.000|  0.002| 0.998| 0.028|   0.803|

#outcome analysis
