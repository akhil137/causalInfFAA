#NASA causal inf
working dir: /Users/ashah/NoBackup/code/nasa/src/causalInfFAA
git remote: github.com/akhil137/causalInfFAA.git
4.14.16
=======
* Recall comments from taf_extractor.r (used to extract taf features that were block averaged in ken's original feature_select.r) that overlap with TFMI data is: 
TAF date overlap with TFMI data is only for: 9/27/13-12/31/14
and last date of TAF is May 22 2015.  
* extracted TAF features from previous parsed raw files (using ken's parseRawTAF.r)
*TAF data only forecasts for 6 hour blocks (mostly)
load("NY_TAF_data.Rdata")
TAF_data.jfk<-TAF_data[TAF_data$airport=="KJFK",]
td<-TAF_data.jfk$fore_time_end-TAF_data.jfk$fore_time_start
> table(td)
td
    1     2     3     4     5     6 
    4    12    30    22    16 10480 
*however there are multiple forecasts for the same time period (which is identified by the forecast end time which is either 6,12,18,or 24; forecast start time varies however).  However most of the repeat forecasts are the same.  We can check for the number unique forecasts using the following code (substitue the correct forecast end time):

*below we subset for each data/end-time pair and select data columns prior to checking uniqueness*

```
num.unique.forecasts<-c()
for (date_ind in 1:n_dates) {
num.unique.forecasts[date_ind]=dim(unique(subset(TAF_data.jfk,date==rel_dates[date_ind] & fore_time_end==6,select=c(6:13))))[1]
} 
```

For example the forecast end time of 24 gives:
```
table(num.unique.forecasts)
num.unique.forecasts
```
  0   1   2   3 
  3 573  12   1
  
we can then examine these subsets as such:
```subset(TAF_data.jfk,date==rel_dates[which(num.unique.forecasts==3)[1]] & fore_time_end==24)```

*The bottom line is that of the 589 days in the data-set, there are only about 9-12 days where there are 2 (and only one instance of 3) different data forecasts for windspeed winddir windgust visibility  snow    TS  rain   fog*

thus reparsing TAF is not worth it!!!

Nonetheless, in feature_select_TAF_akhil.r we have refactored to create a dataframes for unique TAF reports sent out for each forecast end time (6,12,18,24h) for each day.  These include the crosswind computation that Kenny created.  